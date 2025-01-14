import { createClient } from '@libsql/client';
import { csgTokens, csgTokens2 } from '../db/schema';
import { eq, gt, desc } from 'drizzle-orm';
import { drizzle } from 'drizzle-orm/libsql';
import axios, { AxiosRequestConfig } from 'axios';

const client = createClient({
  url: process.env.TURSO_DATABASE_URL!,
  authToken: process.env.TURSO_AUTH_TOKEN!,
});

const db = drizzle(client);

interface TokenResponse {
  token: string;
}

async function fetchTokenFromCSG(quote: boolean = false): Promise<TokenResponse> {
  const apiKey = process.env.CSG_API_KEY;
  const apiUrl = process.env.CSG_API_URL;

  if (!apiKey || !apiUrl) {
    throw new Error('CSG_API_KEY or CSG_API_URL is not set in environment variables');
  }

  const endpoint = `${apiUrl}/v1/auth.json`;
  console.log('Fetching token from CSG:', endpoint);
  const values: { api_key: string; portal_name?: string } = { 
    api_key: apiKey,
    ...(quote ? {} : { portal_name: 'medicareschool' })
  };
  console.log('CSG values:', values);

  try {
    const response = await axios.post<TokenResponse>(endpoint, values);
    if (!response.data.token) {
      throw new Error('Token not received in CSG response');
    }
    return { token: response.data.token };
  } catch (error: any) {
    console.error('Error fetching token from CSG:', error);
    if (error.response) {
      console.error('Response status:', error.response.status);
      console.error('Response data:', error.response.data);
      console.error('Response headers:', error.response.headers);
    } else if (error.request) {
      console.error('No response received. Request details:', error.request);
    } else {
      console.error('Error details:', error.message);
    }
    console.error('Error config:', error.config);
    throw new Error('Failed to fetch token from CSG');
  }
}

async function clearInvalidToken(tokenTable: typeof csgTokens | typeof csgTokens2, token: string) {
  const now = new Date();
  try {
    await db.delete(tokenTable)
      .where(eq(tokenTable.token, token));
    console.log('Cleared invalid token from database');
  } catch (error) {
    console.error('Error clearing invalid token:', error);
  }
}

async function getTokenWithRetry(tokenTable: typeof csgTokens | typeof csgTokens2, quote: boolean = false): Promise<string> {
  const now = new Date();

  try {
    // Check for valid token in database
    const [existingToken] = await db
      .select()
      .from(tokenTable)
      .where(gt(tokenTable.expiresAt, now))
      .orderBy(desc(tokenTable.expiresAt))
      .limit(1);

    if (existingToken) {
      console.log('Found valid CSG token:', existingToken.token);
      console.log('Expires at:', existingToken.expiresAt);
      return existingToken.token;
    }

    console.log('No valid CSG token found, fetching new one...');
    // Get new token from CSG
    const { token } = await fetchTokenFromCSG(quote);

    // Calculate expiration time (7 hours and 59 minutes from now)
    const expiresAt = new Date(now.getTime() + 7 * 60 * 60 * 1000 + 59 * 60 * 1000);

    // Store new token
    await db.insert(tokenTable).values({
      token,
      expiresAt,
      createdAt: now,
      updatedAt: now,
    });

    console.log('New CSG token fetched and saved');
    return token;
  } catch (error) {
    console.error('Error in token management:', error);
    throw new Error('Failed to manage CSG token');
  }
}

export async function getToken(): Promise<string> {
  return getTokenWithRetry(csgTokens);
}

export async function getQuoteToken(): Promise<string> {
  return getTokenWithRetry(csgTokens2, true);
}

export async function handleTokenError(error: any, currentToken: string, isQuoteToken: boolean = false): Promise<string> {
  if (error.response?.status === 403) {
    console.log('Token was invalidated, clearing and fetching new token...');
    const tokenTable = isQuoteToken ? csgTokens2 : csgTokens;
    await clearInvalidToken(tokenTable, currentToken);
    return isQuoteToken ? getQuoteToken() : getToken();
  }
  throw error;
}

export async function makeCSGRequest<T>(config: AxiosRequestConfig, isQuoteToken: boolean = false): Promise<T> {
  const token = await (isQuoteToken ? getQuoteToken() : getToken());
  const csgApiUrl = process.env.CSG_API_URL || 'https://api.csgactuarial.com';
  
  try {
    const response = await axios({
      ...config,
      url: `${csgApiUrl}${config.url}`,
      headers: {
        ...config.headers,
        'x-api-token': token,
        'Content-Type': 'application/json'
      }
    });
    return response.data;
  } catch (error: any) {
    if (error.response?.status === 403) {
      // Token is invalid, get a new one and retry
      const newToken = await handleTokenError(error, token, isQuoteToken);
      const response = await axios({
        ...config,
        url: `${csgApiUrl}${config.url}`,
        headers: {
          ...config.headers,
          'x-api-token': newToken,
          'Content-Type': 'application/json'
        }
      });
      return response.data;
    }
    throw error;
  }
} 