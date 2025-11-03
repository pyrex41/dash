import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables
config({ path: resolve(process.cwd(), '../.env'), override: true });

const HUBSPOT_API_KEY = process.env.HUBSPOT_API_KEY;
const HUBSPOT_BASE_URL = 'https://api.hubapi.com';

interface HubSpotContact {
  properties: {
    email: string;
    firstname?: string;
    lastname?: string;
    phone?: string;
    [key: string]: any;
  };
}

interface HubSpotContactResponse {
  id: string;
  properties: Record<string, any>;
  createdAt: string;
  updatedAt: string;
}

interface RateLimiter {
  queue: Array<() => Promise<any>>;
  processing: boolean;
  lastRequestTime: number;
  requestCount: number;
}

export class HubSpotClient {
  private apiKey: string;
  private baseUrl: string;
  private rateLimiter: RateLimiter;
  private readonly maxRequestsPer10Seconds = 10;
  private readonly windowMs = 10000;

  constructor() {
    if (!HUBSPOT_API_KEY) {
      throw new Error('HUBSPOT_API_KEY environment variable is required');
    }
    this.apiKey = HUBSPOT_API_KEY;
    this.baseUrl = HUBSPOT_BASE_URL;
    this.rateLimiter = {
      queue: [],
      processing: false,
      lastRequestTime: 0,
      requestCount: 0,
    };
  }

  private async executeWithRateLimit<T>(fn: () => Promise<T>): Promise<T> {
    return new Promise((resolve, reject) => {
      this.rateLimiter.queue.push(async () => {
        try {
          const result = await fn();
          resolve(result);
        } catch (error) {
          reject(error);
        }
      });

      this.processQueue();
    });
  }

  private async processQueue(): Promise<void> {
    if (this.rateLimiter.processing || this.rateLimiter.queue.length === 0) {
      return;
    }

    this.rateLimiter.processing = true;

    while (this.rateLimiter.queue.length > 0) {
      const now = Date.now();
      const timeSinceLastWindow = now - this.rateLimiter.lastRequestTime;

      // Reset counter if window has passed
      if (timeSinceLastWindow >= this.windowMs) {
        this.rateLimiter.requestCount = 0;
        this.rateLimiter.lastRequestTime = now;
      }

      // Wait if we've hit the rate limit
      if (this.rateLimiter.requestCount >= this.maxRequestsPer10Seconds) {
        const waitTime = this.windowMs - timeSinceLastWindow;
        await new Promise(resolve => setTimeout(resolve, waitTime));
        this.rateLimiter.requestCount = 0;
        this.rateLimiter.lastRequestTime = Date.now();
      }

      const task = this.rateLimiter.queue.shift();
      if (task) {
        await task();
        this.rateLimiter.requestCount++;
      }
    }

    this.rateLimiter.processing = false;
  }

  private async makeRequest<T>(
    endpoint: string,
    method: 'GET' | 'POST' | 'PATCH' | 'DELETE',
    body?: any
  ): Promise<T> {
    return this.executeWithRateLimit(async () => {
      const url = `${this.baseUrl}${endpoint}`;
      const headers: HeadersInit = {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.apiKey}`,
      };

      const options: RequestInit = {
        method,
        headers,
      };

      if (body && (method === 'POST' || method === 'PATCH')) {
        options.body = JSON.stringify(body);
      }

      console.log(`[HubSpot] ${method} ${url}`);

      const response = await fetch(url, options);

      if (!response.ok) {
        const errorText = await response.text();
        throw new Error(
          `HubSpot API error (${response.status}): ${errorText}`
        );
      }

      return response.json() as Promise<T>;
    });
  }

  async searchContactByEmail(email: string): Promise<HubSpotContactResponse | null> {
    try {
      const result = await this.makeRequest<{
        results: HubSpotContactResponse[];
      }>(
        '/crm/v3/objects/contacts/search',
        'POST',
        {
          filterGroups: [
            {
              filters: [
                {
                  propertyName: 'email',
                  operator: 'EQ',
                  value: email,
                },
              ],
            },
          ],
        }
      );

      return result.results?.[0] || null;
    } catch (error) {
      console.error('[HubSpot] Error searching contact by email:', error);
      throw error;
    }
  }

  async createContact(contactData: HubSpotContact): Promise<HubSpotContactResponse> {
    try {
      return await this.makeRequest<HubSpotContactResponse>(
        '/crm/v3/objects/contacts',
        'POST',
        contactData
      );
    } catch (error) {
      console.error('[HubSpot] Error creating contact:', error);
      throw error;
    }
  }

  async updateContact(
    contactId: string,
    properties: Record<string, any>
  ): Promise<HubSpotContactResponse> {
    try {
      return await this.makeRequest<HubSpotContactResponse>(
        `/crm/v3/objects/contacts/${contactId}`,
        'PATCH',
        { properties }
      );
    } catch (error) {
      console.error('[HubSpot] Error updating contact:', error);
      throw error;
    }
  }

  async createOrUpdateContact(
    email: string,
    properties: Record<string, any>
  ): Promise<{ contact: HubSpotContactResponse; created: boolean }> {
    try {
      // Search for existing contact
      const existingContact = await this.searchContactByEmail(email);

      if (existingContact) {
        // Update existing contact
        console.log(`[HubSpot] Updating existing contact: ${existingContact.id}`);
        const updated = await this.updateContact(existingContact.id, properties);
        return { contact: updated, created: false };
      } else {
        // Create new contact
        console.log(`[HubSpot] Creating new contact for: ${email}`);
        const created = await this.createContact({
          properties: { email, ...properties },
        });
        return { contact: created, created: true };
      }
    } catch (error) {
      console.error('[HubSpot] Error in createOrUpdateContact:', error);
      throw error;
    }
  }
}

// Data transformation utilities
export interface BookingData {
  id: string;
  email: string;
  phone?: string;
  data?: Record<string, any>;
  application?: {
    data?: Record<string, any>;
    name?: string;
  };
  user?: {
    email?: string;
  };
}

export function transformBookingToHubSpot(booking: BookingData): Record<string, any> {
  const properties: Record<string, any> = {
    email: booking.email,
  };

  // Fallback hierarchy: booking.data → application.data → booking fields
  const bookingData = booking.data || {};
  const applicationData = booking.application?.data || {};

  // Extract name from various sources
  const firstName =
    bookingData.applicant_info?.f_name ||
    bookingData.applicant_info?.first_name ||
    applicationData.applicant_info?.f_name ||
    applicationData.applicant_info?.first_name;

  const lastName =
    bookingData.applicant_info?.l_name ||
    bookingData.applicant_info?.last_name ||
    applicationData.applicant_info?.l_name ||
    applicationData.applicant_info?.last_name ||
    booking.application?.name;

  if (firstName) properties.firstname = firstName;
  if (lastName) properties.lastname = lastName;

  // Phone number
  const phone =
    booking.phone ||
    bookingData.applicant_info?.phone ||
    applicationData.applicant_info?.phone;

  if (phone) properties.phone = phone;

  // Additional fields from application data
  const dob =
    bookingData.applicant_info?.applicant_dob ||
    applicationData.applicant_info?.applicant_dob;
  if (dob) properties.date_of_birth = dob;

  const zip =
    bookingData.applicant_info?.zip5 ||
    applicationData.applicant_info?.zip5;
  if (zip) properties.zip = zip;

  const city =
    bookingData.applicant_info?.address_city ||
    applicationData.applicant_info?.address_city;
  if (city) properties.city = city;

  const state =
    bookingData.applicant_info?.address_state ||
    applicationData.applicant_info?.address_state;
  if (state) properties.state = state;

  // Medicare information
  const effectiveDate =
    bookingData.medicare_information?.effective_date ||
    bookingData.applicant_info?.effective_date ||
    applicationData.medicare_information?.effective_date ||
    applicationData.applicant_info?.effective_date;
  if (effectiveDate) properties.medicare_effective_date = effectiveDate;

  // Add booking ID as a custom property
  properties.booking_id = booking.id;

  return properties;
}

export function handleHubSpotError(error: any): {
  status: 'failed';
  error: string;
} {
  let errorMessage = 'Unknown HubSpot error';

  if (error instanceof Error) {
    errorMessage = error.message;
  } else if (typeof error === 'string') {
    errorMessage = error;
  }

  console.error('[HubSpot] Error:', errorMessage);

  return {
    status: 'failed',
    error: errorMessage,
  };
}
