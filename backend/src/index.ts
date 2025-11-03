import { Elysia } from 'elysia'
import { cors } from '@elysiajs/cors'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'
import staticPlugin from '@elysiajs/static'
import { config } from 'dotenv'
import { resolve } from 'path'

// Load environment variables from .env file (go up one directory from backend/)
const envPath = resolve(process.cwd(), '../.env')
console.log('Loading .env from:', envPath)
const result = config({ path: envPath })
if (result.error) {
  console.error('Error loading .env file:', result.error)
} else {
  console.log('Successfully loaded .env file')
}

// Log environment variables to verify they're loaded
console.log('\n=== Environment Variables Check ===')
console.log('TURSO_DATABASE_URL:', process.env.TURSO_DATABASE_URL ? 'Set' : 'NOT SET')
console.log('TURSO_AUTH_TOKEN:', process.env.TURSO_AUTH_TOKEN ? `Set (${process.env.TURSO_AUTH_TOKEN.substring(0, 20)}...)` : 'NOT SET')
console.log('CSG_API_URL:', process.env.CSG_API_URL || 'NOT SET')
console.log('CSG_API_KEY:', process.env.CSG_API_KEY ? 'Set' : 'NOT SET')
console.log('NODE_ENV:', process.env.NODE_ENV || 'NOT SET')
console.log('LAPRO_USERNAME:', process.env.LAPRO_USERNAME || 'NOT SET')
console.log('===================================\n')

import { getApplications, exportApplications, getApplicationWithSchema, updateFormattedData, getProducerConfig, determineStatus, getApplicationStats, getFormattedApplicationWithSchema, createBooking, getBookings, getBookingWithContext } from './db/query'
import { HubSpotClient, transformBookingToHubSpot, handleHubSpotError } from './integrations/hubspot'
import { format_application, getCarrierName } from './formatter'
import { submitToCSG } from './csg/submit'
import { makeCSGRequest } from './csg/token'
import { verifyCSGApplication } from './csg/verify'
import axios from 'axios'
import { getHeaders } from './csg/submit'
import { eq } from 'drizzle-orm'
import { getDb } from './db'
import { csgApplications, applications, bookings } from './db/schema'
import type { ServerWebSocket } from 'bun';

// Resolve __dirname for ESM environments
const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

// Detect environment
const isDev = process.env.NODE_ENV === 'development' || !process.env.NODE_ENV

// WebSocket clients store with subscriptions
interface WSData {
  subscriptions: Set<string>;
  heartbeatInterval?: ReturnType<typeof setInterval>;
  ws: any; // Store WebSocket instance reference
}

const wsClients = new Map<string, WSData>();

// Session management
interface Session {
  username: string;
  expiresAt: number;
}

const sessions = new Map<string, Session>();

// Generate random session token
function generateSessionToken(): string {
  return Math.random().toString(36).substring(2) + Date.now().toString(36);
}

// Clean up expired sessions periodically
setInterval(() => {
  const now = Date.now();
  for (const [token, session] of sessions.entries()) {
    if (session.expiresAt < now) {
      sessions.delete(token);
    }
  }
}, 60 * 60 * 1000); // Clean up every hour

// Add type definition for application response
interface ApplicationResponse {
  id: any;
  naic: any;
  status: string;
  phone: any;
  email: any;
  effectiveDate: any;
  dateStarted: string;
  csgApplication?: {
    verificationStatus?: string;
  };
}

// Helper function to broadcast verification updates only to subscribed clients
export async function broadcastVerificationUpdate(applicationId: string, body: any) {
  const msgId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const timestamp = new Date().toISOString();
  
  console.log(`[${timestamp}] Broadcasting verification update (msgId: ${msgId}) for application:`, applicationId);
  //console.log('Update body:', body);

  // Get the current application state from the database
  const db = getDb();
  const [application] = await db
    .select()
    .from(applications)
    .leftJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
    .leftJoin(bookings, eq(applications.id, bookings.applicationId))
    .where(eq(applications.id, applicationId));

  if (!application) {
    console.error(`[${timestamp}] Application not found for verification update:`, applicationId);
    return;
  }

  const csgApp = application.csg_applications ? { verificationStatus: application.csg_applications.verificationStatus } : undefined;

  // Use the same determineStatus function as the applications query
  const status = determineStatus(
    application.applications.status,
    !!application.csg_applications,
    !!application.bookings,
    csgApp
  );
  
  console.log(`[${timestamp}] Determined status:`, status);
  //console.log(`[${timestamp}] Application data:`, application);
  
  const message = JSON.stringify({
    type: 'verification_update',
    msgId,
    timestamp,
    applicationId,
    body: {
      status,
      csg_id: application.csg_applications?.key || body.key,
      applicationStatus: status,
      error: application.csg_applications?.verificationError || body.error,
      screenshot: application.csg_applications?.verificationScreenshot || body.screenshot,
      verifyUrl: body.verifyUrl,
      signatureUrl: body.signatureUrl,
      message: body.message
    }
  });
  
  //console.log(`[${timestamp}] Message to be sent:`, message);
  console.log(`[${timestamp}] Total connected clients:`, wsClients.size);
  
  // Only send to clients subscribed to this application
  wsClients.forEach((data, clientId) => {
    if (data.subscriptions.has(applicationId)) {
      try {
        console.log(`[${timestamp}] Sending update (msgId: ${msgId}) to client ${clientId}`);
        data.ws.send(message);
      } catch (error) {
        console.error(`[${timestamp}] Error sending WebSocket message (msgId: ${msgId}) to client ${clientId}:`, error);
      }
    }
  });
}

// Add type definition for CSG application response
interface CSGApplicationResponse {
  key: string
  status: string
  // Add other fields as needed
}

const app = new Elysia({
  websocket: {
    idleTimeout: 30,
    // Add support for secure WebSocket
    perMessageDeflate: true
  },
  serve: {
    idleTimeout: 120
  }
})
.use(cors({
  // Add WebSocket headers to CORS
  credentials: true,
  allowedHeaders: ['content-type', 'upgrade', 'connection'],
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
  origin: '*'
}))
.ws('/ws', {
  open(ws) {
    console.log('WebSocket client connected')
    // Initialize client with empty subscriptions
    const wsData: WSData = {
      subscriptions: new Set(),
      ws
    }
    
    // Start heartbeat for this connection
    wsData.heartbeatInterval = setInterval(() => {
      try {
        ws.send(JSON.stringify({ type: 'heartbeat' }))
      } catch (error) {
        console.error('Heartbeat failed, cleaning up:', error)
        const heartbeat = wsData.heartbeatInterval
        if (heartbeat) clearInterval(heartbeat)
        wsClients.delete(ws.id)
      }
    }, 20000)
    
    wsClients.set(ws.id, wsData)
  },
  
  message: async (ws, message) => {
    try {
      const data = typeof message === 'string' ? JSON.parse(message) : message;
      const wsData = wsClients.get(ws.id);
      
      if (!wsData) {
        console.error('No WebSocket data found for client:', ws.id);
        return;
      }
      
      if (data.type === 'ping') {
        ws.send(JSON.stringify({ type: 'pong' }));
        return;
      }
      
      if (data.type === 'pong') {
        return;
      }
      
      // Handle subscription messages
      if (data.type === 'subscribe') {
        const applicationIds = data.applicationIds as string[]
        console.log(`[${new Date().toISOString()}] Received subscription request for:`, applicationIds);
        
        if (Array.isArray(applicationIds)) {
          // Filter out already subscribed IDs
          const newSubscriptions = applicationIds.filter(id => !wsData.subscriptions.has(id));
          
          console.log(`[${new Date().toISOString()}] New subscriptions to add:`, newSubscriptions);
          console.log(`[${new Date().toISOString()}] Current subscriptions:`, Array.from(wsData.subscriptions));
          
          // Only process if there are new subscriptions
          if (newSubscriptions.length > 0) {
            // Add new subscriptions
            newSubscriptions.forEach(id => wsData.subscriptions.add(id));
            
            // Send confirmation only for new subscriptions
            ws.send(JSON.stringify({
              type: 'subscribed',
              applicationIds: newSubscriptions
            }));
            
            console.log(`[${new Date().toISOString()}] Updated subscriptions for client:`, Array.from(wsData.subscriptions));
          } else {
            console.log(`[${new Date().toISOString()}] No new subscriptions to add`);
          }
        } else {
          console.error(`[${new Date().toISOString()}] Invalid applicationIds format:`, applicationIds);
        }
      }
      
      // Handle unsubscribe messages
      if (data.type === 'unsubscribe') {
        const applicationIds = data.applicationIds as string[]
        if (Array.isArray(applicationIds)) {
          applicationIds.forEach(id => wsData.subscriptions.delete(id))
          
          // Send confirmation
          ws.send(JSON.stringify({
            type: 'unsubscribed',
            applicationIds
          }))
          
          console.log('Client unsubscribed from applications:', applicationIds)
          console.log('Remaining subscriptions:', Array.from(wsData.subscriptions))
        }
      }

      // Handle application requests
      if (data.type === 'request_application') {
        const applicationId = data.applicationId as string
        if (applicationId) {
          getFormattedApplicationWithSchema(applicationId).then(application => {
            //console.log('**12** Application data formatted:', application?.formattedData);
            //console.log('**13** Application csgApplication:', application?.csgApplication);
            if (application) {
              ws.send(JSON.stringify({
                type: 'application_data',
                applicationId,
                application
              }))
            } else {
              ws.send(JSON.stringify({
                type: 'application_error',
                applicationId,
                error: 'Application not found'
              }))
            }
          }).catch(error => {
            ws.send(JSON.stringify({
              type: 'application_error',
              applicationId,
              error: error instanceof Error ? error.message : 'Failed to load application'
            }))
          })
        }
      }

      // Handle applications list requests
      if (data.type === 'request_applications') {
        const { page = 0, pageSize = 20, searchTerm = '', hasContactFilter = false, naics = [], status = undefined } = data
        console.log('Fetching applications with params:', { page, pageSize, searchTerm, hasContactFilter, naics, status })
        
        // Get client data
        const wsData = wsClients.get(ws.id);
        if (!wsData) {
          console.error('No WebSocket data found for client:', ws.id);
          return;
        }

        // Clear existing subscriptions
        wsData.subscriptions.clear();
        
        getApplications(page, pageSize, searchTerm, hasContactFilter, naics, status).then(result => {
          // Extract application IDs and add to subscriptions
          const applicationIds = result.applications.map(app => app.id);
          applicationIds.forEach(id => wsData.subscriptions.add(id));
          
          // Send response with applications data and subscription confirmation
          const response = {
            type: 'applications_data',
            applications: result.applications,
            pagination: result.pagination,
            subscribed: applicationIds
          }
          console.log('Sending applications response with subscriptions:', applicationIds.length);
          ws.send(JSON.stringify(response));
        }).catch(error => {
          const errorResponse = {
            type: 'applications_error',
            error: error instanceof Error ? error.message : 'Failed to load applications'
          }
          console.log('Sending applications error:', errorResponse)
          ws.send(JSON.stringify(errorResponse));
        });
      }

      // Handle application stats requests
      if (data.type === 'request_application_stats') {
        console.log('Fetching application stats');
        getApplicationStats().then(stats => {
          const response = {
            type: 'application_stats',
            stats
          }
          console.log('Sending application stats response:');
          ws.send(JSON.stringify(response));
        }).catch(error => {
          const errorResponse = {
            type: 'application_stats_error',
            error: error instanceof Error ? error.message : 'Failed to load application stats'
          }
          console.log('Sending application stats error:', errorResponse);
          ws.send(JSON.stringify(errorResponse));
        });
      }

      // Handle save application requests
      if (data.type === 'save_application') {
        const { id, formData, medications } = data
        updateFormattedData(id, formData, medications).then(() => {
          ws.send(JSON.stringify({
            type: 'save_application_response',
            id,
            success: true,
            error: null
          }))
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'save_application_response',
            id,
            success: false,
            error: error instanceof Error ? error.message : 'Failed to save application'
          }))
        })
      }

      // Handle bookings list requests
      if (data.type === 'request_bookings') {
        const { page = 0, pageSize = 20, searchTerm = '', statusFilter } = data
        console.log('Fetching bookings with params:', { page, pageSize, searchTerm, statusFilter })

        getBookings(page, pageSize, searchTerm, statusFilter).then(result => {
          const response = {
            type: 'bookings_data',
            bookings: result.bookings,
            totalCount: result.totalCount,
            page: result.page,
            pageSize: result.pageSize,
            totalPages: result.totalPages
          }
          console.log('Sending bookings response:', result.bookings.length, 'bookings');
          ws.send(JSON.stringify(response));
        }).catch(error => {
          const errorResponse = {
            type: 'bookings_error',
            error: error instanceof Error ? error.message : 'Failed to load bookings'
          }
          console.log('Sending bookings error:', errorResponse)
          ws.send(JSON.stringify(errorResponse));
        });
      }

      // Handle single booking request
      if (data.type === 'request_booking') {
        const { bookingId } = data
        console.log('Fetching booking:', bookingId)

        getBookingWithContext(bookingId).then(booking => {
          if (booking) {
            ws.send(JSON.stringify({
              type: 'booking_data',
              booking
            }));
          } else {
            ws.send(JSON.stringify({
              type: 'booking_error',
              error: 'Booking not found'
            }));
          }
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'booking_error',
            error: error instanceof Error ? error.message : 'Failed to load booking'
          }));
        });
      }

      // Handle sync booking to HubSpot
      if (data.type === 'sync_booking_to_hubspot') {
        const { bookingId } = data
        console.log('Syncing booking to HubSpot:', bookingId)

        const hubspotClient = new HubSpotClient();

        getBookingWithContext(bookingId).then(async (booking) => {
          if (!booking) {
            throw new Error('Booking not found');
          }

          try {
            // Update sync status to syncing
            const db = getDb();
            await db.update(bookings)
              .set({
                hubspotSyncStatus: 'syncing',
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            // Transform booking data to HubSpot format
            const hubspotProperties = transformBookingToHubSpot(booking);

            // Create or update contact in HubSpot
            const result = await hubspotClient.createOrUpdateContact(
              booking.email,
              hubspotProperties
            );

            console.log('[HubSpot] Sync successful:', result.contact.id);

            // Update booking with HubSpot contact ID and sync status
            await db.update(bookings)
              .set({
                hubspotContactId: result.contact.id,
                hubspotSyncStatus: 'synced',
                hubspotLastSyncedAt: Math.floor(Date.now() / 1000),
                hubspotSyncError: null,
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            ws.send(JSON.stringify({
              type: 'sync_booking_to_hubspot_response',
              success: true,
              bookingId,
              hubspotContactId: result.contact.id,
              created: result.created
            }));
          } catch (error) {
            console.error('[HubSpot] Sync failed:', error);

            const errorResult = handleHubSpotError(error);

            // Update booking with error status
            const db = getDb();
            await db.update(bookings)
              .set({
                hubspotSyncStatus: 'failed',
                hubspotSyncError: errorResult.error,
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            ws.send(JSON.stringify({
              type: 'sync_booking_to_hubspot_response',
              success: false,
              bookingId,
              error: errorResult.error
            }));
          }
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'sync_booking_to_hubspot_response',
            success: false,
            bookingId,
            error: error instanceof Error ? error.message : 'Failed to sync booking'
          }));
        });
      }

      // Handle bulk sync bookings to HubSpot
      if (data.type === 'bulk_sync_bookings') {
        const { bookingIds } = data
        console.log('Bulk syncing bookings to HubSpot:', bookingIds.length, 'bookings')

        const hubspotClient = new HubSpotClient();
        const results: Array<{ bookingId: string; success: boolean; error?: string; hubspotContactId?: string }> = [];

        for (const bookingId of bookingIds) {
          try {
            const booking = await getBookingWithContext(bookingId);
            if (!booking) {
              results.push({ bookingId, success: false, error: 'Booking not found' });
              continue;
            }

            // Update sync status to syncing
            const db = getDb();
            await db.update(bookings)
              .set({
                hubspotSyncStatus: 'syncing',
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            // Transform and sync
            const hubspotProperties = transformBookingToHubSpot(booking);
            const result = await hubspotClient.createOrUpdateContact(
              booking.email,
              hubspotProperties
            );

            // Update booking with success
            await db.update(bookings)
              .set({
                hubspotContactId: result.contact.id,
                hubspotSyncStatus: 'synced',
                hubspotLastSyncedAt: Math.floor(Date.now() / 1000),
                hubspotSyncError: null,
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            results.push({
              bookingId,
              success: true,
              hubspotContactId: result.contact.id
            });
          } catch (error) {
            const errorResult = handleHubSpotError(error);

            // Update booking with error
            const db = getDb();
            await db.update(bookings)
              .set({
                hubspotSyncStatus: 'failed',
                hubspotSyncError: errorResult.error,
                updatedAt: new Date().toISOString()
              })
              .where(eq(bookings.id, bookingId));

            results.push({
              bookingId,
              success: false,
              error: errorResult.error
            });
          }
        }

        const successCount = results.filter(r => r.success).length;
        console.log('[HubSpot] Bulk sync completed:', successCount, '/', bookingIds.length, 'successful');

        ws.send(JSON.stringify({
          type: 'bulk_sync_bookings_response',
          results,
          totalCount: bookingIds.length,
          successCount
        }));
      }

      // Handle CSG submission requests
      if (data.type === 'submit_to_csg') {
        const { applicationId, producerId } = data
        submitToCSG(applicationId, producerId).then(result => {
          // Broadcast submission message
          broadcastVerificationUpdate(applicationId, {
            status: 'submitting',
            message: `Submitting application to ${process.env.CSG_API_URL}/v1/e_app/enrollment_applications.json`
          });

          ws.send(JSON.stringify({
            type: 'submit_to_csg_response',
            success: result.success || false,
            error: result.error || null,
            existingSubmission: result.existingSubmission || null,
            key: result.key || null,
            verificationStatus: result.verificationStatus || null
          }))

          // Start verification process if submission was successful
          if (result.key) {
            console.log('Starting verification process for key:', result.key);
            verifyCSGApplication(result.key, {
              headless: true,
              debug: isDev
            }).catch(error => {
              console.error('Error during verification:', error);
            });
          }
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'submit_to_csg_response',
            success: false,
            error: error instanceof Error ? error.message : 'Failed to submit to CSG',
            existingSubmission: null,
            key: null,
            verificationStatus: null
          }))
        })
      }

      // Handle CSG verification requests
      if (data.type === 'verify_csg_application') {
        const { key } = data
        verifyCSGApplication(key).then(result => {
          // Broadcast quote response status if successful
          if (result.success) {
            broadcastVerificationUpdate(key, {
              status: 'quote_response',
              message: 'Quote response status 200'
            });
          }

          ws.send(JSON.stringify({
            type: 'verify_csg_application_response',
            success: true,
            result
          }))
        }).catch(error => {
          const errorMessage = error instanceof Error ? error.message : 'Failed to verify CSG application';
          
          // Send verification update about failure
          broadcastVerificationUpdate(key, {
            status: 'failed',
            message: errorMessage
          });

          ws.send(JSON.stringify({
            type: 'verify_csg_application_response', 
            success: false,
            error: errorMessage
          }))
        })
      }

      // Handle LAPro token refresh requests
      if (data.type === 'refresh_lapro_token') {
        try {
          const body = {
            username: process.env.LAPRO_USERNAME,
            password: process.env.LAPRO_PASSWORD,
            grant_type: process.env.LAPRO_GRANT_TYPE || 'password',
            client_id: process.env.LAPRO_CLIENT_ID,
            client_secret: process.env.LAPRO_CLIENT_SECRET,
          };

          console.log('Attempting to get LAPRO token with credentials:', {
            username: process.env.LAPRO_USERNAME,
            client_id: process.env.LAPRO_CLIENT_ID
          });

          const response = await fetch('https://authorize.leadadvantagepro.com/access_token', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json'
            },
            body: JSON.stringify(body)
          });

          if (!response.ok) {
            const errorText = await response.text();
            console.error('LAPRO API error:', {
              status: response.status,
              statusText: response.statusText,
              body: errorText
            });
            throw new Error(`HTTP error! status: ${response.status}`);
          }

          const auth = await response.json();
          console.log('Successfully refreshed LAPro token');
          ws.send(JSON.stringify({
            type: 'refresh_lapro_token_response',
            success: true,
            token: auth.access_token,
            error: null
          }));
        } catch (error) {
          console.error('Failed to refresh LAPro token:', error);
          ws.send(JSON.stringify({
            type: 'refresh_lapro_token_response',
            success: false,
            token: null,
            error: error instanceof Error ? error.message : 'Failed to refresh LAPro token'
          }));
        }
      }
      
    } catch (error) {
      console.error('Error handling message:', error)
    }
  },
  
  close(ws) {
    console.log('WebSocket client disconnected')
    const wsData = wsClients.get(ws.id)
    if (wsData?.heartbeatInterval) {
      clearInterval(wsData.heartbeatInterval)
    }
    wsClients.delete(ws.id)
  }
})
.group('/api', app => app
  .post('/login', async ({ body }) => {
    try {
      const { username, password } = body as { username: string; password: string }

      const expectedUsername = process.env.LOGIN
      const expectedPassword = process.env.PASSWORD

      console.log('Login attempt:', {
        receivedUsername: username,
        expectedUsername,
        usernameMatch: username === expectedUsername,
        passwordMatch: password === expectedPassword
      })

      if (username === expectedUsername && password === expectedPassword) {
        // Generate session token
        const sessionToken = generateSessionToken()

        // Session expires in 4 hours
        const expiresAt = Date.now() + (4 * 60 * 60 * 1000)

        // Store session
        sessions.set(sessionToken, {
          username,
          expiresAt
        })

        console.log('Session created:', { token: sessionToken, expiresAt: new Date(expiresAt).toISOString() })

        // Set cookie (expires in 4 hours)
        const cookieExpiry = new Date(expiresAt).toUTCString()

        return new Response(
          JSON.stringify({ success: true }),
          {
            status: 200,
            headers: {
              'Content-Type': 'application/json',
              'Set-Cookie': `session=${sessionToken}; HttpOnly; Path=/; Max-Age=${4 * 60 * 60}; SameSite=Strict${isDev ? '' : '; Secure'}`
            }
          }
        )
      } else {
        return new Response(
          JSON.stringify({ success: false, error: 'Invalid username or password' }),
          {
            status: 401,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      }
    } catch (error) {
      console.error('Login error:', error)
      return new Response(
        JSON.stringify({ success: false, error: 'Server error' }),
        {
          status: 500,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    }
  })
  .get('/session', async ({ request }) => {
    try {
      // Parse cookies from request
      const cookieHeader = request.headers.get('cookie')
      if (!cookieHeader) {
        return new Response(
          JSON.stringify({ authenticated: false }),
          {
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      }

      // Extract session token from cookies
      const cookies = cookieHeader.split(';').reduce((acc, cookie) => {
        const [key, value] = cookie.trim().split('=')
        acc[key] = value
        return acc
      }, {} as Record<string, string>)

      const sessionToken = cookies['session']
      if (!sessionToken) {
        return new Response(
          JSON.stringify({ authenticated: false }),
          {
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      }

      // Check if session exists and is valid
      const session = sessions.get(sessionToken)
      if (!session) {
        return new Response(
          JSON.stringify({ authenticated: false }),
          {
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      }

      // Check if session has expired
      if (session.expiresAt < Date.now()) {
        sessions.delete(sessionToken)
        return new Response(
          JSON.stringify({ authenticated: false }),
          {
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      }

      console.log('Session validated:', { username: session.username })

      return new Response(
        JSON.stringify({ authenticated: true, username: session.username }),
        {
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    } catch (error) {
      console.error('Session validation error:', error)
      return new Response(
        JSON.stringify({ authenticated: false }),
        {
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    }
  })
  .get('/applications', async ({ query: params }) => {
    try {
      const page = Number(params?.page) || 0
      const pageSize = Number(params?.pageSize) || 20
      const searchTerm = params?.searchTerm as string || ''
      const hasContactFilter = params?.hasContactFilter === 'true'
      const naics = Array.isArray(params?.naics) ? params.naics : params?.naics ? [params.naics] : []
      
      console.log('GET /applications query params:', {
        page,
        pageSize,
        searchTerm,
        searchTermLength: searchTerm?.length || 0,
        hasContactFilter,
        naics,
        rawParams: params
      })
      
      const result = await getApplications(page, pageSize, searchTerm, hasContactFilter, naics)

      // Log verification statuses for debugging
      console.log('Verification statuses:', (result.applications as ApplicationResponse[]).map(app => ({
        id: app.id,
        verificationStatus: app.csgApplication?.verificationStatus
      })))
      
      console.log('GET /applications response:', {
        total: result.pagination.total,
        totalPages: result.pagination.totalPages,
        applicationCount: result.applications.length,
        pendingVerifications: (result.applications as ApplicationResponse[]).filter(
          app => app.csgApplication?.verificationStatus === 'pending'
        ).length
      })
      
      return result
    } catch (error) {
      console.error('Error fetching applications:', error)
      return {
        applications: [],
        pagination: {
          total: 0,
          page: 0,
          pageSize: 20,
          totalPages: 0
        }
      }
    }
  })
  .get('/applications/export', async ({ query: params }) => {
    try {
      const searchTerm = params?.searchTerm as string || ''
      const hasContactFilter = params?.hasContactFilter === 'true'
      
      console.log('GET /applications/export query params:', {
        searchTerm,
        hasContactFilter
      })
      
      const result = await exportApplications(searchTerm, hasContactFilter)
      console.log('GET /applications/export response:', {
        exportedCount: result.length
      })
      
      return result
    } catch (error) {
      console.error('Error exporting applications:', error)
      return []
    }
  })
  .get('/applications/:id', async (req) => {
    const { id } = req.params
    const application = await getApplicationWithSchema(id)
    if (!application) {
      return new Response(
        JSON.stringify({ error: 'Application not found' }),
        { status: 404 }
      )
    }
    console.log('Sending application data:', {
      id: application.id,
    })
    return new Response(
      JSON.stringify(application),
      { status: 200 }
    )
  })
  .get('/csg-applications', async ({ query }) => {
    try {
      console.log('GET /api/csg-applications - Starting request...')
      const limit = query?.limit || '10'
      
      const data = await makeCSGRequest({
        method: 'GET',
        url: `/v1/e_app/enrollment_applications.json`,
        params: { limit }
      })
      
      console.log('Successfully fetched CSG applications:', {
        count: Array.isArray(data) ? data.length : 'N/A',
        isArray: Array.isArray(data),
        firstItem: Array.isArray(data) && data.length > 0 ? data[0] : null
      })
      
      return new Response(
        JSON.stringify(data), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    } catch (error) {
      console.error('Error fetching CSG applications:', error)
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fetch CSG applications',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      )
    }
  })
  .put('/applications/:id/formatted', async ({ params, body }) => {
    try {
      const { id } = params
      const { data, rawMedications } = body as { data: Record<string, any>, rawMedications: any[] }
      
      console.log('PUT /applications/:id/formatted:', {
        id,
        dataKeys: Object.keys(data),
        rawMedicationsCount: rawMedications?.length
      })
      
      await updateFormattedData(id, data, rawMedications)
      
      // Reset verification status for any associated CSG application
      const db = getDb()
      await db.update(csgApplications)
        .set({
          verificationStatus: 'pending',
          verificationScreenshot: null,
          verificationError: null,
          lastVerifiedAt: null,
          updatedAt: new Date()
        })
        .where(eq(csgApplications.applicationId, id))
      
      return {
        success: true,
        error: null
      }
    } catch (error) {
      console.error('Error updating formatted data:', error)
      return {
        success: false,
        error: 'Failed to update application data'
      }
    }
  })
  .post('/lapro/token', async () => {
    try {
      const body = {
        username: process.env.LAPRO_USERNAME,
        password: process.env.LAPRO_PASSWORD,
        grant_type: process.env.LAPRO_GRANT_TYPE || 'password',
        client_id: process.env.LAPRO_CLIENT_ID,
        client_secret: process.env.LAPRO_CLIENT_SECRET,
      }

      console.log('Attempting to get LAPRO token with credentials:', {
        username: process.env.LAPRO_USERNAME,
        client_id: process.env.LAPRO_CLIENT_ID
      })

      const response = await fetch('https://authorize.leadadvantagepro.com/access_token', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(body)
      })

      if (!response.ok) {
        const errorText = await response.text()
        console.error('LAPRO API error:', {
          status: response.status,
          statusText: response.statusText,
          body: errorText
        })
        throw new Error(`HTTP error! status: ${response.status}`)
      }

      const auth = await response.json()
      return { access_token: auth.access_token }
    } catch (error) {
      console.error('Error getting LAPRO token:', error)
      return new Response(
        JSON.stringify({ error: 'Failed to get token' }), 
        { status: 500, headers: { 'Content-Type': 'application/json' } }
      )
    }
  })
  .post('/applications/:id/submit', async ({ params, body }) => {
    try {
      const { producerId, forceResubmit } = body as { producerId: number, forceResubmit?: boolean }
      if (!producerId) {
        return {
          success: false,
          error: 'Producer ID is required'
        }
      }

      const db = getDb()
      const [existingCsg] = await db
        .select()
        .from(csgApplications)
        .where(eq(csgApplications.applicationId, params.id))

      // If there's an existing submission and we're not forcing resubmit, return error
      if (existingCsg && !forceResubmit) {
        return {
          success: false,
          error: 'Application already submitted to CSG',
          existingSubmission: true,
          key: existingCsg.key,
          verificationStatus: existingCsg.verificationStatus
        }
      }

      // Update status to submitting in database first
      await db.update(applications)
        .set({
          status: 'submitting',
          updatedAt: new Date()
        })
        .where(eq(applications.id, params.id));

      // Then broadcast the update
      await broadcastVerificationUpdate(params.id, { status: 'submitting' });

      const result = await submitToCSG(params.id, producerId);

      // Handle both direct submission and recovered application cases
      const csgKey = result.key;
      if (!csgKey) {
        // Update database first
        await Promise.all([
          db.update(applications)
            .set({
              status: 'submission_issue',
              updatedAt: new Date()
            })
            .where(eq(applications.id, params.id)),
          db.update(csgApplications)
            .set({
              verificationStatus: 'failed',
              updatedAt: new Date()
            })
            .where(eq(csgApplications.applicationId, params.id))
        ]);

        // Then broadcast the update
        await broadcastVerificationUpdate(params.id, { status: 'failed' });
        return { 
          success: false,
          error: 'Failed to get CSG application key'
        };
      }

      // Update database to verifying state
      await Promise.all([
        db.update(applications)
          .set({
            status: 'verifying',
            updatedAt: new Date()
          })
          .where(eq(applications.id, params.id)),
        db.update(csgApplications)
          .set({
            verificationStatus: 'verifying',
            updatedAt: new Date()
          })
          .where(eq(csgApplications.applicationId, params.id))
      ]);

      // Then broadcast the verification update
      await broadcastVerificationUpdate(params.id, { 
        status: 'verifying', 
        key: csgKey,
        verifyUrl: `${process.env.CSG_API_URL}/v1/e_app/enrollment_applications/${csgKey}/verify`
      });

      // Start verification process
      const { verifyCSGApplication } = await import('./csg/verify');
      verifyCSGApplication(csgKey, {
        headless: true,
        debug: isDev
      });

      return { 
        success: true,
        key: csgKey,
        verificationStatus: 'verifying'
      };
    } catch (error: any) {
      console.error('Error submitting to CSG:', error);
      return {
        success: false,
        error: error.message || 'Failed to submit application'
      };
    }
  })
  .get('/csg-application/:key', async ({ params }) => {
    try {
      const key = params.key.trim()
      
      const data = await makeCSGRequest({
        method: 'GET',
        url: `/v1/e_app/enrollment_applications/${key}.json`
      }) as CSGApplicationResponse

      // Get the application ID from the database
      const db = getDb()
      const [csgApp] = await db
        .select()
        .from(csgApplications)
        .where(eq(csgApplications.key, key))

      if (csgApp) {
        // Check CSG application status
        const csgStatus = data.status
        let newStatus = csgApp.verificationStatus

        // Update application status based on CSG status
        if (csgStatus === 'approved') {
          newStatus = 'completed'
        } else if (csgStatus === 'declined') {
          newStatus = 'declined'
        }
        // For now, we don't change the status for other CSG statuses
        // This is where we'll add more status mappings in the future

        // Update the status if it changed
        if (newStatus !== csgApp.verificationStatus) {
          await db.update(csgApplications)
            .set({
              verificationStatus: newStatus,
              updatedAt: new Date()
            })
            .where(eq(csgApplications.id, csgApp.id))

          // Broadcast the status update
          broadcastVerificationUpdate(csgApp.applicationId, {
            status: newStatus,
            csg_id: key
          })
        }
      }
      
      return new Response(
        JSON.stringify(data, null, 2), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    } catch (error) {
      console.error('Error fetching CSG application:', error)
      if (axios.isAxiosError(error) && error.response?.status === 404) {
        return new Response(
          JSON.stringify({ 
            error: 'CSG application not found',
            details: error.message
          }), 
          { status: 404 }
        )
      }
      return new Response(
        JSON.stringify({ error: 'Failed to fetch CSG application' }), 
        { status: 500 }
      )
    }
  })
  .get('/producer-config', async () => {
    try {
      console.log('GET /producer-config - Fetching producer config...')
      const config = await getProducerConfig()
      console.log('GET /producer-config response:', {
        producerCount: config.producers.length,
        firstProducer: config.producers[0] ? {
          id: config.producers[0].id,
          name: `${config.producers[0].firstName} ${config.producers[0].lastName}`
        } : null
      })
      
      return new Response(
        JSON.stringify(config), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      )
    } catch (error) {
      console.error('Error fetching producer config:', error)
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fetch producer config',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      )
    }
  })
  
  // Verify CSG application using Puppeteer
  .get('/csg-application/:key/verify', async ({ params }) => {
    try {
      const { key } = params
      const { verifyCSGApplication } = await import('./csg/verify')
      
      const result = await verifyCSGApplication(key, {
        headless: true,
        debug: isDev
      })

      // Get the application ID from the database
      const db = getDb()
      const [csgApp] = await db
        .select()
        .from(csgApplications)
        .where(eq(csgApplications.key, key))

      if (csgApp) {
        // Broadcast the verification update
        broadcastVerificationUpdate(
          csgApp.applicationId,
          {
            status: result.success ? 'verified' : 'failed',
            screenshot: result.screenshot,
            verifyUrl: `${process.env.CSG_API_URL}/v1/e_app/enrollment_applications/${csgApp.key}/verify`,
            error: result.success ? undefined : result.error
          }
        )
      }
      
      return result
    } catch (error) {
      console.error('Error verifying CSG application:', error)
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error occurred'
      }
    }
  })

  // Add new endpoint for Chubb zip code fix
  .post('/csg-application/:key/fix-zip', async ({ params }) => {
    try {
      const { key } = params
      const { fixChubbZipCode, getAuthenticatedPage } = await import('./csg/verify')
      
      const page = await getAuthenticatedPage(true) // debug mode on
      try {
        await fixChubbZipCode(page, key, true)
        return new Response(
          JSON.stringify({ success: true }),
          { 
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        )
      } finally {
        await page.close()
      }
    } catch (error) {
      console.error('Error fixing Chubb zip code:', error)
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fix Chubb zip code',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      )
    }
  })

  .post('/api/applications', async (req, res) => {
    const { page, pageSize, searchTerm, hasContactFilter, naics, status } = req.body
    try {
      const result = await getApplications(page, pageSize, searchTerm, hasContactFilter, naics, status)
      res.json(result)
    } catch (error) {
      console.error('Error fetching applications:', error)
      res.status(500).json({ error: 'Internal server error' })
    }
  })

  .post('/api/bookings', async ({ body }) => {
    try {
      const { userId, applicationId, email, phone, url, event, status, data } = body as {
        userId?: string;
        applicationId?: string;
        email: string;
        phone?: string;
        url: string;
        event?: string;
        status: string;
        data?: Record<string, any>;
      };

      // Validate required fields
      if (!email || !url || !status) {
        return {
          success: false,
          error: 'Missing required fields: email, url, and status are required'
        };
      }

      const result = await createBooking({
        userId,
        applicationId,
        email,
        phone,
        url,
        event,
        status,
        data
      });

      console.log('Booking created successfully:', result.id);

      return {
        success: true,
        booking: result
      };
    } catch (error) {
      console.error('Error creating booking:', error);
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Failed to create booking'
      };
    }
  })
)

// --------------------------
// Production static handling
// --------------------------
if (!isDev) {
  try {
    const distPath = join(__dirname, '../../dist')

    const mimeTypes: Record<string, string> = {
      '.js': 'application/javascript',
      '.mjs': 'application/javascript',
      '.css': 'text/css',
      '.html': 'text/html',
      '.json': 'application/json',
      '.png': 'image/png',
      '.jpg': 'image/jpeg',
      '.jpeg': 'image/jpeg',
      '.gif': 'image/gif',
      '.svg': 'image/svg+xml',
      '.ico': 'image/x-icon',
      '.woff': 'font/woff',
      '.woff2': 'font/woff2',
      '.ttf': 'font/ttf',
      '.eot': 'application/vnd.ms-fontobject',
    }

    // Serve static files from the dist directory
    app.get('/assets/*', async ({ request }) => {
      const { pathname } = new URL(request.url);
      const filePath = join(distPath, pathname);
      const ext = pathname.substring(pathname.lastIndexOf('.'));
      const mimeType = mimeTypes[ext] || 'application/octet-stream';

      try {
        const file = Bun.file(filePath);
        return new Response(file, {
          headers: {
            'Content-Type': mimeType,
            'Cache-Control': 'public, max-age=31536000',
            'X-Content-Type-Options': 'nosniff'
          }
        });
      } catch (error) {
        return new Response('Not found', { status: 404 });
      }
    });

    // Serve root index.html
    app.get('/', async () => {
      try {
        const htmlPath = join(distPath, 'index.html');
        const file = Bun.file(htmlPath);
        return new Response(file, {
          headers: {
            'Content-Type': 'text/html',
            'Cache-Control': 'no-cache'
          }
        });
      } catch (error) {
        console.error('Error serving index.html:', error);
        return new Response('Server Error', { status: 500 });
      }
    });

    // Fallback route: serve index.html for non-asset requests
    app.get('*', async ({ request }) => {
      const { pathname } = new URL(request.url);

      // If the request has a file extension and wasn't served by previous routes, return 404
      if (/\.[^/]+$/.test(pathname)) {
        return new Response('Not found', { status: 404 });
      }

      // Serve index.html for all other routes - let the Elm router handle the routing
      try {
        const htmlPath = join(distPath, 'index.html');
        const file = Bun.file(htmlPath);
        return new Response(file, {
          headers: {
            'Content-Type': 'text/html',
            'Cache-Control': 'no-cache'
          }
        });
      } catch (error) {
        console.error('Error serving index.html:', error);
        return new Response('Server Error', { status: 500 });
      }
    });
  } catch (error) {
    console.error('Error setting up static file handling:', error)
  }
}

// Initialize CSG and start server
async function startServer() {
  try {
    // Start the server
    const port = Number(process.env.PORT) || 3000;
    
    const serverConfig: any = {
      port,
      hostname: '0.0.0.0',
      development: isDev
    };

    // Add SSL configuration in production
    if (!isDev && process.env.SSL_CERT && process.env.SSL_KEY) {
      serverConfig.tls = {
        cert: Bun.file(process.env.SSL_CERT),
        key: Bun.file(process.env.SSL_KEY)
      };
      console.log('SSL configuration loaded for secure WebSocket support');
    }

    await app.listen(serverConfig);
    
    console.log(`🦊 Server is running at ${isDev ? 'http' : 'https'}://localhost:${port} (${isDev ? 'development' : 'production'} mode)`);
  } catch (error) {
    console.error('Failed to start server:', error);
    process.exit(1);
  }
}

startServer()