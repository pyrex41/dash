import { Elysia } from 'elysia'
import { cors } from '@elysiajs/cors'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'
import staticPlugin from '@elysiajs/static'
import { getApplications, exportApplications, getApplicationWithSchema, updateFormattedData, getProducerConfig } from './db/query'
import { format_application, getCarrierName } from './formatter'
import { submitToCSG } from './csg/submit'
import { makeCSGRequest } from './csg/token'
import axios from 'axios'
import { getHeaders } from './csg/submit'
import { eq } from 'drizzle-orm'
import { getDb } from './db'
import { csgApplications, applications } from './db/schema'
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

// Helper function to broadcast verification updates only to subscribed clients
export async function broadcastVerificationUpdate(applicationId: string, body: any) {
  const msgId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const timestamp = new Date().toISOString();
  
  console.log(`[${timestamp}] Broadcasting verification update (msgId: ${msgId}) for application:`, applicationId);
  console.log('Update body:', body);

  // Get the current application state from the database
  const db = getDb();
  const [application] = await db
    .select()
    .from(applications)
    .leftJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
    .where(eq(applications.id, applicationId));

  if (!application) {
    console.error(`[${timestamp}] Application not found for verification update:`, applicationId);
    return;
  }
  
  const message = JSON.stringify({
    type: 'verification_update',
    msgId,
    timestamp,
    applicationId,
    body: {
      status: application.csg_applications?.verificationStatus || body.status,
      csg_id: application.csg_applications?.key || body.key,
      applicationStatus: application.applications.status || body.applicationStatus,
      error: application.csg_applications?.verificationError || body.error,
      screenshot: application.csg_applications?.verificationScreenshot || body.screenshot,
      verifyUrl: body.verifyUrl,
      signatureUrl: body.signatureUrl
    }
  });
  
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
    idleTimeout: 30
  },
  serve: {
    idleTimeout: 120
  }
})
.use(cors())
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
        ws.send(JSON.stringify({ type: 'ping' }))
      } catch (error) {
        console.error('Heartbeat failed, cleaning up:', error)
        const heartbeat = wsData.heartbeatInterval
        if (heartbeat) clearInterval(heartbeat)
        wsClients.delete(ws.id)
      }
    }, 20000)
    
    wsClients.set(ws.id, wsData)
  },
  
  message(ws, message) {
    try {
      const data = typeof message === 'string' ? JSON.parse(message) : message
      const wsData = wsClients.get(ws.id)
      
      if (!wsData) {
        console.error('No WebSocket data found for client:', ws.id)
        return
      }
      
      if (data.type === 'pong') {
        return
      }
      
      // Handle subscription messages
      if (data.type === 'subscribe') {
        const applicationIds = data.applicationIds as string[]
        if (Array.isArray(applicationIds)) {
          // Add new subscriptions without clearing existing ones
          applicationIds.forEach(id => wsData.subscriptions.add(id))
          
          // Send confirmation
          ws.send(JSON.stringify({
            type: 'subscribed',
            applicationIds: Array.from(wsData.subscriptions)
          }))
          
          console.log('Client subscribed to applications:', applicationIds)
          console.log('Total subscriptions:', Array.from(wsData.subscriptions))
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
          getApplicationWithSchema(applicationId).then(application => {
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
        const { page = 0, pageSize = 20, searchTerm = '', hasContactFilter = false, naics = [] } = data
        getApplications(page, pageSize, searchTerm, hasContactFilter, naics).then(result => {
          ws.send(JSON.stringify({
            type: 'applications_data',
            applications: result
          }))
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'applications_error',
            error: error instanceof Error ? error.message : 'Failed to load applications'
          }))
        })
      }

      // Handle save application requests
      if (data.type === 'save_application') {
        const { id, formData, medications } = data
        updateFormattedData(id, formData, medications).then(() => {
          // Reset verification status for any associated CSG application
          const db = getDb()
          return db.update(csgApplications)
            .set({
              verificationStatus: 'pending',
              verificationScreenshot: null,
              verificationError: null,
              lastVerifiedAt: null,
              updatedAt: new Date()
            })
            .where(eq(csgApplications.applicationId, id))
            .then(() => {
              ws.send(JSON.stringify({
                type: 'save_application_response',
                id,
                success: true,
                error: null
              }))
            })
        }).catch(error => {
          ws.send(JSON.stringify({
            type: 'save_application_response',
            id,
            success: false,
            error: error instanceof Error ? error.message : 'Failed to save application'
          }))
        })
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
      console.log('Verification statuses:', result.applications.map(app => ({
        id: app.id,
        verificationStatus: app.csgApplication?.verificationStatus
      })))
      
      console.log('GET /applications response:', {
        total: result.pagination.total,
        totalPages: result.pagination.totalPages,
        applicationCount: result.applications.length,
        pendingVerifications: result.applications.filter(
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
    app.use(staticPlugin({
      assets: distPath,
      prefix: '/',
      alwaysStatic: true,
      headers: {
        'Content-Type': 'application/octet-stream',
        'Cache-Control': 'public, max-age=31536000',
        'X-Content-Type-Options': 'nosniff'
      }
    }))

    // Fallback route: serve index.html for non-asset requests
    app.get('*', async ({ request }) => {
      const { pathname } = new URL(request.url)

      // If the request has a file extension and wasn't served by staticPlugin, return 404
      if (/\.[^/]+$/.test(pathname)) {
        return new Response('Not found', { status: 404 })
      }

      // Serve index.html for all routes - let the Elm router handle the routing
      try {
        const htmlPath = join(distPath, 'index.html')
        const html = await Bun.file(htmlPath).text()
        return new Response(html, {
          headers: {
            'Content-Type': 'text/html',
            'Cache-Control': 'no-cache'
          }
        })
      } catch (error) {
        console.error('Error serving index.html:', error)
        return new Response('Server Error', { status: 500 })
      }
    })
  } catch (error) {
    console.error('Error setting up static file handling:', error)
  }
}

// Initialize CSG and start server
async function startServer() {
  try {
    // Start the server
    const port = Number(process.env.PORT) || 3000
    await app.listen({
      port,
      hostname: '0.0.0.0',
      development: isDev
    })
    
    console.log(`🦊 Server is running at http://localhost:${port} (${isDev ? 'development' : 'production'} mode)`)
  } catch (error) {
    console.error('Failed to start server:', error)
    process.exit(1)
  }
}

startServer()