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
import { initializeCSG, cleanup as cleanupCSG } from './csg/verify'
import { eq } from 'drizzle-orm'
import { getDb } from './db'
import { csgApplications } from './db/schema'

// Resolve __dirname for ESM environments
const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

// Detect environment
const isDev = process.env.NODE_ENV === 'development' || !process.env.NODE_ENV

const app = new Elysia({
  serve: {
    hostname: '0.0.0.0',
    port: Number(process.env.PORT) || 3000,
    idleTimeout: 240, // 4 minutes (must be <= 255 seconds)
    development: isDev
  }
}).use(cors({
  origin: [
    'http://localhost:5173',  // Development
    'http://localhost:3000',  // Local production
  ],
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'HEAD'],
  credentials: true,
  allowedHeaders: ['Content-Type', 'Authorization', 'X-Requested-With'],
  exposeHeaders: ['Content-Length', 'Content-Type'],
  maxAge: 600
}))

// --------------------------
// API Routes
// --------------------------
app.group('/api', app => app
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
      console.log('GET /applications response:', {
        total: result.pagination.total,
        totalPages: result.pagination.totalPages,
        applicationCount: result.applications.length
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
      rawMedications: application.rawMedications,
      data: application.data,
      formattedData: application.formattedData
    })
    return new Response(
      JSON.stringify(application),
      { status: 200 }
    )
  })
  .get('/csg-applications', async ({ query }) => {
    try {
      console.log('GET /api/csg-applications - Starting request...');
      const limit = query?.limit || '10';
      
      const data = await makeCSGRequest({
        method: 'GET',
        url: `/v1/e_app/enrollment_applications.json`,
        params: { limit }
      });
      
      console.log('Successfully fetched CSG applications:', {
        count: Array.isArray(data) ? data.length : 'N/A',
        isArray: Array.isArray(data),
        firstItem: Array.isArray(data) && data.length > 0 ? data[0] : null
      });
      
      return new Response(
        JSON.stringify(data), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    } catch (error) {
      console.error('Error fetching CSG applications:', error);
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fetch CSG applications',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      );
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
      const db = getDb();
      await db.update(csgApplications)
        .set({
          verificationStatus: 'pending',
          verificationScreenshot: null,
          verificationError: null,
          lastVerifiedAt: null,
          updatedAt: new Date()
        })
        .where(eq(csgApplications.applicationId, id));
      
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
      return { access_token: auth.access_token };
    } catch (error) {
      console.error('Error getting LAPRO token:', error);
      return new Response(
        JSON.stringify({ error: 'Failed to get token' }), 
        { status: 500, headers: { 'Content-Type': 'application/json' } }
      );
    }
  })
  .post('/applications/:id/submit', async ({ params, body }) => {
    try {
      const { producerId, forceResubmit } = body as { producerId: number, forceResubmit?: boolean };
      if (!producerId) {
        return {
          success: false,
          error: 'Producer ID is required'
        };
      }

      const db = getDb();
      const [existingCsg] = await db
        .select()
        .from(csgApplications)
        .where(eq(csgApplications.applicationId, params.id));

      // If there's an existing submission and we're not forcing resubmit, return error
      if (existingCsg && !forceResubmit) {
        return {
          success: false,
          error: 'Application already submitted to CSG',
          existingSubmission: true,
          key: existingCsg.key,
          verificationStatus: existingCsg.verificationStatus
        };
      }

      const result = await submitToCSG(params.id, producerId);
      
      // Start verification in the background
      const port = Number(process.env.PORT) || 3000;
      const verifyUrl = `http://localhost:${port}/api/csg-application/${result.key}/verify`;
      fetch(verifyUrl).catch(error => {
        console.error('Error starting verification:', error);
      });

      return { 
        success: true,  
        data: result,
        verificationStatus: 'pending'
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
      const key = params.key.trim();
      
      const data = await makeCSGRequest({
        method: 'GET',
        url: `/v1/e_app/enrollment_applications/${key}.json`
      });
      
      return new Response(
        JSON.stringify(data, null, 2), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    } catch (error) {
      console.error('Error fetching CSG application:', error);
      if (axios.isAxiosError(error) && error.response?.status === 404) {
        return new Response(
          JSON.stringify({ 
            error: 'CSG application not found',
            details: error.message
          }), 
          { status: 404 }
        );
      }
      return new Response(
        JSON.stringify({ error: 'Failed to fetch CSG application' }), 
        { status: 500 }
      );
    }
  })
  .get('/producer-config', async () => {
    try {
      console.log('GET /producer-config - Fetching producer config...');
      const config = await getProducerConfig();
      console.log('GET /producer-config response:', {
        producerCount: config.producers.length,
        firstProducer: config.producers[0] ? {
          id: config.producers[0].id,
          name: `${config.producers[0].firstName} ${config.producers[0].lastName}`
        } : null
      });
      
      return new Response(
        JSON.stringify(config), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    } catch (error) {
      console.error('Error fetching producer config:', error);
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fetch producer config',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      );
    }
  })
  
  // Verify CSG application using Puppeteer
  .get('/csg-application/:key/verify', async ({ params }) => {
    try {
      const { key } = params;
      const { verifyCSGApplication } = await import('./csg/verify');
      
      const result = await verifyCSGApplication(key, {
        headless: true,
        debug: process.env.NODE_ENV === 'development'
      });
      
      return new Response(
        JSON.stringify(result),
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    } catch (error) {
      console.error('Error verifying CSG application:', error);
      return new Response(
        JSON.stringify({ 
          error: 'Failed to verify CSG application',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      );
    }
  })

  // Add new endpoint for Chubb zip code fix
  .post('/csg-application/:key/fix-zip', async ({ params }) => {
    try {
      const { key } = params;
      const { fixChubbZipCode, getAuthenticatedPage } = await import('./csg/verify');
      
      const page = await getAuthenticatedPage(true); // debug mode on
      try {
        await fixChubbZipCode(page, key, true);
        return new Response(
          JSON.stringify({ success: true }),
          { 
            status: 200,
            headers: { 'Content-Type': 'application/json' }
          }
        );
      } finally {
        await page.close();
      }
    } catch (error) {
      console.error('Error fixing Chubb zip code:', error);
      return new Response(
        JSON.stringify({ 
          error: 'Failed to fix Chubb zip code',
          details: error instanceof Error ? error.message : String(error)
        }), 
        { status: 500 }
      );
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

let isInitializing = false;
let isInitialized = false;

async function startServer() {
  try {
    if (!isInitialized && !isInitializing) {
      isInitializing = true;
      console.log('Initializing CSG session...');
      await initializeCSG(isDev);
      console.log('CSG session initialized successfully');
      isInitialized = true;
      isInitializing = false;
    }
    
    // Create server instance but don't start listening yet
    const server = app.listen();
    
    // Wait a moment for everything to initialize
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Start accepting connections
    server.listen(process.env.PORT || 3000);
    
    console.log(
      `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port} (${isDev ? 'development' : 'production'} mode)`
    );

    // Handle cleanup on server shutdown
    process.on('SIGTERM', async () => {
      console.log('SIGTERM received. Cleaning up...');
      await cleanupCSG();
      await server.stop();
      process.exit(0);
    });

    process.on('SIGINT', async () => {
      console.log('SIGINT received. Cleaning up...');
      await cleanupCSG();
      await server.stop();
      process.exit(0);
    });

  } catch (error) {
    isInitializing = false;
    console.error('Failed to start server:', error);
    process.exit(1);
  }
}

startServer();