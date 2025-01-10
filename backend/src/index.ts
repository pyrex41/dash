import { Elysia } from 'elysia'
import { cors } from '@elysiajs/cors'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'
import staticPlugin from '@elysiajs/static'
import { getApplications, exportApplications, getApplicationWithSchema, updateFormattedData } from './db/query'
import { format_application, getCarrierName } from './formatter'
import { submitToCSG } from './csg/submit'
import { getToken } from './csg/token'

// Resolve __dirname for ESM environments
const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

// Detect environment
const isDev = process.env.NODE_ENV === 'development' || !process.env.NODE_ENV

const app = new Elysia().use(cors({
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
  .get('/applications/:id', async ({ params }) => {
    try {
      const application = await getApplicationWithSchema(params.id)
      
      console.log('GET /application response:', {
        id: application?.id,
        hasData: !!application?.data,
        hasSchema: !!application?.schema,
        rawData: application?.data,
        formattedData: application?.formattedData
      })
      
      if (!application) {
        return new Response('Application not found', { 
          status: 404,
          headers: { 'Content-Type': 'application/json' }
        })
      }
      
      return application
    } catch (error) {
      console.error('Error fetching application:', error)
      return new Response('Server error', { 
        status: 500,
        headers: { 'Content-Type': 'application/json' }
      })
    }
  })
  .put('/applications/:id/formatted', async ({ params, body }) => {
    try {
      const { id } = params
      const { data } = body as { data: Record<string, any> }
      
      console.log('PUT /applications/:id/formatted:', {
        id,
        dataKeys: Object.keys(data)
      })
      
      await updateFormattedData(id, data)
      
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
      const { producerId } = body as { producerId: number };
      if (!producerId) {
        return {
          success: false,
          error: 'Producer ID is required'
        };
      }
      const result = await submitToCSG(params.id, producerId);
      return { success: true, data: result };
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
      const csgApiUrl = process.env.CSG_API_URL || 'https://api.csgactuarial.com';
      
      // Get token using the token management system
      console.log('key', key)
      
      const response = await fetch(`${csgApiUrl}/v1/e_app/enrollment_applications/${key}.json`, {
        headers: {
          'x-api-token': await getToken(),
          'Content-Type': 'application/json'
        }
      });
      
      if (!response.ok) {
        const errorText = await response.text();
        console.error('CSG API error:', {
          status: response.status,
          statusText: response.statusText,
          body: errorText
        });
        
        return new Response(
          JSON.stringify({ 
            error: response.status === 404 ? 'CSG application not found' : 'Failed to fetch CSG application',
            details: errorText
          }), 
          { status: response.status }
        );
      }
      
      const data = await response.json();
      return new Response(
        JSON.stringify(data, null, 2), 
        { 
          status: 200,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    } catch (error) {
      console.error('Error fetching CSG application:', error);
      return new Response(
        JSON.stringify({ error: 'Failed to fetch CSG application' }), 
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

app.listen(process.env.PORT || 3000)

console.log(
  `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port} (${isDev ? 'development' : 'production'} mode)`
)