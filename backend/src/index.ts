import { Elysia } from 'elysia'
import { cors } from '@elysiajs/cors'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'
import staticPlugin from '@elysiajs/static'
import { getApplications, exportApplications, getApplicationWithSchema } from './db/query'
import { format_application, getCarrierName } from './formatter'

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
      
      console.log('GET /applications query params:', {
        page,
        pageSize,
        searchTerm,
        hasContactFilter
      })
      
      const result = await getApplications(page, pageSize, searchTerm, hasContactFilter)
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
        rawData: application?.data
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