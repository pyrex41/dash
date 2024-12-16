import { Elysia } from 'elysia'
import { cors } from '@elysiajs/cors'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'
import staticPlugin from '@elysiajs/static'
import { drizzle } from 'drizzle-orm/libsql'
import { createClient } from '@libsql/client'
import { desc, sql } from 'drizzle-orm'
import { applications, bookings, user, csgApplications } from './db/schema'

// Resolve __dirname for ESM environments
const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

// Detect environment
const isDev = process.env.NODE_ENV === 'development' || !process.env.NODE_ENV

// Database configuration
const client = createClient({
  url: process.env.TURSO_DATABASE_URL!,
  authToken: process.env.TURSO_AUTH_TOKEN!,
})
const db = drizzle(client)

// Helper function to format application data
const formatApplicationData = async (rawApplications: any[]) => {
  if (rawApplications.length === 0) return []

  const applicationIds = rawApplications.map(app => app.id)
  const userIds = rawApplications.map(app => app.userId)

  const [relatedBookings, relatedUsers, relatedCsgApps] = await Promise.all([
    db.select().from(bookings).where(sql`application_id IN ${applicationIds}`),
    db.select().from(user).where(sql`id IN ${userIds}`),
    db.select().from(csgApplications).where(sql`application_id IN ${applicationIds}`)
  ])

  const bookingsByAppId = new Map(relatedBookings.map(booking => [booking.applicationId, booking]))
  const usersById = new Map(relatedUsers.map(user => [user.id, user]))
  const csgAppsByAppId = new Map(relatedCsgApps.map(csgApp => [csgApp.applicationId, csgApp]))

  return rawApplications.map(app => {
    const relatedBooking = bookingsByAppId.get(app.id)
    const relatedUser = usersById.get(app.userId)
    const relatedCsgApp = csgAppsByAppId.get(app.id)

    const safeDate = (dateStr: string | number): string => {
      try {
        return new Date(dateStr).toISOString()
      } catch {
        return String(dateStr)
      }
    }

    const status = determineStatus(app.status, !!relatedCsgApp, !!relatedBooking)

    return {
      id: app.id,
      userId: app.userId,
      userEmail: relatedUser?.email || null,
      createdAt: safeDate(app.createdAt),
      dateStarted: safeDate(app.createdAt),
      dateCompleted: null,
      status,
      state: null,
      data: typeof app.data === 'string' ? JSON.parse(app.data) : app.data,
      name: app.name || 'Unknown',
      booking: relatedBooking ? {
        email: relatedBooking.email,
        phone: relatedBooking.phone,
        url: relatedBooking.url,
        status: relatedBooking.status
      } : null,
      csgApplication: relatedCsgApp ? {
        key: relatedCsgApp.key,
        brokerEmail: relatedCsgApp.brokerEmail
      } : null
    }
  })
}

const determineStatus = (
  status: string,
  hasCsgApp: boolean,
  hasBooking: boolean
): 'completed' | 'review' | 'quote' | 'submitted_to_csg' | 'call_booked' => {
  if (hasCsgApp) return 'submitted_to_csg'
  if (hasBooking) return 'call_booked'
  
  switch (status.toLowerCase()) {
    case 'completed': return 'completed'
    case 'review': return 'review'
    default: return 'quote'
  }
}

const app = new Elysia().use(cors())

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
      
      const offset = page * pageSize
      const searchPattern = `%${searchTerm.toLowerCase()}%`
      const shouldSearch = searchTerm.length >= 3

      // Build base query
      let dbQuery = db
        .select({
          id: applications.id,
          userId: applications.userId,
          status: applications.status,
          createdAt: applications.createdAt,
          updatedAt: applications.updatedAt,
          data: applications.data,
          name: applications.name,
        })
        .from(applications)

      // Add contact filter
      if (hasContactFilter) {
        dbQuery = dbQuery.where(
          sql`(
            json_extract(${applications.data}, '$.applicant_info.email') IS NOT NULL OR
            json_extract(${applications.data}, '$.applicant_info.phone') IS NOT NULL OR
            EXISTS (
              SELECT 1 FROM ${bookings}
              WHERE ${bookings.applicationId} = ${applications.id}
              AND (${bookings.email} IS NOT NULL OR ${bookings.phone} IS NOT NULL)
            ) OR
            EXISTS (
              SELECT 1 FROM ${user}
              WHERE ${user.id} = ${applications.userId}
              AND ${user.email} IS NOT NULL
            )
          )`
        )
      }

      // Add search conditions
      if (shouldSearch) {
        dbQuery = dbQuery.where(
          sql`(
            LOWER(json_extract(${applications.data}, '$.applicant_info.f_name')) LIKE ${searchPattern} OR
            LOWER(json_extract(${applications.data}, '$.applicant_info.l_name')) LIKE ${searchPattern} OR
            LOWER(json_extract(${applications.data}, '$.applicant_info.phone')) LIKE ${searchPattern} OR
            LOWER(json_extract(${applications.data}, '$.applicant_info.email')) LIKE ${searchPattern} OR
            LOWER(${applications.name}) LIKE ${searchPattern} OR
            EXISTS (
              SELECT 1 FROM ${bookings}
              WHERE ${bookings.applicationId} = ${applications.id}
              AND (
                LOWER(${bookings.email}) LIKE ${searchPattern}
                ${bookings.phone ? sql`OR LOWER(${bookings.phone}) LIKE ${searchPattern}` : sql``}
              )
            ) OR
            EXISTS (
              SELECT 1 FROM ${user}
              WHERE ${user.id} = ${applications.userId}
              AND LOWER(${user.email}) LIKE ${searchPattern}
            )
          )`
        )
      }

      // Execute queries
      const [results, totalCount] = await Promise.all([
        dbQuery
          .orderBy(desc(applications.createdAt))
          .limit(pageSize)
          .offset(offset),
        db
          .select({ count: sql`count(*)`.mapWith(Number) })
          .from(applications)
          .where(
            shouldSearch || hasContactFilter
              ? sql`${
                  hasContactFilter
                    ? sql`(
                        json_extract(${applications.data}, '$.applicant_info.email') IS NOT NULL OR
                        json_extract(${applications.data}, '$.applicant_info.phone') IS NOT NULL OR
                        EXISTS (
                          SELECT 1 FROM ${bookings}
                          WHERE ${bookings.applicationId} = ${applications.id}
                          AND (${bookings.email} IS NOT NULL OR ${bookings.phone} IS NOT NULL)
                        ) OR
                        EXISTS (
                          SELECT 1 FROM ${user}
                          WHERE ${user.id} = ${applications.userId}
                          AND ${user.email} IS NOT NULL
                        )
                      )`
                    : sql`1=1`
                } AND ${
                  shouldSearch
                    ? sql`(
                        LOWER(json_extract(${applications.data}, '$.applicant_info.f_name')) LIKE ${searchPattern} OR
                        LOWER(json_extract(${applications.data}, '$.applicant_info.l_name')) LIKE ${searchPattern} OR
                        LOWER(json_extract(${applications.data}, '$.applicant_info.phone')) LIKE ${searchPattern} OR
                        LOWER(json_extract(${applications.data}, '$.applicant_info.email')) LIKE ${searchPattern} OR
                        LOWER(${applications.name}) LIKE ${searchPattern} OR
                        EXISTS (
                          SELECT 1 FROM ${bookings}
                          WHERE ${bookings.applicationId} = ${applications.id}
                          AND (
                            LOWER(${bookings.email}) LIKE ${searchPattern}
                            ${bookings.phone ? sql`OR LOWER(${bookings.phone}) LIKE ${searchPattern}` : sql``}
                          )
                        ) OR
                        EXISTS (
                          SELECT 1 FROM ${user}
                          WHERE ${user.id} = ${applications.userId}
                          AND LOWER(${user.email}) LIKE ${searchPattern}
                        )
                      )`
                    : sql`1=1`
                }`
              : undefined
          )
      ])

      const formattedApplications = await formatApplicationData(results)

      return {
        applications: formattedApplications,
        pagination: {
          total: totalCount[0].count,
          page,
          pageSize,
          totalPages: Math.ceil(totalCount[0].count / pageSize)
        }
      }
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
      
      let dbQuery = db
        .select({
          id: applications.id,
          name: applications.name,
          status: applications.status,
          createdAt: applications.createdAt,
          updatedAt: applications.updatedAt,
          data: applications.data,
        })
        .from(applications)
        .orderBy(desc(applications.createdAt))

      if (searchTerm.length >= 3) {
        dbQuery = dbQuery.where(sql`name LIKE ${`%${searchTerm}%`}`)
      }

      const results = await dbQuery
      const formattedResults = await formatApplicationData(results)
      
      return formattedResults
    } catch (error) {
      console.error('Error exporting applications:', error)
      return []
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
      headers: (path) => {
        const ext = '.' + path.split('.').pop()
        const mime = mimeTypes[ext] || 'application/octet-stream'
        return {
          'Content-Type': mime,
          'Cache-Control': 'public, max-age=31536000',
          'X-Content-Type-Options': 'nosniff',
        }
      }
    }))

    // Fallback route: serve index.html for non-asset requests
    app.get('*', async ({ request }) => {
      const { pathname } = new URL(request.url)

      // If the request has a file extension and wasn't served by staticPlugin, return 404
      if (/\.[^/]+$/.test(pathname)) {
        return new Response('Not found', { status: 404 })
      }

      // Otherwise, serve the SPA's index.html
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