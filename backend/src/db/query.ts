import { drizzle } from 'drizzle-orm/libsql'
import { createClient } from '@libsql/client'
import { desc, sql } from 'drizzle-orm'
import { applications, bookings, user, csgApplications } from './schema'

// Database configuration
const client = createClient({
  url: process.env.TURSO_DATABASE_URL!,
  authToken: process.env.TURSO_AUTH_TOKEN!,
})
export const db = drizzle(client)

// Helper function to format application data
export const formatApplicationData = async (rawApplications: any[]) => {
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

export const getApplications = async (page: number, pageSize: number, searchTerm: string, hasContactFilter: boolean) => {
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
      .where(shouldSearch || hasContactFilter
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
}

export const exportApplications = async (searchTerm: string, hasContactFilter: boolean) => {
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
  return formatApplicationData(results)
}