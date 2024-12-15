import './style.css'
import { Elm } from './Dashboard.elm'
import { db } from './db'
import { applications, bookings, user, csgApplications } from './db/schema'
import { desc, eq, sql } from 'drizzle-orm'

// Define TypeScript types that match Elm's expectations
type Application = {
  id: string
  userId: string
  userEmail: string | null
  createdAt: string
  dateStarted: string
  dateCompleted: string | null
  status: 'quote' | 'review' | 'completed' | 'submitted_to_csg' | 'call_booked'
  state: string | null
  data: Record<string, any>
  name: string
  booking: {
    email: string
    phone: string | null
    url: string
    status: string
  } | null
  csgApplication: {
    key: string
    brokerEmail: string | null
  } | null
}

type PaginationRequest = {
  page: number
  pageSize: number
  searchTerm: string
  hasContactFilter: boolean
}

const app = Elm.Dashboard.init({
  node: document.getElementById('app')
})

// Helper function to format application data for Elm
const formatApplicationData = async (rawApplications: any[]): Promise<Application[]> => {
  // Get all related bookings for these applications
  const applicationIds = rawApplications.map(app => app.id)
  const relatedBookings = await db
    .select()
    .from(bookings)
    .where(sql`application_id IN ${applicationIds}`)

  // Get all related users
  const userIds = rawApplications.map(app => app.userId)
  const relatedUsers = await db
    .select()
    .from(user)
    .where(sql`id IN ${userIds}`)

  // Get related CSG applications
  const relatedCsgApps = await db
    .select()
    .from(csgApplications)
    .where(sql`application_id IN ${applicationIds}`)

  // Create lookup maps
  const bookingsByAppId = new Map(
    relatedBookings.map(booking => [booking.applicationId, booking])
  )
  const usersById = new Map(
    relatedUsers.map(user => [user.id, user])
  )
  const csgAppsByAppId = new Map(
    relatedCsgApps.map(csgApp => [csgApp.applicationId, csgApp])
  )

  return rawApplications.map(app => {
    const relatedBooking = bookingsByAppId.get(app.id)
    const relatedUser = usersById.get(app.userId)
    const relatedCsgApp = csgAppsByAppId.get(app.id)

    const safeDate = (dateStr: string | number): string => {
      try {
        return new Date(dateStr).toISOString();
      } catch {
        return String(dateStr);
      }
    }

    // Determine the status based on the new rules
    let status = determineStatus(app.status, !!relatedCsgApp, !!relatedBooking)

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

// Helper function to determine status
const determineStatus = (
  status: string,
  hasCsgApp: boolean,
  hasBooking: boolean
): 'completed' | 'review' | 'quote' | 'submitted_to_csg' | 'call_booked' => {
  if (hasCsgApp) {
    return 'submitted_to_csg'
  }
  if (hasBooking) {
    return 'call_booked'
  }
  
  switch (status.toLowerCase()) {
    case 'completed':
      return 'completed'
    case 'review':
      return 'review'
    default:
      return 'quote'
  }
}

// Listen for refresh requests from Elm
app.ports.requestRefresh?.subscribe(async (request: PaginationRequest) => {
  try {
    const { page, pageSize, searchTerm, hasContactFilter } = request
    
    const shouldSearch = searchTerm.length >= 3;
    const offset = page * pageSize
    const searchPattern = `%${searchTerm.toLowerCase()}%`

    // Build the base query
    let query = db
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

    // Add contact filter condition if enabled
    if (hasContactFilter) {
      query = query.where(
        sql`(
          -- Check for email or phone in application data
          json_extract(${applications.data}, '$.applicant_info.email') IS NOT NULL OR
          json_extract(${applications.data}, '$.applicant_info.phone') IS NOT NULL OR
          -- Check for email or phone in bookings
          EXISTS (
            SELECT 1 FROM ${bookings}
            WHERE ${bookings.applicationId} = ${applications.id}
            AND (${bookings.email} IS NOT NULL OR ${bookings.phone} IS NOT NULL)
          ) OR
          -- Check for user email
          EXISTS (
            SELECT 1 FROM ${user}
            WHERE ${user.id} = ${applications.userId}
            AND ${user.email} IS NOT NULL
          )
        )`
      )
    }

    // Add search conditions if there's a search term
    if (searchTerm && shouldSearch) {
      query = query.where(
        sql`(
          -- Search in application data JSON
          LOWER(json_extract(${applications.data}, '$.applicant_info.f_name')) LIKE ${searchPattern} OR
          LOWER(json_extract(${applications.data}, '$.applicant_info.l_name')) LIKE ${searchPattern} OR
          LOWER(json_extract(${applications.data}, '$.applicant_info.phone')) LIKE ${searchPattern} OR
          LOWER(json_extract(${applications.data}, '$.applicant_info.email')) LIKE ${searchPattern} OR
          -- Search in carrier name
          LOWER(${applications.name}) LIKE ${searchPattern} OR
          -- Search in related bookings
          EXISTS (
            SELECT 1 FROM ${bookings}
            WHERE ${bookings.applicationId} = ${applications.id}
            AND (
              LOWER(${bookings.email}) LIKE ${searchPattern}
              ${bookings.phone ? sql`OR LOWER(${bookings.phone}) LIKE ${searchPattern}` : sql``}
            )
          ) OR
          -- Search in related users
          EXISTS (
            SELECT 1 FROM ${user}
            WHERE ${user.id} = ${applications.userId}
            AND LOWER(${user.email}) LIKE ${searchPattern}
          )
        )`
      )
    }

    // Add ordering and pagination
    const results = await query
      .orderBy(desc(applications.createdAt))
      .limit(pageSize)
      .offset(offset)

    // Get total count with the same conditions as the main query
    let countQuery = db
      .select({ count: sql`count(*)`.mapWith(Number) })
      .from(applications)

    // Add contact filter condition to count query if enabled
    if (hasContactFilter) {
      countQuery = countQuery.where(
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

    // Add search conditions to count query if there's a search term
    if (searchTerm && shouldSearch) {
      countQuery = countQuery.where(
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

    const [{ count }] = await countQuery

    // Format the data for Elm
    const formattedApplications = await formatApplicationData(results)

    // Send the data back to Elm
    app.ports.receiveApplications.send({
      applications: formattedApplications,
      pagination: {
        total: count,
        page,
        pageSize,
        totalPages: Math.ceil(count / pageSize)
      }
    })
  } catch (error) {
    console.error('Error fetching applications:', error)
  }
})

// Handle CSV export requests
app.ports.exportToCsv?.subscribe(async ({ searchTerm, hasContactFilter, hasCSGFilter }) => {
  try {
    let query = db
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

    if (searchTerm) {
      query = query.where(sql`name LIKE ${`%${searchTerm}%`}`)
    }

    const results = await query
    const formattedResults = await formatApplicationData(results)
    
    // Convert results to CSV
    const csvContent = convertToCSV(formattedResults)
    downloadCSV(csvContent, 'applications-export.csv')
  } catch (error) {
    console.error('Error exporting to CSV:', error)
  }
})

function convertToCSV(data: Application[]): string {
  const headers = ['ID', 'Name', 'Carrier', 'Status', 'Email', 'Phone', 'Date Started', 'Date Completed']
  const rows = data.map(item => {
    const applicantInfo = item.data?.applicant_info || {}
    const fullName = `${applicantInfo.f_name || ''} ${applicantInfo.l_name || ''}`.trim()
    
    return [
      item.id,
      fullName,
      item.name,
      item.status,
      item.booking?.email || item.userEmail || '',
      item.booking?.phone || '',
      new Date(item.dateStarted).toLocaleDateString(),
      item.dateCompleted ? new Date(item.dateCompleted).toLocaleDateString() : ''
    ]
  })
  
  return [
    headers.join(','),
    ...rows.map(row => row.join(','))
  ].join('\n')
}

function downloadCSV(content: string, filename: string) {
  const blob = new Blob([content], { type: 'text/csv;charset=utf-8;' })
  const link = document.createElement('a')
  link.href = URL.createObjectURL(blob)
  link.download = filename
  link.click()
  URL.revokeObjectURL(link.href)
}