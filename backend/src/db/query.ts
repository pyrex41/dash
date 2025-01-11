import { drizzle } from 'drizzle-orm/libsql'
import { createClient } from '@libsql/client'
import { desc, sql } from 'drizzle-orm'
import { applications, bookings, user, csgApplications, producers } from './schema'
import { eq } from 'drizzle-orm'

const formatServer = process.env.FORMAT_SERVER_URL 
console.log('formatServer', formatServer)

const getFormatUrl = (applicationId: string) => {
  return `${formatServer}/api/formatter/api/applications/${applicationId}/formatted?skip_medication=true&skip_producer=true`
}

const format_application = async (applicationId: string) => {
  const url = getFormatUrl(applicationId)
  console.log('format_application', url)
  const response = await fetch(url)
  const data = await response.json()
  return data
}

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
      naic: app.naic,
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

// Add a new function to get a single application with schema
export const getApplicationWithSchema = async (applicationId: string) => {
  const results = await db
    .select({
      id: applications.id,
      userId: applications.userId,
      status: applications.status,
      createdAt: applications.createdAt,
      data: applications.data,
      formattedData: applications.formattedData,
      rawMedications: applications.rawMedications,
      schema: applications.originalSchema,
      name: applications.name,
      naic: applications.naic,
    })
    .from(applications)
    .where(sql`${applications.id} = ${applicationId}`)
    .all()

  const application = results[0]
  if (!application) {
    return null
  }

  console.log('Raw application from database:', {
    id: application.id,
    data: application.data,
    formattedData: application.formattedData,
    rawMedications: application.rawMedications
  })

  return {
    ...application,
    data: typeof application.data === 'string' ? JSON.parse(application.data) : application.data,
    formattedData: application.formattedData ? (typeof application.formattedData === 'string' ? JSON.parse(application.formattedData) : application.formattedData) : null,
    rawMedications: application.rawMedications ? (typeof application.rawMedications === 'string' ? JSON.parse(application.rawMedications) : application.rawMedications) : []
  }
}

export const getFromattedApplicationWithSchema = async (applicationId: string) => {
  const application = await getApplicationWithSchema(applicationId)
  if (!application) {
    return null
  }

  const newData = await format_application(applicationId)
  // Compare old and new data structures
  const oldData = typeof application.data === 'string' ? JSON.parse(application.data) : application.data
  console.log('\nComparing old and new data:')

  // Get all section keys from both objects
  const allSections = new Set([...Object.keys(oldData || {}), ...Object.keys(newData || {})])

  allSections.forEach(section => {
    const oldSection = oldData?.[section]
    const newSection = newData?.[section]

    if (!oldSection) {
      console.log(`\nSection '${section}' only exists in new data:`, newSection)
    } else if (!newSection) {
      console.log(`\nSection '${section}' only exists in old data:`, oldSection)
    } else {
      // Compare fields within section
      const allFields = new Set([...Object.keys(oldSection), ...Object.keys(newSection)])
      const differences: Record<string, {old?: any, new?: any}> = {}

      allFields.forEach(field => {
        if (!(field in oldSection)) {
          differences[field] = {new: newSection[field]}
        } else if (!(field in newSection)) {
          differences[field] = {old: oldSection[field]}
        } else if (oldSection[field] !== newSection[field]) {
          differences[field] = {
            old: oldSection[field],
            new: newSection[field]
          }
        }
      })

      if (Object.keys(differences).length > 0) {
        console.log(`\nDifferences in section '${section}':`)
        Object.entries(differences).forEach(([field, diff]) => {
          console.log(`  ${field}:`, diff)
        })
      }
    }
  })

  return {
    ...application,
    data: newData,
    rawMedications: application.rawMedications || []
  }
}

export const getApplications = async (page: number, pageSize: number, searchTerm: string, hasContactFilter: boolean, naics: string[] = []) => {
  const offset = page * pageSize
  const searchPattern = `%${searchTerm.toLowerCase()}%`
  const shouldSearch = searchTerm.length >= 3

  // Build the WHERE clause conditions
  const whereConditions = []
  
  if (hasContactFilter) {
    whereConditions.push(sql`(
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
    )`)
  }

  if (naics.length > 0) {
    whereConditions.push(sql`${applications.naic} IN ${naics}`)
  }

  if (shouldSearch) {
    whereConditions.push(sql`(
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
    )`)
  }

  // Combine conditions with AND if there are multiple conditions
  const whereClause = whereConditions.length > 0 
    ? sql`WHERE ${sql.join(whereConditions, sql` AND `)}` 
    : sql``

  // Execute queries
  const [results, totalCount] = await Promise.all([
    db
      .select({
        id: applications.id,
        userId: applications.userId,
        status: applications.status,
        createdAt: applications.createdAt,
        data: applications.data,
        name: applications.name,
      })
      .from(applications)
      .where(whereConditions.length > 0 ? sql.join(whereConditions, sql` AND `) : undefined)
      .orderBy(desc(applications.createdAt))
      .limit(pageSize)
      .offset(offset),
    db
      .select({ count: sql`count(*)`.mapWith(Number) })
      .from(applications)
      .where(whereConditions.length > 0 ? sql.join(whereConditions, sql` AND `) : undefined)
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

export async function updateFormattedData(id: string, formattedData: Record<string, any>, rawMedications?: any[]) {
    return await db
        .update(applications)
        .set({ 
            formattedData,
            rawMedications: rawMedications || null,
            updatedAt: new Date()
        })
        .where(eq(applications.id, id));
}

export async function getProducerConfig() {
    try {
        console.log('Querying producers from database...');
        const producersResult = await db
            .select({
                id: producers.id,
                firstName: producers.firstName,
                lastName: producers.lastName,
                phone: producers.phone,
                email: producers.email,
                addressLine1: producers.addressLine1,
                addressCity: producers.addressCity,
                addressState: producers.addressState,
                addressZip5: producers.addressZip5,
                npn: producers.npn,
                writingNumbers: producers.writingNumbers,
                isDefault: producers.isDefault,
            })
            .from(producers)
            .orderBy(producers.lastName, producers.firstName);

        console.log('Raw producers result:', {
            count: producersResult.length,
            firstProducer: producersResult[0] ? {
                id: producersResult[0].id,
                name: `${producersResult[0].firstName} ${producersResult[0].lastName}`
            } : null
        });

        const formattedProducers = producersResult.map(p => ({
            ...p,
            writingNumbers: typeof p.writingNumbers === 'string' ? JSON.parse(p.writingNumbers) : p.writingNumbers
        }));

        return {
            producers: formattedProducers
        };
    } catch (error) {
        console.error('Error in getProducerConfig:', error);
        throw error; // Re-throw to be handled by the route handler
    }
}