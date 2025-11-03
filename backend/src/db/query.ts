import { drizzle } from 'drizzle-orm/libsql'
import { createClient } from '@libsql/client'
import { desc, sql } from 'drizzle-orm'
import { applications, bookings, user, csgApplications, producers, onboarding } from './schema'
import { eq } from 'drizzle-orm'
import { getDb } from '.'
import { config } from 'dotenv'
import { resolve } from 'path'

// Load environment variables (go up one directory from backend/)
// Use override: true to replace any existing environment variables
config({ path: resolve(process.cwd(), '../.env'), override: true })

const formatServer = process.env.FORMAT_SERVER_URL
const formatServerApiKey = process.env.FORMAT_SERVER_API_KEY
console.log('========================================')
console.log('FORMAT_SERVER_URL loaded:', formatServer)
console.log('FORMAT_SERVER_API_KEY loaded:', formatServerApiKey ? 'Set' : 'NOT SET')
console.log('Expected: https://csgformat-1-pyrex41.replit.app')
console.log('========================================')

const getFormatUrl = (applicationId: string) => {
  return `${formatServer}/api/formatter/api/applications/${applicationId}/formatted?skip_medication=true&skip_producer=true`
}

const format_application = async (applicationId: string) => {
  const url = getFormatUrl(applicationId)
  console.log('format_application', url)

  const headers: HeadersInit = {
    'Content-Type': 'application/json'
  }

  // Add Authorization header if API key is available
  if (formatServerApiKey) {
    headers['Authorization'] = `Bearer ${formatServerApiKey}`
  }

  const response = await fetch(url, { headers })
  const data = await response.json()
  return data
}

export const determineStatus = (
  status: string,
  hasCsgApp: boolean,
  hasBooking: boolean,
  csgApp?: { verificationStatus: string }
): string | null => {
  // If there's a CSG app, status depends on verification status
  if (hasCsgApp && csgApp) {
    switch (csgApp.verificationStatus) {
      case 'verified':
        return 'awaiting_signature';
      case 'failed':
        return 'submission_issue';
      case 'pending':
      case 'verifying':
        return 'waiting_review';
      default:
        // If there's a booking, keep it in waiting_review even if CSG status is unknown
        if (hasBooking) return 'waiting_review';
        return null;
    }
  }

  // If there's a booking but no CSG app, it's waiting for review
  if (hasBooking) return 'waiting_review';
  
  // Otherwise use the stored status or return null
  switch (status.toLowerCase()) {
    case 'completed':
      return 'completed';
    case 'review':
      return 'waiting_review';
    case 'submitted_to_csg':
      return 'waiting_review';
    case 'declined':
      return 'declined';
    case 'issued':
      return 'issued';
    case 'awaiting_signature':
      return 'awaiting_signature';
    case 'partial':
      return 'partial';
    default:
      return 'partial';
  }
}

// Database configuration
const client = createClient({
  url: process.env.TURSO_DATABASE_URL!,
  authToken: process.env.TURSO_AUTH_TOKEN!,
})
const db = drizzle(client)

// Add function to check if application is complete
const isApplicationComplete = (data: any): boolean => {
  try {
    // Required sections that must be complete
    const requiredSections = [
      'applicant_info',
      'medicare_information',
      'previous_coverage_information',
      'payment',
      'producer'
    ];

    // Check if all required sections exist
    for (const section of requiredSections) {
      if (!data[section]) {
        return false;
      }
    }

    // Check applicant info completeness
    const applicantInfo = data.applicant_info;
    const requiredApplicantFields = ['f_name', 'l_name', 'dob', 'phone', 'email', 'address_line1', 'city', 'state', 'zip'];
    if (!applicantInfo || !requiredApplicantFields.every(field => applicantInfo[field])) {
      return false;
    }

    // Check Medicare info completeness
    const medicareInfo = data.medicare_information;
    if (!medicareInfo?.medicare_number || !medicareInfo?.part_a_date || !medicareInfo?.part_b_date) {
      return false;
    }

    // Check payment info completeness
    const payment = data.payment;
    if (!payment?.payment_mode || !payment?.payment_method) {
      return false;
    }

    // Check producer info completeness
    const producer = data.producer;
    if (!producer?.agent_reviewed || !producer?.applicant_reviewed) {
      return false;
    }

    return true;
  } catch (error) {
    console.error('Error checking application completeness:', error);
    return false;
  }
};

// Helper function to format application data
export const formatApplicationData = async (rawApplications: any[]) => {
  if (rawApplications.length === 0) return []

  const applicationIds = rawApplications.map(app => app.id)
  const userIds = rawApplications.map(app => app.userId)

  const [relatedBookings, relatedUsers, relatedCsgApps, relatedOnboarding] = await Promise.all([
    db.select().from(bookings).where(sql`application_id IN ${applicationIds}`),
    db.select().from(user).where(sql`id IN ${userIds}`),
    db.select().from(csgApplications).where(sql`application_id IN ${applicationIds}`),
    db.select().from(onboarding).where(sql`user_id IN ${userIds}`)
  ])

  const bookingsByAppId = new Map(relatedBookings.map(booking => [booking.applicationId, booking]))
  const usersById = new Map(relatedUsers.map(user => [user.id, user]))
  const csgAppsByAppId = new Map(relatedCsgApps.map(csgApp => [csgApp.applicationId, csgApp]))
  const onboardingByUserId = new Map(relatedOnboarding.map(onb => [onb.userId, onb]))

  return rawApplications.map(app => {
    const relatedBooking = bookingsByAppId.get(app.id)
    const relatedUser = usersById.get(app.userId)
    const relatedCsgApp = csgAppsByAppId.get(app.id)
    const relatedOnboarding = onboardingByUserId.get(app.userId)

    const safeDate = (dateStr: string | number): string => {
      try {
        return new Date(Number(dateStr)).toISOString()
      } catch {
        return String(dateStr)
      }
    }

    const appData = typeof app.data === 'string' ? JSON.parse(app.data) : app.data;
    const status = determineStatus(
      app.status, 
      !!relatedCsgApp, 
      !!relatedBooking,
      relatedCsgApp
    )

    // Extract contact info from application data
    const applicantInfo = appData?.applicant_info || {};
    const phone = applicantInfo.phone || relatedBooking?.phone || null;
    const email = applicantInfo.email || relatedBooking?.email || relatedUser?.email || null;
    const name = applicantInfo.f_name && applicantInfo.l_name 
      ? `${applicantInfo.f_name} ${applicantInfo.l_name}`.trim()
      : null;

    return {
      id: app.id,
      naic: app.naic,
      name,
      status,
      phone,
      email,
      effectiveDate: appData?.medicare_information?.effective_date || appData?.applicant_info?.effective_date || appData?.effective_date || null,
      dateStarted: safeDate(app.createdAt)
    }
  })
}

// Add a new function to get a single application with schema
export const getApplicationWithSchema = async (applicationId: string) => {
  const db = getDb()
  const [application] = await db
    .select({
      id: applications.id,
      userId: applications.userId,
      status: applications.status,
      createdAt: applications.createdAt,
      data: applications.data,
      formattedData: applications.formattedData,
      name: applications.name,
      naic: applications.naic,
      schema: applications.originalSchema,
      rawMedications: applications.rawMedications
    })
    .from(applications)
    .where(eq(applications.id, applicationId))

  if (!application) {
    return null
  }

  const [relatedUser, relatedOnboarding, relatedCsgApp, relatedBooking] = await Promise.all([
    db.select({ email: user.email }).from(user).where(eq(user.id, application.userId)),
    db.select({ data: onboarding.data }).from(onboarding).where(eq(onboarding.userId, application.userId)),
    db.select({
      key: csgApplications.key,
      brokerEmail: csgApplications.brokerEmail,
      verificationStatus: csgApplications.verificationStatus,
      verificationScreenshot: csgApplications.verificationScreenshot,
      verificationError: csgApplications.verificationError,
      lastVerifiedAt: csgApplications.lastVerifiedAt
    }).from(csgApplications).where(eq(csgApplications.applicationId, applicationId)),
    db.select().from(bookings).where(eq(bookings.applicationId, applicationId))
  ])
  //console.log('csgApp', relatedCsgApp)
  const onboardingData = relatedOnboarding ? (typeof relatedOnboarding.data === 'string' ? JSON.parse(relatedOnboarding.data) : relatedOnboarding.data) : {};

  const appData = typeof application.data === 'string' ? JSON.parse(application.data) : application.data;
  const status = determineStatus(
    application.status, 
    !!relatedCsgApp, 
    !!relatedBooking?.[0],
    relatedCsgApp?.[0]
  )

  const safeDate = (timestamp: number | null): string => {
    if (!timestamp) return new Date().toISOString()
    try {
      return new Date(timestamp * 1000).toISOString()
    } catch {
      return new Date().toISOString()
    }
  }

  const createdAtStr = safeDate(application.createdAt);

  return {
    id: application.id,
    userId: application.userId,
    userEmail: relatedUser?.email || null,
    status,
    state: null,
    data: appData,
    formattedData: application.formattedData,
    name: application.name || 'Unknown',
    naic: application.naic,
    schema: application.schema?.sections,
    rawMedications: application.rawMedications,
    onboarding_data: onboardingData,
    csgApplication: relatedCsgApp ? relatedCsgApp[0] : null
  }
}

export const getFormattedApplicationWithSchema = async (applicationId: string) => {
  const application = await getApplicationWithSchema(applicationId)
  if (!application) {
    return null
  }

  if (application.formattedData) {
    return {
      ...application,
      formattedData: application.formattedData,
      rawMedications: application.rawMedications || [],
    }
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
    formattedData: newData.data,
    rawMedications: application.rawMedications || [],
  }
}

export const getApplications = async (page: number, pageSize: number, searchTerm: string, hasContactFilter: boolean, naics: string[] = [], status?: string) => {
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

  if (status) {
    switch (status) {
      case 'partial':
        whereConditions.push(sql`(
          ${applications.status} = 'partial' OR
          (${applications.status} NOT IN ('completed', 'review', 'submitted_to_csg', 'declined', 'issued', 'awaiting_signature') AND
           NOT EXISTS (SELECT 1 FROM ${bookings} WHERE ${bookings.applicationId} = ${applications.id}) AND
           NOT EXISTS (SELECT 1 FROM ${csgApplications} WHERE ${csgApplications.applicationId} = ${applications.id}))
        )`)
        break;
      case 'awaiting_signature':
        whereConditions.push(sql`(
          ${applications.status} = 'awaiting_signature' OR
          EXISTS (
            SELECT 1 FROM ${csgApplications}
            WHERE ${csgApplications.applicationId} = ${applications.id}
            AND ${csgApplications.verificationStatus} = 'verified'
          )
        )`)
        break;
      case 'waiting_review':
        whereConditions.push(sql`(
          ${applications.status} IN ('review', 'submitted_to_csg') OR
          EXISTS (SELECT 1 FROM ${bookings} WHERE ${bookings.applicationId} = ${applications.id}) OR
          EXISTS (
            SELECT 1 FROM ${csgApplications}
            WHERE ${csgApplications.applicationId} = ${applications.id}
            AND ${csgApplications.verificationStatus} IN ('pending', 'verifying')
          )
        )`)
        break;
      case 'completed':
        whereConditions.push(sql`${applications.status} IN ('completed', 'issued')`)
        break;
    }
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
        naic: applications.naic,
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

  // Fetch related data for status determination
  const applicationIds = results.map(app => app.id)
  const [relatedBookings, relatedCsgApps] = await Promise.all([
    db.select().from(bookings).where(sql`application_id IN ${applicationIds}`),
    db.select().from(csgApplications).where(sql`application_id IN ${applicationIds}`)
  ])

  const bookingsByAppId = new Map(relatedBookings.map(booking => [booking.applicationId, booking]))
  const csgAppsByAppId = new Map(relatedCsgApps.map(csgApp => [csgApp.applicationId, csgApp]))

  const formattedApplications = results.map(app => {
    const relatedBooking = bookingsByAppId.get(app.id)
    const relatedCsgApp = csgAppsByAppId.get(app.id)

    const appData = typeof app.data === 'string' ? JSON.parse(app.data) : app.data;
    const status = determineStatus(
      app.status, 
      !!relatedCsgApp, 
      !!relatedBooking,
      relatedCsgApp
    )

    // Extract contact info from application data
    const applicantInfo = appData?.applicant_info || {};
    const phone = applicantInfo.phone || relatedBooking?.phone || null;
    const email = applicantInfo.email || relatedBooking?.email || null;
    const name = applicantInfo.f_name && applicantInfo.l_name 
      ? `${applicantInfo.f_name} ${applicantInfo.l_name}`.trim()
      : null;

    return {
      id: app.id,
      naic: app.naic,
      name,
      status,
      phone,
      email,
      effectiveDate: appData?.medicare_information?.effective_date || appData?.applicant_info?.effective_date || appData?.effective_date || null,
      dateStarted: new Date(app.createdAt).toISOString()
    }
  })

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
    const now = sql`CURRENT_TIMESTAMP`;
    return await db.transaction(async (tx) => {
        // Update the application data
        await tx.update(applications)
            .set({ 
                formattedData,
                rawMedications: rawMedications || null,
                updatedAt: now
            })
            .where(eq(applications.id, id));
        
        // Reset verification status for any associated CSG application
        await tx.update(csgApplications)
            .set({
                verificationStatus: 'pending',
                verificationScreenshot: null,
                verificationError: null,
                lastVerifiedAt: null,
                updatedAt: now
            })
            .where(eq(csgApplications.applicationId, id));
    });
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

export const getApplicationStats = async () => {
  const db = getDb();
  const thirtyDaysAgo = new Date();
  thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);
  const now = new Date();

  // Get all applications with their CSG and booking status
  const results = await db
    .select({
      id: applications.id,
      status: applications.status,
      csgId: csgApplications.id,
      csgStatus: csgApplications.verificationStatus,
      bookingId: bookings.id
    })
    .from(applications)
    .leftJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
    .leftJoin(bookings, eq(applications.id, bookings.applicationId));

  console.log('Date range:', {
    from: thirtyDaysAgo.toISOString(),
    to: now.toISOString(),
    fromTimestamp: Math.floor(thirtyDaysAgo.getTime() / 1000),
    toTimestamp: Math.floor(now.getTime() / 1000)
  });

  // Use the determineStatus function to count each status
  let totalCount = 0;
  let submittedCount = 0;
  let waitingReviewCount = 0;
  let completedCount = 0;

  results.forEach(app => {
    totalCount++;
    const status = determineStatus(
      app.status,
      !!app.csgId,
      !!app.bookingId,
      app.csgId ? { verificationStatus: app.csgStatus || '' } : undefined
    );

    switch (status) {
      case 'awaiting_signature':
      case 'submitted_to_csg':
        submittedCount++;
        break;
      case 'waiting_review':
      case 'verifying':
        waitingReviewCount++;
        break;
      case 'completed':
      case 'issued':
        completedCount++;
        break;
    }
  });

  const stats = {
    total: totalCount,
    submitted: submittedCount,
    waitingReview: waitingReviewCount,
    completed: completedCount
  };

  console.log('Stats query results:', stats);

  return stats;
};

// Booking functions
export const createBooking = async (bookingData: {
  userId?: string;
  applicationId?: string;
  email: string;
  phone?: string;
  url: string;
  event?: string;
  status: string;
  data?: Record<string, any>;
}) => {
  const db = getDb();
  const id = crypto.randomUUID();

  await db.insert(bookings).values({
    id,
    userId: bookingData.userId || null,
    applicationId: bookingData.applicationId || null,
    email: bookingData.email,
    phone: bookingData.phone || null,
    url: bookingData.url,
    event: bookingData.event || null,
    status: bookingData.status,
    data: bookingData.data ? JSON.stringify(bookingData.data) : null,
    hubspotSyncStatus: 'pending',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  });

  return { id, ...bookingData };
};

export const getBookings = async (page: number, pageSize: number, searchTerm: string = '', statusFilter?: string) => {
  const db = getDb();
  const offset = page * pageSize;
  const searchPattern = `%${searchTerm.toLowerCase()}%`;
  const shouldSearch = searchTerm.length >= 3;

  // Build WHERE conditions
  const whereConditions = [];

  if (statusFilter) {
    whereConditions.push(sql`${bookings.hubspotSyncStatus} = ${statusFilter}`);
  }

  if (shouldSearch) {
    whereConditions.push(sql`(
      LOWER(${bookings.email}) LIKE ${searchPattern} OR
      LOWER(${bookings.phone}) LIKE ${searchPattern}
    )`);
  }

  const whereClause = whereConditions.length > 0
    ? sql`WHERE ${sql.join(whereConditions, sql` AND `)}`
    : sql``;

  // Execute queries with joins
  const [results, totalCount] = await Promise.all([
    db
      .select({
        id: bookings.id,
        userId: bookings.userId,
        applicationId: bookings.applicationId,
        email: bookings.email,
        phone: bookings.phone,
        url: bookings.url,
        event: bookings.event,
        status: bookings.status,
        data: bookings.data,
        hubspotContactId: bookings.hubspotContactId,
        hubspotSyncStatus: bookings.hubspotSyncStatus,
        hubspotLastSyncedAt: bookings.hubspotLastSyncedAt,
        hubspotSyncError: bookings.hubspotSyncError,
        createdAt: bookings.createdAt,
        updatedAt: bookings.updatedAt,
      })
      .from(bookings)
      .where(whereClause)
      .orderBy(desc(bookings.createdAt))
      .limit(pageSize)
      .offset(offset),
    db
      .select({ count: sql<number>`cast(count(*) as integer)` })
      .from(bookings)
      .where(whereClause)
      .then(rows => rows[0]?.count || 0)
  ]);

  // Fetch related data for each booking
  const applicationIds = results
    .map(r => r.applicationId)
    .filter((id): id is string => id !== null);

  const [relatedApplications, relatedUsers] = await Promise.all([
    applicationIds.length > 0
      ? db
          .select({
            id: applications.id,
            name: applications.name,
            naic: applications.naic,
            data: applications.data,
          })
          .from(applications)
          .where(sql`${applications.id} IN ${applicationIds}`)
      : [],
    db
      .select({
        id: user.id,
        email: user.email,
      })
      .from(user)
      .where(sql`${user.id} IN ${results.map(r => r.userId).filter(Boolean)}`)
  ]);

  const appMap = new Map(relatedApplications.map(app => [app.id, app]));
  const userMap = new Map(relatedUsers.map(u => [u.id, u]));

  const enrichedResults = results.map(booking => ({
    ...booking,
    application: booking.applicationId ? appMap.get(booking.applicationId) : null,
    user: booking.userId ? userMap.get(booking.userId) : null,
  }));

  return {
    bookings: enrichedResults,
    totalCount,
    page,
    pageSize,
    totalPages: Math.ceil(totalCount / pageSize),
  };
};

export const getBookingWithContext = async (bookingId: string) => {
  const db = getDb();

  const [booking] = await db
    .select()
    .from(bookings)
    .where(eq(bookings.id, bookingId));

  if (!booking) {
    return null;
  }

  // Fetch related application and user
  const [relatedApplication, relatedUser] = await Promise.all([
    booking.applicationId
      ? db
          .select({
            id: applications.id,
            name: applications.name,
            naic: applications.naic,
            data: applications.data,
            formattedData: applications.formattedData,
          })
          .from(applications)
          .where(eq(applications.id, booking.applicationId))
          .then(rows => rows[0] || null)
      : null,
    booking.userId
      ? db
          .select({
            id: user.id,
            email: user.email,
          })
          .from(user)
          .where(eq(user.id, booking.userId))
          .then(rows => rows[0] || null)
      : null,
  ]);

  // Merge booking data with application data if available
  const mergedData = {
    ...booking,
    data: booking.data
      ? typeof booking.data === 'string'
        ? JSON.parse(booking.data)
        : booking.data
      : {},
    application: relatedApplication,
    user: relatedUser,
  };

  return mergedData;
};

export const exportBookings = async (searchTerm: string = '') => {
  const db = getDb();
  const searchPattern = `%${searchTerm.toLowerCase()}%`;
  const shouldSearch = searchTerm.length >= 3;

  const whereClause = shouldSearch
    ? sql`WHERE (
        LOWER(${bookings.email}) LIKE ${searchPattern} OR
        LOWER(${bookings.phone}) LIKE ${searchPattern}
      )`
    : sql``;

  const results = await db
    .select()
    .from(bookings)
    .where(whereClause)
    .orderBy(desc(bookings.createdAt));

  return results;
};

export const determineBookingStatus = (booking: {
  status: string;
  hubspotSyncStatus: string | null;
  hubspotContactId: string | null;
}) => {
  if (booking.hubspotContactId) {
    return 'synced';
  }

  switch (booking.hubspotSyncStatus) {
    case 'syncing':
      return 'syncing';
    case 'failed':
      return 'sync_failed';
    case 'pending':
    default:
      return booking.status || 'pending';
  }
};