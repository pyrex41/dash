import { getDb } from '../db';
import { applications, csgApplications, producers } from '../db/schema';
import { eq } from 'drizzle-orm';
import { getToken, getQuoteToken, makeCSGRequest, handleTokenError } from './token';
import axios from 'axios';

interface QuoteRequest {
  effective_date: string;
  age: number;
  zip5: string;
  county?: string;
  gender: string;
  tobacco: number;
  plan: string;
  naic?: string;
  select?: number;
}

interface CSGApplication {
  key: string;
  tracking?: {
    application_id?: string;
  };
  [key: string]: any;
}

// Validate environment variables
function validateConfig() {
  if (!process.env.CSG_API_URL) {
    throw new Error('CSG_API_URL environment variable is not set');
  }
  
  try {
    new URL(process.env.CSG_API_URL);
  } catch (error) {
    throw new Error('CSG_API_URL is not a valid URL');
  }
}

export async function getHeaders() {
  return {
    'x-api-token': await getToken(),
    'Content-Type': 'application/json',
  };
}

async function getQuoteHeaders() {
  return {
    'x-api-token': await getQuoteToken(),
    'Content-Type': 'application/json',
  };
}

function calculateAge(dob: string, asOfDate: string): number {
  const today = new Date(asOfDate);
  const birthDate = new Date(dob);
  let age = today.getFullYear() - birthDate.getFullYear();
  const monthDiff = today.getMonth() - birthDate.getMonth();
  if (monthDiff < 0 || (monthDiff === 0 && today.getDate() < birthDate.getDate())) {
    age--;
  }
  return age;
}

function carrierFromNaic(naic: string): string {
  switch (naic) {
    case '20699':
      return 'Chubb';
    case '72052':
    case '78700': 
    case '68500':
      return 'Aetna';
    case '79413':
      return 'UnitedHealthcare';
    case '82538':
    case '60534':
      return 'Allstate';
    default:
      throw new Error('Invalid NAIC code');
  }
}

async function getCarrierAssignedIdentifier(producerId: number, naic: string) {
  const carrier = carrierFromNaic(naic);
  if (!carrier) {
    throw new Error('Carrier not found');
  }
  const db = getDb();
  const [producer] = await db
    .select()
    .from(producers)
    .where(eq(producers.id, producerId));

  if (!producer) {
    throw new Error('Producer not found');
  }

  const writingNumbers = producer.writingNumbers as Record<string, string>;
  switch (carrier) {
    case 'Chubb':
    case 'Allstate':
      return writingNumbers[carrier];
    case 'Aetna':
    case 'UnitedHealthcare':
      return producer.npn;
    default:
      throw new Error('Invalid carrier');
  }
}

async function findApplicationByTrackingId(applicationId: string): Promise<CSGApplication | null> {
  try {
    console.log('Attempting to find application with tracking ID:', applicationId);
    
    // Fetch recent applications with a larger limit
    const response = await makeCSGRequest<CSGApplication[]>({
      method: 'GET',
      url: '/v1/e_app/enrollment_applications.json',
      params: { limit: 10 } // Fetch last 10 applications to ensure we catch it
    });

    console.log('Fetched', response.length, 'recent applications');
    
    // Find the application with matching tracking ID
    const matchingApp = response.find(app => {
      const matches = app.values?.tracking?.application_id === applicationId;
      if (matches) {
        console.log('Found matching application:', app.key);
      }
      return matches;
    });

    if (!matchingApp) {
      console.log('No matching application found. First few applications:', 
        response.slice(0, 3).map(app => ({
          key: app.key,
          tracking: app.tracking,
          created_date: app.created_date
        }))
      );
    }

    return matchingApp || null;
  } catch (error) {
    console.error('Error finding application by tracking ID:', error);
    return null;
  }
}

export async function submitToCSG(applicationId: string, producerId: number, forceQuote: boolean = false): Promise<any> {
  try {
    // Validate configuration
    validateConfig();

    const db = getDb();
    const [application] = await db
    .select()
    .from(applications)
    .where(eq(applications.id, applicationId));
    
    if (!application || !application.naic) {
      throw new Error('Application not found');
    }
    const carrierAssignedIdentifier = await getCarrierAssignedIdentifier(producerId, application.naic);

    // Check for existing CSG application
    const [existingCsg] = await db
      .select()
      .from(csgApplications)
      .where(eq(csgApplications.applicationId, applicationId));

    const now = new Date();

    // If there's an existing submission, update lastSubmittedAt
    if (existingCsg) {
      await db.update(csgApplications)
        .set({
          lastSubmittedAt: now,
          verificationStatus: 'pending',
          updatedAt: now
        })
        .where(eq(csgApplications.id, existingCsg.id));
    }

    const applicationHeaders = await getHeaders();
    console.log('Application Headers:', applicationHeaders); 
    const formattedData = application.formattedData;
    if (!formattedData) {
      throw new Error('Formatted data not found');
    }
    console.log('Formatted Data applicant info:', formattedData?.applicant_info);
    
    if (!application.naic) {
      throw new Error('NAIC is required for CSG submission');
    }
    
    // For carriers requiring quotes first (UHC, Allstate)
    if (['79413', '60534', '82538'].includes(application.naic) || forceQuote) {
      const quoteHeaders = await getQuoteHeaders();
      console.log('Quote Headers:', quoteHeaders); 
      console.log('Preparing CSG quote request for NAIC:', application.naic);

      const calculatedAge = calculateAge(formattedData.applicant_info.applicant_dob, formattedData.applicant_info.effective_date);
      
      const quoteRequest: QuoteRequest = {
        effective_date: formattedData.applicant_info.effective_date,
        age: calculatedAge,
        zip5: formattedData.applicant_info.zip5,
        gender: formattedData.applicant_info.gender.toLowerCase().includes('f') ? 'F' : 'M',
        tobacco: formattedData.applicant_info.tobacco_usage ? 1 : 0,
        plan: formattedData.applicant_info.applicant_plan,
        naic: application.naic,
        county: formattedData.applicant_info.county
      };
      const params = {
        effective_date: quoteRequest.effective_date,
        age: quoteRequest.age.toString(),
        zip5: quoteRequest.zip5,
        gender: quoteRequest.gender,
        tobacco: quoteRequest.tobacco.toString(),
        plan: quoteRequest.plan,
        ...(quoteRequest.naic && { naic: quoteRequest.naic }),
        ...(quoteRequest.county != null && quoteRequest.county !== undefined && { county: quoteRequest.county })
      };
      console.log('Quote Request:', params);

      const quoteUrl = new URL('/v1/med_supp/quotes.json', process.env.CSG_API_URL).toString();
      
      try {
        const quoteResponse = await axios.get(quoteUrl, {
          headers: applicationHeaders,
          params: params
        });

        const quoteData = quoteResponse.data;
        console.log('quote response status', quoteResponse.status);
        const logKey = quoteResponse.headers['csg-log-key'] || '';
        console.log('log key', logKey);
        
        // Find standard quote
        const standardQuote = quoteData.find((quote: any) => 
          application.naic === '79413' ? 
            (quote.rating_class.toLowerCase().includes('standard') && 
             !quote.rating_class.toLowerCase().includes('household')) :
            quote.rating_class === ''
        );

        if (!standardQuote) {
          throw new Error('No standard quote found');
        }

        // Submit application with quote
        const submitUrl = new URL('/v1/e_app/enrollment_applications.json', process.env.CSG_API_URL).toString();
        console.log('submitting application to', submitUrl);
        const trimmedData = { ...formattedData };
        delete trimmedData.enrollment_application;
        const payload = { 
          //tool_name: 'med_supp_tool',
          //company_identifier: application.naic || '',
          logging_key: logKey,
          quote_key: standardQuote.key,
          desired_underwriting_type: application.underwritingType || 0,
          underwriting_type: application.underwritingType || 0,
          carrier_assigned_identifier: carrierAssignedIdentifier,
          broker_email: "josh@enlightnu.com",
          auxiliary_values: [],
          values: {
            ...trimmedData,
            tracking: {
              application_id: applicationId
            }
          },
        }
        console.log('payload', payload);
        const response = await axios.post(submitUrl, payload, {
          headers: applicationHeaders
        });

        const responseData = response.data;

        // Update database
        await Promise.all([
          db.update(applications)
            .set({       
              status: 'submitted_to_csg',
              updatedAt: now
            })
            .where(eq(applications.id, applicationId)),

          existingCsg ? 
            db.update(csgApplications)
              .set({
                key: responseData.key,
                responseBody: JSON.stringify(responseData),
                lastSubmittedAt: now,
                verificationStatus: 'pending',
                updatedAt: now
              })
              .where(eq(csgApplications.id, existingCsg.id)) :
            db.insert(csgApplications).values({
              applicationId,
              key: responseData.key,
              responseBody: JSON.stringify(responseData),
              lastSubmittedAt: now,
              verificationStatus: 'pending',
              createdAt: now,
              updatedAt: now,
            })
        ]);

        return responseData;
      } catch (error) {
        if (axios.isAxiosError(error)) {
          console.error('CSG Error Response:', {
            status: error.response?.status,
            statusText: error.response?.statusText,
            headers: error.response?.headers,
            data: error.response?.data
          });

          // Handle token invalidation
          if (error.response?.status === 403) {
            console.log('Token invalidated, retrying with new token...');
            const newToken = await handleTokenError(error, applicationHeaders['x-api-token'], false);
            applicationHeaders['x-api-token'] = newToken;
            // Retry the request with new token
            return submitToCSG(applicationId, producerId, forceQuote);
          }

          // If we got a 500 error, try to recover the application
          if (error.response?.status === 500) {
            console.log('Got 500 error, attempting to recover application...');
            const recoveredApp = await findApplicationByTrackingId(applicationId);
            
            if (recoveredApp) {
              console.log('Successfully recovered application:', recoveredApp.key);
              
              // Update database with recovered application
              await Promise.all([
                db.update(applications)
                  .set({       
                    status: 'submitted_to_csg',
                    updatedAt: now
                  })
                  .where(eq(applications.id, applicationId)),

                existingCsg ?
                  db.update(csgApplications)
                    .set({
                      key: recoveredApp.key,
                      responseBody: JSON.stringify(recoveredApp),
                      lastSubmittedAt: now,
                      verificationStatus: 'pending',
                      updatedAt: now
                    })
                    .where(eq(csgApplications.id, existingCsg.id)) :
                  db.insert(csgApplications).values({
                    applicationId,
                    key: recoveredApp.key,
                    responseBody: JSON.stringify(recoveredApp),
                    lastSubmittedAt: now,
                    verificationStatus: 'pending',
                    createdAt: now,
                    updatedAt: now,
                  })
              ]);

              return recoveredApp;
            }
          }
          throw new Error(`Failed to interact with CSG: ${error.message}`);
        }
        throw error;
      }
    }

    // Direct submission for other carriers
    try {
      const submitUrl = new URL('/v1/e_app/enrollment_applications.json', process.env.CSG_API_URL).toString();
      
      const response = await axios.post(submitUrl, {
        tool_name: 'med_supp_tool',
        company_identifier: application.naic,
        desired_underwriting_type: application.underwritingType || 0,
        carrier_assigned_identifier: carrierAssignedIdentifier,
        values: {
          ...formattedData,
          tracking: {
            application_id: applicationId
          }
        },
      }, {
        headers: applicationHeaders
      });

      const responseData = response.data;

      // Update database
      await Promise.all([
        db.update(applications)
          .set({ 
            status: 'submitted_to_csg',
            updatedAt: now
          })
          .where(eq(applications.id, applicationId)),

        existingCsg ?
          db.update(csgApplications)
            .set({
              key: responseData.key,
              responseBody: JSON.stringify(responseData),
              lastSubmittedAt: now,
              verificationStatus: 'pending',
              updatedAt: now
            })
            .where(eq(csgApplications.id, existingCsg.id)) :
          db.insert(csgApplications).values({
            applicationId,
            key: responseData.key,
            responseBody: JSON.stringify(responseData),
            lastSubmittedAt: now,
            verificationStatus: 'pending',
            createdAt: now,
            updatedAt: now,
          })
      ]);

      return responseData;
    } catch (error) {
      if (axios.isAxiosError(error)) {
        console.error('CSG Error Response:', {
          status: error.response?.status,
          statusText: error.response?.statusText,
          headers: error.response?.headers,
          data: error.response?.data
        });

        // Handle token invalidation
        if (error.response?.status === 403) {
          console.log('Token invalidated, retrying with new token...');
          const newToken = await handleTokenError(error, applicationHeaders['x-api-token'], false);
          applicationHeaders['x-api-token'] = newToken;
          // Retry the request with new token
          return submitToCSG(applicationId, producerId, forceQuote);
        }
      }
      // If direct submission fails, try quote method as fallback
      return submitToCSG(applicationId, producerId, true);
    }
  } catch (error) {
    // Log the error for debugging
    console.error('Error in submitToCSG:', error);
    throw error;
  }
}