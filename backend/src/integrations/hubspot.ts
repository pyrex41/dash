// Environment variables are loaded by backend/src/index.ts
const HUBSPOT_API_KEY = process.env.HUBSPOT_API_KEY;
const HUBSPOT_BASE_URL = 'https://api.hubapi.com';

interface HubSpotContact {
  properties: {
    email: string;
    firstname?: string;
    lastname?: string;
    phone?: string;
    [key: string]: any;
  };
}

interface HubSpotContactResponse {
  id: string;
  properties: Record<string, any>;
  createdAt: string;
  updatedAt: string;
}

interface RateLimiter {
  queue: Array<() => Promise<any>>;
  processing: boolean;
  lastRequestTime: number;
  requestCount: number;
}

export class HubSpotClient {
  private apiKey: string;
  private baseUrl: string;
  private rateLimiter: RateLimiter;
  private readonly maxRequestsPer10Seconds = 10;
  private readonly windowMs = 10000;

  constructor() {
    if (!HUBSPOT_API_KEY) {
      console.warn('[HubSpot] HUBSPOT_API_KEY not set - HubSpot sync will be disabled');
      this.apiKey = '';
    } else {
      this.apiKey = HUBSPOT_API_KEY;
    }
    this.baseUrl = HUBSPOT_BASE_URL;
    this.rateLimiter = {
      queue: [],
      processing: false,
      lastRequestTime: 0,
      requestCount: 0,
    };
  }

  private async executeWithRateLimit<T>(fn: () => Promise<T>): Promise<T> {
    return new Promise((resolve, reject) => {
      this.rateLimiter.queue.push(async () => {
        try {
          const result = await fn();
          resolve(result);
        } catch (error) {
          reject(error);
        }
      });

      this.processQueue();
    });
  }

  private async processQueue(): Promise<void> {
    if (this.rateLimiter.processing || this.rateLimiter.queue.length === 0) {
      return;
    }

    this.rateLimiter.processing = true;

    while (this.rateLimiter.queue.length > 0) {
      const now = Date.now();
      const timeSinceLastWindow = now - this.rateLimiter.lastRequestTime;

      // Reset counter if window has passed
      if (timeSinceLastWindow >= this.windowMs) {
        this.rateLimiter.requestCount = 0;
        this.rateLimiter.lastRequestTime = now;
      }

      // Wait if we've hit the rate limit
      if (this.rateLimiter.requestCount >= this.maxRequestsPer10Seconds) {
        const waitTime = this.windowMs - timeSinceLastWindow;
        await new Promise(resolve => setTimeout(resolve, waitTime));
        this.rateLimiter.requestCount = 0;
        this.rateLimiter.lastRequestTime = Date.now();
      }

      const task = this.rateLimiter.queue.shift();
      if (task) {
        await task();
        this.rateLimiter.requestCount++;
      }
    }

    this.rateLimiter.processing = false;
  }

  private async makeRequest<T>(
    endpoint: string,
    method: 'GET' | 'POST' | 'PATCH' | 'DELETE',
    body?: any
  ): Promise<T> {
    return this.executeWithRateLimit(async () => {
      const url = `${this.baseUrl}${endpoint}`;
      const headers: HeadersInit = {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.apiKey}`,  // HubSpot Private Apps use Bearer token format
      };

      const options: RequestInit = {
        method,
        headers,
      };

      if (body && (method === 'POST' || method === 'PATCH')) {
        options.body = JSON.stringify(body);
      }

      console.log(`[HubSpot] ${method} ${url}`);

      const response = await fetch(url, options);

      if (!response.ok) {
        const errorText = await response.text();
        throw new Error(
          `HubSpot API error (${response.status}): ${errorText}`
        );
      }

      return response.json() as Promise<T>;
    });
  }

  async searchContactByEmail(email: string): Promise<HubSpotContactResponse | null> {
    try {
      const searchBody = {
        filterGroups: [
          {
            filters: [
              {
                propertyName: 'email',
                operator: 'EQ',
                value: email,
              },
            ],
          },
        ],
      };

      console.log('[HubSpot] Search body:', JSON.stringify(searchBody));

      const result = await this.makeRequest<{
        results: HubSpotContactResponse[];
      }>(
        '/crm/v3/objects/contacts/search',
        'POST',
        searchBody
      );

      return result.results?.[0] || null;
    } catch (error) {
      console.error('[HubSpot] Error searching contact by email:', error);
      throw error;
    }
  }

  async createContact(contactData: HubSpotContact): Promise<HubSpotContactResponse> {
    try {
      return await this.makeRequest<HubSpotContactResponse>(
        '/crm/v3/objects/contacts',
        'POST',
        contactData
      );
    } catch (error) {
      console.error('[HubSpot] Error creating contact:', error);
      throw error;
    }
  }

  async updateContact(
    contactId: string,
    properties: Record<string, any>
  ): Promise<HubSpotContactResponse> {
    try {
      return await this.makeRequest<HubSpotContactResponse>(
        `/crm/v3/objects/contacts/${contactId}`,
        'PATCH',
        { properties }
      );
    } catch (error) {
      console.error('[HubSpot] Error updating contact:', error);
      throw error;
    }
  }

  async createOrUpdateContact(
    email: string,
    properties: Record<string, any>
  ): Promise<{ contact: HubSpotContactResponse; created: boolean; contactId?: string }> {
    if (!this.apiKey) {
      throw new Error('HubSpot API key not configured - sync disabled');
    }

    try {
      // Search for existing contact - pass email string, NOT properties object
      const existingContact = await this.searchContactByEmail(email);

      if (existingContact) {
        // Update existing contact - ONLY update booking_json_data field
        console.log(`[HubSpot] Updating existing contact: ${existingContact.id} (booking_json_data only)`);
        const updateProperties: Record<string, any> = {};

        // Only include booking_json_data for existing contacts
        if (properties.booking_json_data) {
          updateProperties.booking_json_data = properties.booking_json_data;
        }

        const updated = await this.updateContact(existingContact.id, updateProperties);
        return { contact: updated, created: false, contactId: existingContact.id };
      } else {
        // Create new contact - use all available fields
        console.log(`[HubSpot] Creating new contact for: ${email}`);
        try {
          const created = await this.createContact({
            properties: { ...properties, email }, // email last to ensure it's not overwritten
          });
          return { contact: created, created: true, contactId: created.id };
        } catch (createError: any) {
          // Handle race condition: contact was created between search and create
          if (createError?.message?.includes('409') || createError?.message?.includes('Contact already exists')) {
            console.log(`[HubSpot] Contact was created concurrently, retrying as update for: ${email}`);
            // Extract contact ID from error message if available
            const match = createError.message.match(/Existing ID: (\d+)/);
            if (match) {
              const contactId = match[1];
              const updateProperties: Record<string, any> = {};
              if (properties.booking_json_data) {
                updateProperties.booking_json_data = properties.booking_json_data;
              }
              const updated = await this.updateContact(contactId, updateProperties);
              return { contact: updated, created: false, contactId };
            }
            // If we can't extract ID, search again
            const retryContact = await this.searchContactByEmail(email);
            if (retryContact) {
              const updateProperties: Record<string, any> = {};
              if (properties.booking_json_data) {
                updateProperties.booking_json_data = properties.booking_json_data;
              }
              const updated = await this.updateContact(retryContact.id, updateProperties);
              return { contact: updated, created: false, contactId: retryContact.id };
            }
          }
          throw createError;
        }
      }
    } catch (error) {
      console.error('[HubSpot] Error in createOrUpdateContact:', error);
      throw error;
    }
  }

  /**
   * Batch upsert contacts using HubSpot's batch upsert API
   * This is MUCH more efficient than individual creates/updates
   * Uses email as the idProperty to identify contacts
   */
  async batchUpsertContacts(
    contacts: Array<{ email: string; properties: Record<string, any> }>
  ): Promise<{
    results: Array<{ id: string; email: string; created: boolean }>;
    errors: Array<{ email: string; error: string }>;
  }> {
    if (!this.apiKey) {
      throw new Error('HubSpot API key not configured - sync disabled');
    }

    const results: Array<{ id: string; email: string; created: boolean }> = [];
    const errors: Array<{ email: string; error: string }> = [];

    // Format contacts for batch upsert API
    const inputs = contacts.map(({ email, properties }) => ({
      id: email,
      idProperty: 'email',
      properties: properties, // All properties including email, booking_json_data, etc.
    }));

    try {
      const response = await this.makeRequest<any>(
        '/crm/v3/objects/contacts/batch/upsert',
        'POST',
        { inputs }
      );

      // Process results
      // NOTE: HubSpot batch API response order may NOT match input order!
      // We must match results back to inputs by email address
      for (const result of response.results) {
        // Extract email from the response properties (HubSpot returns it)
        const resultEmail = result.properties.email;

        // Check if contact was newly created (no previous properties) or updated
        const wasCreated = !result.properties.hs_object_id; // Simple heuristic

        results.push({
          id: result.id,
          email: resultEmail,
          created: wasCreated,
        });
      }

      // Process errors if any
      if (response.errors && response.errors.length > 0) {
        for (const error of response.errors) {
          const failedInput = inputs[error.index || 0];
          errors.push({
            email: failedInput?.id || 'unknown',
            error: error.message || JSON.stringify(error),
          });
        }
      }

      console.log(`[HubSpot Batch] Upserted ${results.length} contacts, ${errors.length} errors`);

      return { results, errors };
    } catch (error: any) {
      console.error('[HubSpot Batch] Error in batchUpsertContacts:', error);
      throw error;
    }
  }
}

// Data transformation utilities
export interface BookingData {
  id: string;
  email: string;
  phone?: string;
  data?: Record<string, any>;
  application?: {
    data?: Record<string, any>;
    name?: string;
  };
  user?: {
    email?: string;
  };
}

export function transformBookingToHubSpot(booking: BookingData): Record<string, any> {
  // Smart fallback hierarchy: booking.data → application.data → booking fields
  const bookingData = booking.data || {};
  const applicationData = booking.application?.data || {};

  // Extract applicant info with fallback
  const applicantInfo = bookingData.applicant_info || applicationData.applicant_info || {};

  const properties: Record<string, any> = {
    email: booking.email, // Required field
  };

  // Add optional fields with fallback hierarchy
  if (applicantInfo.f_name) {
    properties.firstname = applicantInfo.f_name;
  }

  if (applicantInfo.l_name) {
    properties.lastname = applicantInfo.l_name;
  }

  if (booking.phone || applicantInfo.phone) {
    properties.phone = booking.phone || applicantInfo.phone;
  }

  if (applicantInfo.dob) {
    properties.date_of_birth = applicantInfo.dob;
  }

  if (applicantInfo.zip) {
    properties.zip = applicantInfo.zip;
  }

  if (applicantInfo.city) {
    properties.city = applicantInfo.city;
  }

  if (applicantInfo.state) {
    properties.state = applicantInfo.state;
  }

  // Use part_b_effective_date from booking or application data
  const partBEffectiveDate = bookingData.part_b_effective_date || applicationData.part_b_effective_date;
  if (partBEffectiveDate) {
    properties.part_b_effective_date = partBEffectiveDate;
  }

  // Add complete booking data as formatted JSON for reference
  if (booking.data || booking.application?.data) {
    const completeData = {
      booking: booking.data || {},
      application: booking.application?.data || {},
      metadata: {
        bookingId: booking.id,
        email: booking.email,
        phone: booking.phone,
        syncedAt: new Date().toISOString()
      }
    };

    // Format as human-readable JSON with 2-space indentation
    properties.booking_json_data = JSON.stringify(completeData, null, 2);
  }

  console.log(`[HubSpot Transform] Extracted ${Object.keys(properties).length} properties from booking ${booking.id}`);
  console.log('[HubSpot Transform] Properties:', JSON.stringify(properties, null, 2).substring(0, 500));

  return properties;
}

export function handleHubSpotError(error: any): {
  status: 'failed';
  error: string;
} {
  let errorMessage = 'Unknown HubSpot error';

  if (error instanceof Error) {
    errorMessage = error.message;
  } else if (typeof error === 'string') {
    errorMessage = error;
  }

  console.error('[HubSpot] Error:', errorMessage);

  return {
    status: 'failed',
    error: errorMessage,
  };
}
