/**
 * Automatic HubSpot Sync Scheduler
 *
 * Runs on a configurable interval to automatically sync pending bookings to HubSpot.
 * Since the app doesn't receive Calendly webhooks, this scheduler polls the database
 * for bookings with hubspotSyncStatus='pending' and syncs them automatically.
 */

import { getDb } from '../db';
import { bookings } from '../db/schema';
import { eq, or, and, lt, isNull } from 'drizzle-orm';
import { HubSpotClient, transformBookingToHubSpot } from '../integrations/hubspot';
import { getBookingWithContext } from '../db/query';

const db = getDb();

interface SyncSchedulerConfig {
  intervalMs: number;
  maxRetries: number;
  retryDelayMs: number;
  batchSize: number;
}

interface SyncResult {
  bookingId: string;
  success: boolean;
  hubspotContactId?: string;
  error?: string;
  created?: boolean;
}

interface SyncStats {
  startTime: Date;
  endTime: Date;
  totalProcessed: number;
  successCount: number;
  failureCount: number;
  errors: Array<{ bookingId: string; error: string }>;
}

export class SyncScheduler {
  private intervalId: NodeJS.Timeout | null = null;
  private isRunning = false;
  private hubspotClient: HubSpotClient;
  private config: SyncSchedulerConfig;
  private lastSyncTime: Date | null = null;
  private syncStats: SyncStats | null = null;

  constructor(config?: Partial<SyncSchedulerConfig>) {
    this.config = {
      intervalMs: config?.intervalMs ?? 5 * 60 * 1000, // Default: 5 minutes
      maxRetries: config?.maxRetries ?? 3,
      retryDelayMs: config?.retryDelayMs ?? 60 * 1000, // 1 minute
      batchSize: config?.batchSize ?? 10,
    };
    this.hubspotClient = new HubSpotClient();
  }

  /**
   * Start the automatic sync scheduler
   */
  start(): void {
    if (this.intervalId) {
      console.log('[SyncScheduler] Scheduler already running');
      return;
    }

    console.log(`[SyncScheduler] Starting automatic sync scheduler (interval: ${this.config.intervalMs}ms)`);

    // Run immediately on start
    this.runSyncCycle().catch(error => {
      console.error('[SyncScheduler] Error in initial sync cycle:', error);
    });

    // Then run on interval
    this.intervalId = setInterval(() => {
      this.runSyncCycle().catch(error => {
        console.error('[SyncScheduler] Error in sync cycle:', error);
      });
    }, this.config.intervalMs);
  }

  /**
   * Stop the automatic sync scheduler
   */
  stop(): void {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      console.log('[SyncScheduler] Stopped automatic sync scheduler');
    }
  }

  /**
   * Get the last sync time and stats
   */
  getStatus() {
    return {
      isRunning: this.intervalId !== null,
      intervalMs: this.config.intervalMs,
      lastSyncTime: this.lastSyncTime,
      lastSyncStats: this.syncStats,
      nextSyncIn: this.lastSyncTime
        ? Math.max(0, this.config.intervalMs - (Date.now() - this.lastSyncTime.getTime()))
        : 0,
    };
  }

  /**
   * Run a single sync cycle
   */
  private async runSyncCycle(forceAll = false): Promise<void> {
    if (this.isRunning) {
      console.log('[SyncScheduler] Sync cycle already in progress, skipping');
      return;
    }

    this.isRunning = true;
    const startTime = new Date();
    const stats: SyncStats = {
      startTime,
      endTime: startTime,
      totalProcessed: 0,
      successCount: 0,
      failureCount: 0,
      errors: [],
    };

    try {
      console.log(forceAll ? '[SyncScheduler] Starting FULL sync cycle...' : '[SyncScheduler] Starting sync cycle...');

      // Find pending bookings and failed bookings ready for retry
      const pendingBookings = await this.findBookingsToSync(forceAll);

      if (pendingBookings.length === 0) {
        console.log('[SyncScheduler] No bookings to sync');
        return;
      }

      console.log(`[SyncScheduler] Found ${pendingBookings.length} bookings to sync`);
      stats.totalProcessed = pendingBookings.length;

      // Process bookings in batches to respect rate limits
      for (let i = 0; i < pendingBookings.length; i += this.config.batchSize) {
        const batch = pendingBookings.slice(i, i + this.config.batchSize);
        const results = await this.syncBatch(batch);

        // Update stats
        for (const result of results) {
          if (result.success) {
            stats.successCount++;
          } else {
            stats.failureCount++;
            stats.errors.push({
              bookingId: result.bookingId,
              error: result.error || 'Unknown error',
            });
          }
        }

        // Small delay between batches
        if (i + this.config.batchSize < pendingBookings.length) {
          await new Promise(resolve => setTimeout(resolve, 1000));
        }
      }

      stats.endTime = new Date();
      this.syncStats = stats;
      this.lastSyncTime = new Date();

      console.log(`[SyncScheduler] Sync cycle complete: ${stats.successCount}/${stats.totalProcessed} successful`);

      if (stats.errors.length > 0) {
        console.log(`[SyncScheduler] Errors:`, stats.errors);
      }

    } catch (error) {
      console.error('[SyncScheduler] Error in sync cycle:', error);
      stats.endTime = new Date();
      this.syncStats = stats;
    } finally {
      this.isRunning = false;
    }
  }

  /**
   * Find bookings that need syncing
   */
  private async findBookingsToSync(forceAll = false): Promise<Array<{ id: string }>> {
    try {
      if (forceAll) {
        // Force re-sync of all bookings (no status filter)
        console.log('[SyncScheduler] Force sync: fetching ALL bookings');
        const results = await db
          .select({ id: bookings.id })
          .from(bookings);

        return results;
      }

      // Find bookings with:
      // 1. hubspotSyncStatus = 'pending' OR
      // 2. hubspotSyncStatus = 'failed' AND (hubspotLastSyncedAt is null OR hubspotLastSyncedAt < now - retryDelay)

      // For SQLite timestamp comparison, we need to compare as strings in ISO format
      const retryThreshold = new Date(Date.now() - this.config.retryDelayMs).toISOString();

      const results = await db
        .select({ id: bookings.id })
        .from(bookings)
        .where(
          or(
            eq(bookings.hubspotSyncStatus, 'pending'),
            and(
              eq(bookings.hubspotSyncStatus, 'failed'),
              or(
                isNull(bookings.hubspotLastSyncedAt),
                lt(bookings.hubspotLastSyncedAt, retryThreshold)
              )
            )
          )
        );

      return results;
    } catch (error) {
      console.error('[SyncScheduler] Error finding bookings to sync:', error);
      return [];
    }
  }

  /**
   * Sync a batch of bookings using HubSpot's batch upsert API
   * This is MUCH more efficient than individual syncs (1 API call vs N calls)
   */
  private async syncBatch(batch: Array<{ id: string }>): Promise<SyncResult[]> {
    const results: SyncResult[] = [];

    // Fetch all bookings with context
    const bookingsData = await Promise.all(
      batch.map(({ id }) => getBookingWithContext(id))
    );

    // Filter out null bookings and prepare contacts for batch upsert
    const validBookings: Array<{ id: string; email: string; properties: Record<string, any> }> = [];
    const emailToBookingIds = new Map<string, string[]>();

    for (let i = 0; i < bookingsData.length; i++) {
      const booking = bookingsData[i];
      const bookingId = batch[i].id;

      if (!booking) {
        console.error(`[SyncScheduler] Booking ${bookingId} not found`);
        results.push({
          bookingId,
          success: false,
          error: 'Booking not found',
        });
        continue;
      }

      // Transform booking data
      const hubspotData = transformBookingToHubSpot(booking);
      const { email, ...properties } = hubspotData;

      // Track all booking IDs for this email (for duplicate detection)
      const bookingIds = emailToBookingIds.get(email) || [];
      bookingIds.push(bookingId);
      emailToBookingIds.set(email, bookingIds);

      // Only add the first occurrence of each email to validBookings
      // (batch API requires unique emails per request)
      if (bookingIds.length === 1) {
        validBookings.push({
          id: bookingId,
          email,
          properties,
        });
      }

      // Update status to 'syncing'
      await db
        .update(bookings)
        .set({
          hubspotSyncStatus: 'syncing',
          hubspotLastSyncedAt: new Date().toISOString(),
        })
        .where(eq(bookings.id, bookingId));
    }

    if (validBookings.length === 0) {
      return results;
    }

    // Batch upsert to HubSpot
    try {
      console.log(`[SyncScheduler] Batch upserting ${validBookings.length} unique contacts to HubSpot (from ${batch.length} bookings)...`);

      const batchResult = await this.hubspotClient.batchUpsertContacts(
        validBookings.map(b => ({ email: b.email, properties: b.properties }))
      );

      // Process successful results
      for (let i = 0; i < batchResult.results.length; i++) {
        const hubspotResult = batchResult.results[i];

        // Get ALL booking IDs for this email (multiple bookings can have same email)
        const bookingIdsForEmail = emailToBookingIds.get(hubspotResult.email) || [];

        // Update ALL bookings with this email as synced
        for (const bookingId of bookingIdsForEmail) {
          await db
            .update(bookings)
            .set({
              hubspotContactId: hubspotResult.id,
              hubspotSyncStatus: 'synced',
              hubspotLastSyncedAt: new Date().toISOString(),
              hubspotSyncError: null,
            })
            .where(eq(bookings.id, bookingId));

          console.log(`[SyncScheduler] Successfully synced booking ${bookingId} to HubSpot (contact: ${hubspotResult.id})`);

          results.push({
            bookingId,
            success: true,
            hubspotContactId: hubspotResult.id,
            created: hubspotResult.created,
          });
        }
      }

      // Process errors
      for (const errorResult of batchResult.errors) {
        // Get ALL booking IDs for this email
        const bookingIdsForEmail = emailToBookingIds.get(errorResult.email) || [];

        // Mark ALL bookings with this email as failed
        for (const bookingId of bookingIdsForEmail) {
          console.error(`[SyncScheduler] Error syncing booking ${bookingId}:`, errorResult.error);

          await db
            .update(bookings)
            .set({
              hubspotSyncStatus: 'failed',
              hubspotSyncError: errorResult.error,
              hubspotLastSyncedAt: new Date().toISOString(),
            })
            .where(eq(bookings.id, bookingId));

          results.push({
            bookingId,
            success: false,
            error: errorResult.error,
          });
        }
      }

    } catch (error: any) {
      console.error('[SyncScheduler] Batch upsert error:', error);

      // Mark all as failed
      for (const bookingData of validBookings) {
        await db
          .update(bookings)
          .set({
            hubspotSyncStatus: 'failed',
            hubspotSyncError: error.message || String(error),
            hubspotLastSyncedAt: new Date().toISOString(),
          })
          .where(eq(bookings.id, bookingData.id));

        results.push({
          bookingId: bookingData.id,
          success: false,
          error: error.message || String(error),
        });
      }
    }

    return results;
  }

  /**
   * Sync a single booking to HubSpot
   */
  private async syncBooking(bookingId: string): Promise<SyncResult> {
    try {
      // Get booking with full context
      const booking = await getBookingWithContext(bookingId);

      if (!booking) {
        console.error(`[SyncScheduler] Booking ${bookingId} not found`);
        return {
          bookingId,
          success: false,
          error: 'Booking not found',
        };
      }

      // Update status to 'syncing'
      await db
        .update(bookings)
        .set({
          hubspotSyncStatus: 'syncing',
          hubspotLastSyncedAt: new Date().toISOString(),
        })
        .where(eq(bookings.id, bookingId));

      // Transform booking data
      const hubspotData = transformBookingToHubSpot(booking);

      // Extract email and sync to HubSpot
      const { email, ...properties } = hubspotData;
      const hubspotResult = await this.hubspotClient.createOrUpdateContact(email, properties);

      // Update database with success
      await db
        .update(bookings)
        .set({
          hubspotContactId: hubspotResult.contactId,
          hubspotSyncStatus: 'synced',
          hubspotLastSyncedAt: new Date().toISOString(),
          hubspotSyncError: null,
        })
        .where(eq(bookings.id, bookingId));

      console.log(`[SyncScheduler] Successfully synced booking ${bookingId} to HubSpot (contact: ${hubspotResult.contactId})`);

      return {
        bookingId,
        success: true,
        hubspotContactId: hubspotResult.contactId,
        created: hubspotResult.created,
      };

    } catch (error: any) {
      console.error(`[SyncScheduler] Error syncing booking ${bookingId}:`, error);

      // Update database with failure
      await db
        .update(bookings)
        .set({
          hubspotSyncStatus: 'failed',
          hubspotSyncError: error.message || String(error),
          hubspotLastSyncedAt: new Date().toISOString(),
        })
        .where(eq(bookings.id, bookingId));

      return {
        bookingId,
        success: false,
        error: error.message || String(error),
      };
    }
  }

  /**
   * Manually trigger a sync cycle (useful for testing or manual triggering)
   */
  async triggerSync(forceAll = false): Promise<SyncStats | null> {
    await this.runSyncCycle(forceAll);
    return this.syncStats;
  }
}

// Singleton instance
let schedulerInstance: SyncScheduler | null = null;

/**
 * Get or create the sync scheduler instance
 */
export function getSyncScheduler(config?: Partial<SyncSchedulerConfig>): SyncScheduler {
  if (!schedulerInstance) {
    schedulerInstance = new SyncScheduler(config);
  }
  return schedulerInstance;
}

/**
 * Start the automatic sync scheduler (call once at app startup)
 */
export function startSyncScheduler(config?: Partial<SyncSchedulerConfig>): void {
  const scheduler = getSyncScheduler(config);
  scheduler.start();
}

/**
 * Stop the automatic sync scheduler
 */
export function stopSyncScheduler(): void {
  if (schedulerInstance) {
    schedulerInstance.stop();
  }
}

/**
 * Get sync scheduler status
 */
export function getSyncSchedulerStatus() {
  if (!schedulerInstance) {
    return { isRunning: false };
  }
  return schedulerInstance.getStatus();
}
