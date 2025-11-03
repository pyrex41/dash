CREATE TABLE `csg_sessions` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`cookie` text NOT NULL,
	`expires_at` integer NOT NULL,
	`created_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL,
	`updated_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL
);
--> statement-breakpoint
CREATE INDEX `idx_csg_sessions_expires_at` ON `csg_sessions` (`expires_at`);--> statement-breakpoint
ALTER TABLE `bookings` ADD `data` text;--> statement-breakpoint
ALTER TABLE `bookings` ADD `hubspot_contact_id` text;--> statement-breakpoint
ALTER TABLE `bookings` ADD `hubspot_sync_status` text DEFAULT 'pending';--> statement-breakpoint
ALTER TABLE `bookings` ADD `hubspot_last_synced_at` integer;--> statement-breakpoint
ALTER TABLE `bookings` ADD `hubspot_sync_error` text;--> statement-breakpoint
CREATE INDEX `idx_bookings_hubspot_contact_id` ON `bookings` (`hubspot_contact_id`);--> statement-breakpoint
CREATE INDEX `idx_bookings_hubspot_sync_status` ON `bookings` (`hubspot_sync_status`);