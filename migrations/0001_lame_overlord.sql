CREATE TABLE `producers` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`first_name` text NOT NULL,
	`last_name` text NOT NULL,
	`phone` text NOT NULL,
	`is_default` integer DEFAULT false NOT NULL,
	`email` text NOT NULL,
	`address_line1` text NOT NULL,
	`address_city` text NOT NULL,
	`address_state` text NOT NULL,
	`address_zip5` text NOT NULL,
	`npn` text NOT NULL,
	`writing_numbers` text NOT NULL,
	`created_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL,
	`updated_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL
);
--> statement-breakpoint
CREATE INDEX `idx_producers_email` ON `producers` (`email`);--> statement-breakpoint
CREATE INDEX `idx_producers_npn` ON `producers` (`npn`);