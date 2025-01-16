CREATE TABLE `csg_sessions` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`cookie` text NOT NULL,
	`expires_at` integer NOT NULL,
	`created_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL,
	`updated_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL
);
--> statement-breakpoint
CREATE INDEX `idx_csg_sessions_expires_at` ON `csg_sessions` (`expires_at`); 