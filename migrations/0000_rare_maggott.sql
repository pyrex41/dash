CREATE TABLE `csg_tokens2` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`token` text NOT NULL,
	`expires_at` integer NOT NULL,
	`created_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL,
	`updated_at` integer DEFAULT CURRENT_TIMESTAMP NOT NULL
);
--> statement-breakpoint
CREATE INDEX `idx_csg_tokens2_token` ON `csg_tokens2` (`token`);--> statement-breakpoint
CREATE INDEX `idx_csg_tokens2_expires_at` ON `csg_tokens2` (`expires_at`);--> statement-breakpoint

