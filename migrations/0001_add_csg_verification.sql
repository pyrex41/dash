ALTER TABLE csg_applications ADD COLUMN last_submitted_at INTEGER DEFAULT 0 NOT NULL;
ALTER TABLE csg_applications ADD COLUMN verification_status TEXT DEFAULT 'pending' NOT NULL;
UPDATE csg_applications SET last_submitted_at = unixepoch(); 