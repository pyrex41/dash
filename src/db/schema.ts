// /server/db/schema.ts
import { sql } from 'drizzle-orm';
import { sqliteTable, text, integer, unique, index } from 'drizzle-orm/sqlite-core';

export const user = sqliteTable('user', {
  id: text('id').primaryKey().notNull(),
  createdAt: integer('createdAt', { mode: 'timestamp' })
    .default(sql`(CURRENT_TIMESTAMP)`)
    .notNull(),
  updatedAt: integer('updatedAt', { mode: 'timestamp' })
    .default(sql`(CURRENT_TIMESTAMP)`)
    .notNull(),
  email: text('email'),
  isTemporary: integer('is_temporary', { mode: 'boolean' }).notNull().default(true),
  isAnonymous: integer('is_anonymous', { mode: 'boolean' }).notNull().default(true),
});

export const applications = sqliteTable(
  'applications',
  {
    id: text('id').primaryKey().notNull(),
    userId: text('user_id')
      .notNull()
      .references(() => user.id),
    status: text('status').notNull(),
    createdAt: integer('created_at', { mode: 'timestamp' }).notNull(),
    updatedAt: integer('updated_at', { mode: 'timestamp' }).notNull(),
    data: text('data', { mode: 'json' }).$type<Record<string, any>>().notNull(),
    name: text('name'),
    naic: text('naic'),
    zip: text('zip').notNull(),
    county: text('county').notNull(),
    dob: text('dob').notNull(),
    schema: text('schema', { mode: 'json' }).$type<Record<string, any>>().notNull(),
    originalSchema: text('original_schema', { mode: 'json' }).$type<Record<string, any>>().notNull(),
    underwritingType: integer('underwriting_type').notNull().default(0),
  },
  (table) => ({
    csgKeyIndex: index('idx_applications_csg_key').on(table.id),
    underwritingTypeCheck: sql`CHECK (underwriting_type IN (0, 1, 2))`
  })
);

export const csgApplications = sqliteTable(
  'csg_applications',
  {
    id: integer('id').primaryKey({ autoIncrement: true }),
    applicationId: text('application_id')
      .notNull()
      .references(() => applications.id),
    key: text('key').notNull(),
    responseBody: text('response_body').notNull(),
    brokerEmail: text('broker_email'),
    createdAt: integer('created_at', { mode: 'timestamp' })
      .notNull()
      .default(sql`CURRENT_TIMESTAMP`),
    updatedAt: integer('updated_at', { mode: 'timestamp' })
      .notNull()
      .default(sql`CURRENT_TIMESTAMP`),
  },
  (table) => ({
    applicationIdIndex: index('idx_csg_applications_application_id').on(table.applicationId),
    keyIndex: index('idx_csg_applications_key').on(table.key),
  })
);

export const onboarding = sqliteTable('onboarding', {
  id: text('id').primaryKey(),
  userId: text('user_id')
    .notNull()
    .references(() => user.id),
  createdAt: integer('created_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
  updatedAt: integer('updated_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
  data: text('data', { mode: 'json' }).notNull(),
});

export const csgTokens = sqliteTable('csg_tokens', {
  id: integer('id').primaryKey({ autoIncrement: true }),
  token: text('token').notNull(),
  expiresAt: integer('expires_at', { mode: 'timestamp' }).notNull(),
  createdAt: integer('created_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
  updatedAt: integer('updated_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
}, (table) => ({
  tokenIndex: index('idx_csg_tokens_token').on(table.token),
  expiresAtIndex: index('idx_csg_tokens_expires_at').on(table.expiresAt),
}));

export const brokers = sqliteTable('brokers', {
  id: text('id').primaryKey(),
  username: text('username').notNull().unique(),
  passwordHash: text('password_hash').notNull(),
  createdAt: integer('created_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
  updatedAt: integer('updated_at', { mode: 'timestamp' })
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
});

export const bookings = sqliteTable('bookings', {
  id: text('id').primaryKey().notNull(),
  userId: text('user_id')
    .references(() => user.id),
  applicationId: text('application_id')
    .references(() => applications.id),
  email: text('email').notNull(),
  phone: text('phone'),
  url: text('url').notNull(),
  event: text('event'),
  status: text('status').notNull(),
  createdAt: text('created_at')
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
  updatedAt: text('updated_at')
    .notNull()
    .default(sql`CURRENT_TIMESTAMP`),
}, (table) => ({
  userIdIndex: index('idx_bookings_user_id').on(table.userId),
  emailIndex: index('idx_bookings_email').on(table.email),
  applicationIdIndex: index('idx_bookings_application_id').on(table.applicationId),
}));

export const schema = {
  user,
  applications,
  csgApplications,
  onboarding,
  csgTokens,
  brokers,
  bookings
};