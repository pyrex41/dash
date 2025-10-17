import { drizzle } from 'drizzle-orm/libsql';
import { createClient } from '@libsql/client';
import { schema } from './schema';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables (go up one directory from backend/)
config({ path: resolve(process.cwd(), '../.env') });

const dbFile = 'file:./csg.db';

const client = createClient({
    url: process.env.TURSO_DATABASE_URL || dbFile,
    authToken: process.env.TURSO_AUTH_TOKEN,
});

export const getDb = () => drizzle(client, { schema });

