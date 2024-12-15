import { drizzle } from 'drizzle-orm/libsql';
import { createClient } from '@libsql/client';
import { schema } from './schema';

const dbFile = 'file:./csg.db';
const tursoDbUrl = import.meta.env.VITE_TURSO_DATABASE_URL;
const tursoAuthToken = import.meta.env.VITE_TURSO_AUTH_TOKEN;

const client = createClient({
    url: tursoDbUrl,
    authToken: tursoAuthToken,
    // syncUrl: tursoDbUrl,
    // syncInterval: 60 // 60 seconds, adjust as needed
});

export const db = drizzle(client, { schema });

