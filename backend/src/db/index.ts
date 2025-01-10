import { drizzle } from 'drizzle-orm/libsql';
import { createClient } from '@libsql/client';
import { schema } from './schema';

const dbFile = 'file:./csg.db';

const client = createClient({
    url: process.env.TURSO_DATABASE_URL || dbFile,
    authToken: process.env.TURSO_AUTH_TOKEN,
});

export const getDb = () => drizzle(client, { schema });

