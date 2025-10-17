# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a full-stack insurance application dashboard built with:
- **Frontend**: Elm (functional language) with Vite, TypeScript, and Tailwind CSS
- **Backend**: Bun runtime with Elysia.js framework
- **Database**: Turso (LibSQL) with Drizzle ORM
- **External Integrations**: CSG (insurance carrier) API, LAPro (Lead Advantage Pro) API

The application manages insurance applications, automates submissions to CSG, and provides real-time verification using Puppeteer automation.

## Development Commands

### Running the Application

```bash
# Development mode (runs both frontend and backend concurrently)
bun run dev

# Run frontend only (Vite dev server on port 5173)
bun run dev:frontend

# Run backend only (Elysia server on port 3000)
bun run dev:backend

# Production build (builds frontend to dist/)
bun run build

# Production start (serves built frontend from backend)
bun run start
```

### Database Commands

```bash
# Generate migrations from schema changes
bunx drizzle-kit generate

# Push schema changes to database
bunx drizzle-kit push

# Open Drizzle Studio (database GUI)
bunx drizzle-kit studio
```

### Frontend Development

The frontend uses Elm, which compiles to JavaScript. Changes to `.elm` files trigger hot module reloading via Vite.

```bash
cd frontend
bun run dev    # Start Vite dev server
bun run build  # Build for production
```

## Architecture

### Frontend Structure

- **Main.elm**: Application router and entry point
- **Dashboard.elm**: Main applications list view with filtering, pagination
- **ApplicationPage.elm**: Detailed view/edit for individual applications
- **CSGApplicationView.elm**: View for CSG-submitted applications
- **CSGApplicationsPage.elm**: List view for CSG applications
- **Ports.elm**: Elm-JavaScript interop definitions
- **main.ts**: WebSocket setup and Elm initialization
- **SchemaDecoder.elm**: JSON decoders for API responses
- **DataEncoder.elm**: JSON encoders for API requests

### Backend Structure

- **index.ts**: Elysia server with WebSocket and HTTP endpoints
- **db/schema.ts**: Drizzle ORM schema definitions (applications, csgApplications, bookings, producers)
- **db/query.ts**: Database query functions
- **formatter.ts**: Application data formatting for CSG submission
- **csg/submit.ts**: CSG API submission logic
- **csg/verify.ts**: Puppeteer automation for CSG verification
- **csg/token.ts**: CSG authentication token management

### Real-time Communication

The application uses **WebSocket-first architecture** for all data operations:

- Frontend connects to `/ws` endpoint on startup
- All data fetching (applications, stats) goes through WebSocket messages
- Real-time updates for verification status broadcast to subscribed clients
- Client subscribes to application IDs to receive targeted updates

**Message Types**:
- `request_applications` → `applications_data`
- `request_application` → `application_data`
- `save_application` → `save_application_response`
- `submit_to_csg` → `submit_to_csg_response` + `verification_update` broadcasts
- `subscribe`/`unsubscribe` for real-time updates

### Database Schema

**applications**: Core application data
- `id`, `naic` (carrier code), `status`, `phone`, `email`, `effectiveDate`
- `rawData` (JSON from LAPro), `formattedData` (JSON for CSG), `rawMedications` (JSON array)

**csgApplications**: CSG submission tracking
- `applicationId` (FK to applications), `key` (CSG ID), `producerId`
- `verificationStatus` (pending/verifying/verified/failed/completed/declined)
- `verificationScreenshot`, `verificationError`, `lastVerifiedAt`

**bookings**: Confirmed enrollments
- `applicationId` (FK), `bookedAt`

**producers**: Agent/producer configuration
- `firstName`, `lastName`, `naic` (carrier), various carrier-specific IDs

### CSG Integration Flow

1. Application submitted via WebSocket `submit_to_csg` message
2. Backend calls CSG API with formatted application data
3. Backend updates database with CSG key and sets status to 'verifying'
4. Backend launches Puppeteer to verify submission
5. Puppeteer navigates to CSG portal, logs in, verifies application data
6. Verification results broadcast via WebSocket to subscribed clients
7. Screenshots saved to database for debugging

### Environment Variables

**IMPORTANT**: Environment variables must be duplicated with and without the `VITE_` prefix:
- **Without prefix**: Used by backend (e.g., `TURSO_DATABASE_URL`)
- **With `VITE_` prefix**: Used by frontend (e.g., `VITE_TURSO_DATABASE_URL`)

Required in `.env` (root):

**Database**:
- `TURSO_DATABASE_URL`, `TURSO_AUTH_TOKEN`: Backend database connection
- `VITE_TURSO_DATABASE_URL`, `VITE_TURSO_AUTH_TOKEN`: Frontend database connection

**LAPro API**:
- `LAPRO_USERNAME`, `LAPRO_PASSWORD`, `LAPRO_CLIENT_ID`, `LAPRO_CLIENT_SECRET`, `LAPRO_GRANT_TYPE`: Backend auth
- `VITE_LAPRO_*`: Same variables with `VITE_` prefix for frontend

**CSG API**:
- `CSG_API_URL`: CSG API base URL (e.g., https://api.csgactuarial.com)
- `CSG_API_KEY`: CSG API authentication key
- `CSG_USERNAME`, `CSG_PASSWORD`: CSG portal credentials for Puppeteer automation

**Server Configuration**:
- `PORT`: Backend server port (default 3000)
- `NODE_ENV`: 'development' or 'production'
- `VITE_API_WS_URL`: WebSocket endpoint (e.g., /ws)

**Optional**:
- `SSL_CERT`, `SSL_KEY`: SSL certificate paths for production
- `FORMAT_SERVER_URL`: Optional format server URL

## Key Patterns

### Elm Ports

Elm uses "ports" for JavaScript interop. Defined in `Ports.elm`:

```elm
port requestRefresh : { page : Int, pageSize : Int, ... } -> Cmd msg
port receiveApplications : (Value -> msg) -> Sub msg
```

TypeScript side in `main.ts`:
```typescript
app.ports.requestRefresh.subscribe(({ page, pageSize, ... }) => {
  socket.send(JSON.stringify({ type: 'request_applications', ... }))
})
```

### WebSocket Subscriptions

Clients subscribe to application IDs to receive updates:
```typescript
socket.send(JSON.stringify({
  type: 'subscribe',
  applicationIds: ['app-id-1', 'app-id-2']
}))
```

Backend broadcasts updates only to subscribed clients via `broadcastVerificationUpdate()`.

### Status Determination

Application status is computed by `determineStatus()` function (backend/src/db/query.ts):
- Base status from `applications.status`
- Enhanced with CSG verification status
- Considers booking status
- Returns: 'pending', 'submitting', 'verifying', 'verified', 'completed', 'declined', etc.

## Important Notes

- **Bun Runtime**: This project uses Bun, not Node.js. Use `bun` commands, not `npm`/`yarn`
- **Elm Compilation**: Elm files must be syntactically valid or the build fails completely
- **WebSocket Reliability**: All WebSocket messages logged with timestamps for debugging
- **Puppeteer Automation**: Runs in headless mode in production, headed mode when `debug: true`
- **Database Migrations**: Schema changes require running `bunx drizzle-kit generate` then `push`
- **CORS**: Backend configured for CORS in development; production uses same-origin

## Deployment

### Syncing Secrets to Fly.io

**Configuration split:**
- **Non-sensitive config** (in `fly.toml` `[env]` section): `PORT`, `NODE_ENV`, `FORMAT_SERVER_URL`
- **Sensitive secrets** (set via `flyctl secrets`): All database credentials, API keys, passwords

Use the provided script to sync your `.env` file to Fly.io secrets:

```bash
# Preview what will be set (dry run)
./sync-fly-secrets.sh --dry-run

# Actually set the secrets
./sync-fly-secrets.sh
```

The script automatically:
- Skips comments and empty lines
- Skips `VITE_` prefixed variables (frontend-only)
- Skips variables already in `fly.toml` (`PORT`, `NODE_ENV`, `FORMAT_SERVER_URL`)
- Handles quoted values and special characters
- Shows preview and asks for confirmation

## Common Tasks

### Adding a New Database Column

1. Edit `backend/src/db/schema.ts` to add column
2. Run `bunx drizzle-kit generate` to create migration
3. Run `bunx drizzle-kit push` to apply migration
4. Update TypeScript types and query functions in `backend/src/db/query.ts`
5. Update Elm decoders in `frontend/src/SchemaDecoder.elm`

### Adding a New WebSocket Message Type

1. Define message type in TypeScript interfaces (`frontend/src/main.ts`)
2. Add message handler in `socket.onmessage` (frontend)
3. Add message handler in WebSocket `message` callback (backend `index.ts`)
4. Define Elm port in `Ports.elm` if needed
5. Wire up port subscription in `main.ts`

### Debugging CSG Verification

1. Set `debug: true` in `verifyCSGApplication()` call
2. Check `csgApplications.verificationScreenshot` in database
3. Review backend logs for Puppeteer navigation errors
4. Verify CSG credentials in `.env`

### Testing Elm Changes

Elm has strong compile-time guarantees. If it compiles, it usually works. Focus on:
- JSON decoder/encoder correctness (test with real API data)
- Routing logic (verify URL patterns in `Main.elm`)
- Port compatibility (ensure types match between Elm and TypeScript)
