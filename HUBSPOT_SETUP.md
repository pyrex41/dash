# HubSpot Integration Setup Guide

This guide walks you through connecting your insurance dashboard to a real HubSpot account.

## Prerequisites

- HubSpot account (any tier - Free, Starter, Professional, or Enterprise)
- Admin access to create Private Apps in HubSpot
- Access to your `.env` file in this project

---

## Step 1: Create HubSpot Private App

### 1.1 Navigate to Private Apps
1. Log into your **HubSpot account**
2. Click the **Settings icon** (gear) in the top right
3. In the left sidebar, go to **Integrations** → **Private Apps**

### 1.2 Create New Private App
1. Click **"Create a private app"** button
2. Fill in the **Basic Info** tab:
   - **Name**: `Insurance Dashboard Integration` (or your preferred name)
   - **Description**: `Syncs insurance booking data to HubSpot contacts`
   - **Logo**: (optional)

### 1.3 Configure Scopes (Permissions)
1. Click the **"Scopes"** tab
2. Search for and enable these scopes:
   - ✅ `crm.objects.contacts.read` - Read contacts
   - ✅ `crm.objects.contacts.write` - Create and update contacts

   **Note**: These are the ONLY scopes needed. Don't grant additional permissions.

### 1.4 Generate Access Token
1. Click **"Create app"** button
2. Review the permissions and click **"Continue creating"**
3. **IMPORTANT**: Copy the **Access Token** that appears
   - This is your `HUBSPOT_API_KEY`
   - You won't be able to see it again (but you can regenerate if needed)
   - Keep it secure - treat it like a password

---

## Step 2: Get Your HubSpot Portal ID

### Method 1: From Account Settings
1. In HubSpot, go to **Settings** → **Account Setup** → **Account Defaults**
2. Your **Hub ID** is displayed near the top
3. It's a 7-8 digit number (e.g., `12345678`)

### Method 2: From URL
1. Look at your HubSpot URL: `https://app.hubspot.com/contacts/12345678/...`
2. The number after `/contacts/` is your Portal ID

---

## Step 3: Configure Environment Variables

### 3.1 Update .env File
Open your `.env` file and replace the placeholder values:

```bash
# HubSpot Integration Configuration
HUBSPOT_API_KEY=pat-na1-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
HUBSPOT_PORTAL_ID=12345678

# Optional: Customize sync behavior
HUBSPOT_SYNC_INTERVAL_MS=300000       # 5 minutes (300000 ms)
HUBSPOT_RETRY_DELAY_MS=60000          # 1 minute (60000 ms)
```

**Replace**:
- `your_hubspot_private_app_token_here` → Your actual Private App access token
- `your_portal_id_here` → Your actual Hub ID

### 3.2 Optional Configuration

**Adjust Sync Interval** (how often automatic sync runs):
```bash
HUBSPOT_SYNC_INTERVAL_MS=300000   # Default: 5 minutes
# HUBSPOT_SYNC_INTERVAL_MS=60000  # 1 minute (more frequent)
# HUBSPOT_SYNC_INTERVAL_MS=600000 # 10 minutes (less frequent)
```

**Adjust Retry Delay** (how long to wait before retrying failed syncs):
```bash
HUBSPOT_RETRY_DELAY_MS=60000      # Default: 1 minute
# HUBSPOT_RETRY_DELAY_MS=300000   # 5 minutes (wait longer)
```

---

## Step 4: Verify Configuration

### 4.1 Restart Backend Server
If your server is running, restart it to load the new environment variables:

```bash
# Stop the backend (Ctrl+C if running)
# Then restart:
bun run dev:backend
```

### 4.2 Check Backend Logs
Look for these log messages on startup:

**✅ Success**:
```
[HubSpot] Sync scheduler initialized
[HubSpot] Sync interval: 300000ms (5 minutes)
```

**❌ Warning** (if API key is missing):
```
[HubSpot] HUBSPOT_API_KEY not set - HubSpot sync will be disabled
```

### 4.3 Test Connection
Test the HubSpot connection by triggering a manual sync:

**Method 1: Via API endpoint**
```bash
curl -X POST http://localhost:3000/api/sync/trigger
```

**Method 2: Via the UI**
1. Open the dashboard: http://localhost:5173
2. Navigate to the **Bookings** tab
3. Click the **Sync** button on any booking row
4. Watch for the status to change from "Pending" to "Syncing" to "Synced"

**Method 3: Check sync status**
```bash
curl http://localhost:3000/api/sync/status
```

Should return:
```json
{
  "running": true,
  "totalSynced": 0,
  "totalFailed": 0,
  "lastRunTime": "2025-11-03T..."
}
```

---

## Step 5: Verify in HubSpot

### 5.1 Check Contact Creation
1. Go to **HubSpot** → **Contacts** → **Contacts**
2. Look for newly synced contacts
3. They should have the email from your booking

### 5.2 Verify Contact Data
Click on a synced contact and verify these fields are populated:
- **Email** - Contact's email address
- **First Name** - From applicant info
- **Last Name** - From applicant info
- **Phone** - Phone number
- **Date of Birth** - If available
- **City/State/Zip** - Address info
- **Medicare Effective Date** - If available
- **Booking ID** - Custom property with booking reference

---

## Step 6: (Optional) Create Custom Properties

The integration automatically uses standard HubSpot properties. To add custom fields:

### 6.1 Create Custom Contact Properties
1. Go to **Settings** → **Data Management** → **Properties**
2. Click **"Create property"**
3. Create these custom properties:

| Property Name | Internal Name | Field Type | Description |
|--------------|---------------|------------|-------------|
| Booking ID | `booking_id` | Single-line text | Insurance booking reference |
| Medicare Effective Date | `medicare_effective_date` | Date picker | Medicare coverage start date |

### 6.2 Update Field Mapping (if needed)
Custom property mapping is in `backend/src/integrations/hubspot.ts` in the `transformBookingToHubSpot()` function.

---

## How It Works

### Automatic Sync
- **Runs every 5 minutes** (configurable)
- Finds bookings with `hubspotSyncStatus = 'pending'` or `'failed'`
- Syncs up to **100 bookings per cycle**
- Processes in **batches of 10** with 1-second delays
- Updates database with sync results

### Manual Sync
- Click **Sync** button in the Bookings UI
- Triggers immediate sync for that specific booking
- Real-time status updates via WebSocket

### Retry Logic
- Failed syncs automatically retry after 1 minute (configurable)
- Tracks retry attempts in database
- Logs errors for debugging

### Rate Limiting
- **10 requests per 10 seconds** to HubSpot API
- Automatic throttling to stay within limits
- Queues requests when approaching limits

---

## Data Mapping Reference

The system syncs these fields from bookings to HubSpot contacts:

| HubSpot Property | Source Field | Fallback Order |
|-----------------|--------------|----------------|
| `email` | `booking.email` | Required - no fallback |
| `firstname` | `booking.data.applicant_info.f_name` | `application.data.applicant_info.f_name` |
| `lastname` | `booking.data.applicant_info.l_name` | `application.data.applicant_info.l_name` |
| `phone` | `booking.phone` | `booking.data.applicant_info.phone` |
| `date_of_birth` | `booking.data.applicant_info.dob` | `application.data.applicant_info.dob` |
| `zip` | `booking.data.applicant_info.zip` | `application.data.applicant_info.zip` |
| `city` | `booking.data.applicant_info.city` | `application.data.applicant_info.city` |
| `state` | `booking.data.applicant_info.state` | `application.data.applicant_info.state` |
| `medicare_effective_date` | `application.effectiveDate` | - |
| `booking_id` | `booking.id` | - |

**Fallback Hierarchy**:
1. `booking.data` - Booking's stored application data
2. `application.data` - Original application data
3. `booking` fields - Direct booking properties

---

## Monitoring & Troubleshooting

### Check Sync Status
```bash
# Get current sync statistics
curl http://localhost:3000/api/sync/status
```

Response:
```json
{
  "running": true,
  "totalSynced": 45,
  "totalFailed": 2,
  "lastRunTime": "2025-11-03T19:30:00.000Z"
}
```

### Trigger Manual Sync Cycle
```bash
# Force immediate sync of all pending bookings
curl -X POST http://localhost:3000/api/sync/trigger
```

### View Backend Logs
Backend logs show detailed sync activity:
```
[HubSpot] Starting sync cycle...
[HubSpot] Found 12 bookings to sync
[HubSpot] Successfully synced booking abc-123 to contact 54321
[HubSpot] Sync cycle complete: 12 synced, 0 failed
```

### Common Issues

**Issue**: "HUBSPOT_API_KEY not set" warning
- **Solution**: Check `.env` file has `HUBSPOT_API_KEY=...` set
- Restart backend server after adding

**Issue**: Sync status stays "Pending"
- **Solution**: Check API key is valid in HubSpot
- Verify scopes are correctly set (contacts read/write)
- Check backend logs for error messages

**Issue**: Rate limit errors (429)
- **Solution**: Automatic retry is built-in
- Reduce `HUBSPOT_SYNC_INTERVAL_MS` if syncing too fast
- System will automatically throttle

**Issue**: Contact not appearing in HubSpot
- **Solution**: Check email address is valid
- Verify required scopes are granted
- Look for errors in `hubspotSyncError` database field

### Database Fields for Debugging

Check these fields in the `bookings` table:
```sql
SELECT
  id,
  email,
  hubspot_sync_status,
  hubspot_contact_id,
  hubspot_last_synced_at,
  hubspot_sync_error
FROM bookings
WHERE hubspot_sync_status = 'failed';
```

---

## Security Best Practices

### 1. Protect Your API Key
- ✅ Keep API key in `.env` file (already in `.gitignore`)
- ✅ Never commit API key to version control
- ✅ Use different keys for development/production environments
- ✅ Rotate keys periodically

### 2. Minimum Required Scopes
- Only grant `crm.objects.contacts.read` and `crm.objects.contacts.write`
- Don't grant additional permissions
- Review Private App permissions regularly

### 3. Monitor API Usage
- Check HubSpot's API usage dashboard regularly
- Set up alerts for unusual activity
- Review sync logs for errors

### 4. Production Deployment
When deploying to production (e.g., Fly.io):
```bash
# Set secrets (not in fly.toml)
flyctl secrets set HUBSPOT_API_KEY=your_production_key_here
flyctl secrets set HUBSPOT_PORTAL_ID=your_portal_id
```

---

## Testing Recommendations

### Development Testing
1. Start with a **test HubSpot account** if available
2. Test with a small batch (1-2 bookings) first
3. Verify data appears correctly in HubSpot
4. Test error scenarios (invalid email, network issues)

### Production Deployment
1. Set `HUBSPOT_SYNC_INTERVAL_MS` to a longer interval initially (e.g., 15 minutes)
2. Monitor first few sync cycles closely
3. Check for errors in HubSpot contact creation
4. Gradually reduce interval if performance is good

---

## Next Steps

After successful configuration:

1. ✅ **Verify automatic sync** - Wait for next sync cycle (5 min)
2. ✅ **Test manual sync** - Use UI sync buttons
3. ✅ **Check HubSpot contacts** - Verify data appears correctly
4. ✅ **Monitor logs** - Watch for errors or issues
5. ✅ **Configure alerts** - Set up monitoring if needed

---

## Support & Resources

- **HubSpot API Documentation**: https://developers.hubspot.com/docs/api/overview
- **Private Apps Guide**: https://developers.hubspot.com/docs/api/private-apps
- **Rate Limits**: https://developers.hubspot.com/docs/api/usage-details
- **Integration Code**: `backend/src/integrations/hubspot.ts`
- **Sync Scheduler**: `backend/src/services/syncScheduler.ts`

---

**Configuration complete!** Your insurance dashboard is now connected to HubSpot. 🎉
