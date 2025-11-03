# HubSpot Configuration

This directory contains configuration files for HubSpot contact sync integration.

## Files

### `hubspot-properties.json`

Defines the data mapping between booking/application data and HubSpot contact properties.

**Structure:**
- `mappings`: Array of property mappings with source paths and validation rules
- `validators`: Regular expressions for data validation
- `customProperties`: Custom properties that need to be created in HubSpot
- `notes`: Important usage notes for development and production

**Example mapping:**
```json
{
  "hubspotProperty": "firstname",
  "sources": [
    "booking.data.applicant_info.f_name",
    "booking.data.applicant_info.first_name",
    "application.data.applicant_info.f_name",
    "application.data.applicant_info.first_name"
  ],
  "required": false
}
```

### `hubspot-config.ts`

TypeScript module that loads and processes the JSON configuration.

**Key functions:**
- `extractPropertiesFromConfig(bookingData)` - Extract HubSpot properties from booking data
- `getNestedValue(obj, path)` - Get value from nested object using dot notation
- `validateValue(value, validatorName)` - Validate value against regex pattern
- `getCustomProperties()` - Get list of custom properties to create in HubSpot
- `getConfigNotes()` - Get configuration usage notes

## Setup Instructions

### 1. Create Custom Properties in HubSpot

Before syncing, create these custom properties in your HubSpot portal:

1. Navigate to: **Settings → Properties → Contact Properties**
2. Create the following properties:

| Property Name | Label | Type | Field Type | Group | Description |
|---------------|-------|------|------------|-------|-------------|
| `booking_id` | Booking ID | string | text | Booking Info | Unique identifier for the booking from the insurance dashboard |
| `medicare_effective_date` | Medicare Effective Date | date | date | Insurance Info | The effective date for Medicare coverage |

### 2. Configure Environment Variables

Copy the required variables from `.env.dashboard.example` to your `.env` file:

```bash
# Backend
HUBSPOT_API_KEY=your_api_key_here
HUBSPOT_PORTAL_ID=your_portal_id

# Sync settings
HUBSPOT_CONTACT_SYNC_ENABLED=true
HUBSPOT_MOCK_API=false  # Use true for testing
HUBSPOT_RATE_LIMIT_DELAY=100
HUBSPOT_SYNC_INTERVAL_MS=300000
HUBSPOT_SYNC_BATCH_SIZE=10

# Frontend (Vite)
VITE_HUBSPOT_CONTACT_SYNC_ENABLED=true
VITE_HUBSPOT_MOCK_API=false
```

### 3. Test the Configuration

Run the test script to verify mappings:

```bash
bun run test:hubspot-config
```

## Data Flow

```
Booking Data → extractPropertiesFromConfig() → HubSpot Properties
     ↓                      ↓                          ↓
booking.data    Source path resolution         firstname
application.data    Validation                 lastname
booking fields      Fallback chain             email
                                               phone
                                               ...
```

## Validation

The configuration supports built-in validators:

- **email**: RFC-compliant email format
- **phone**: International phone format
- **zip**: US ZIP code (5 digits or ZIP+4)
- **state**: 2-letter US state code
- **date**: ISO date format (YYYY-MM-DD)

Invalid values are logged but do not block the sync operation.

## Customization

### Adding a New Property Mapping

1. Edit `hubspot-properties.json`
2. Add a new object to the `mappings` array:

```json
{
  "hubspotProperty": "custom_field",
  "sources": [
    "booking.data.path.to.field",
    "application.data.path.to.field"
  ],
  "required": false,
  "validate": "email"
}
```

3. If using a custom HubSpot property, add it to `customProperties` array
4. Restart the backend server to load new configuration

### Adding a New Validator

1. Edit `hubspot-properties.json`
2. Add regex pattern to `validators` object:

```json
"validators": {
  "custom_format": "^[A-Z]{3}-\\d{3}$"
}
```

3. Reference in mapping: `"validate": "custom_format"`

## Development vs Production

### Development Mode

```bash
HUBSPOT_MOCK_API=true
```

- Safe testing without affecting real HubSpot data
- Logs all sync operations
- Simulates API responses

### Production Mode

```bash
HUBSPOT_MOCK_API=false
```

- **WARNING**: Creates real contacts in HubSpot
- Ensure custom properties are created first
- Monitor sync logs for errors
- Test thoroughly in development first

## Troubleshooting

### Common Issues

**Missing Required Fields**
- Check console for validation errors
- Verify source data paths in booking/application objects
- Add fallback sources to mapping configuration

**Validation Failures**
- Review validator regex patterns
- Check data format in source system
- Consider adding data transformation logic

**Sync Failures**
- Verify HubSpot API key is valid
- Check that custom properties exist in HubSpot portal
- Review rate limit settings
- Check backend logs for detailed error messages

### Debug Logging

Enable debug logging:

```typescript
// In backend/src/integrations/hubspot.ts
console.log('[HubSpot Transform] Properties:', properties);
```

## Architecture

```
┌─────────────────────────────────────┐
│  hubspot-properties.json            │
│  (Configuration)                    │
└──────────────┬──────────────────────┘
               │
               ↓
┌─────────────────────────────────────┐
│  hubspot-config.ts                  │
│  (Configuration Loader)             │
│  - extractPropertiesFromConfig()    │
│  - validateValue()                  │
└──────────────┬──────────────────────┘
               │
               ↓
┌─────────────────────────────────────┐
│  integrations/hubspot.ts            │
│  (HubSpot Client)                   │
│  - transformBookingToHubSpot()      │
│  - createOrUpdateContact()          │
└──────────────┬──────────────────────┘
               │
               ↓
┌─────────────────────────────────────┐
│  services/syncScheduler.ts          │
│  (Automatic Sync)                   │
│  - Background sync interval         │
│  - Retry logic                      │
└─────────────────────────────────────┘
```

## Support

For issues or questions about HubSpot configuration:
1. Review console logs for detailed error messages
2. Check HubSpot API documentation: https://developers.hubspot.com/
3. Verify custom properties exist in HubSpot portal
4. Test with HUBSPOT_MOCK_API=true first
