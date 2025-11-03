# HubSpot Integration & Bookings Dashboard Expansion PRD

## Overview

This project expands the current insurance application dashboard to include a bookings view and integrates with HubSpot CRM to sync booking data as contacts and custom objects. The goal is to provide comprehensive visibility into both applications and confirmed bookings while automatically feeding customer data into the CRM for sales and service teams.

## Current State Analysis

### Database Schema

**Applications Table** (`applications`):
- Core application data with full JSON `data` field
- Contains applicant info, medicare details, payment info, producer info
- Status tracking for application lifecycle

**Bookings Table** (`bookings`):
- Current fields: `id`, `userId`, `applicationId`, `email`, `phone`, `url`, `event`, `status`, `createdAt`, `updatedAt`
- **Missing**: `data` field to store partial/complete application data
- Links to applications via `applicationId` foreign key

**CSG Applications Table** (`csg_applications`):
- Tracks CSG submission and verification status
- Links to applications via `applicationId`

**Users Table** (`user`):
- User account information with email
- Links to applications and bookings via `userId`

### Dashboard Functionality

**Current Applications View** (Dashboard.elm):
- Displays applications with columns: Name, Carrier, Status, Phone, Email, Effective Date, Date Started
- Status computed via `determineStatus()` function considering CSG verification and booking existence
- WebSocket integration for real-time updates
- Search, pagination, and filtering capabilities

**Data Flow**:
1. Frontend requests applications via WebSocket (`request_applications`)
2. Backend queries `getApplications()` from `query.ts`
3. Joins with `bookings`, `csgApplications`, `user` tables for complete data
4. Returns formatted `ApplicationRow` objects with computed status

## Requirements

### 1. Database Schema Updates

**Add Data Field to Bookings Table**:
```sql
ALTER TABLE bookings ADD COLUMN data TEXT;
```
- Store partial or complete application data as JSON
- Enable bookings to be self-contained while maintaining application relationships
- Backfill existing bookings with relevant application data

**Add HubSpot Sync Tracking**:
```sql
ALTER TABLE bookings ADD COLUMN hubspot_contact_id TEXT;
ALTER TABLE bookings ADD COLUMN hubspot_sync_status TEXT DEFAULT 'pending';
ALTER TABLE bookings ADD COLUMN hubspot_last_synced_at INTEGER;
ALTER TABLE bookings ADD COLUMN hubspot_sync_error TEXT;
```
- Track HubSpot contact creation and updates
- Monitor sync status (pending, success, failed, retrying)
- Store error messages for failed syncs
- Timestamp last successful sync

### 2. Backend API Enhancements

**New Query Functions** (in `db/query.ts`):

```typescript
// Get bookings with related application data
export const getBookings = async (
  page: number, 
  pageSize: number, 
  searchTerm: string, 
  status?: string
) => {
  // Similar structure to getApplications but focused on bookings
  // Join with applications for missing data
  // Return BookingRow objects
};

// Get single booking with full context
export const getBookingWithContext = async (bookingId: string) => {
  // Fetch booking + related application + user + CSG data
  // Merge booking.data with application data where needed
  // Return complete booking object
};

// Export bookings for bulk operations
export const exportBookings = async (searchTerm: string) => {
  // Similar to exportApplications but for bookings
};
```

**HubSpot Integration Module** (new file: `integrations/hubspot.ts`):

```typescript
// HubSpot API client with authentication
export class HubSpotClient {
  private apiKey: string;
  private baseUrl = 'https://api.hubapi.com';
  
  constructor(apiKey: string) {
    this.apiKey = apiKey;
  }
  
  async createContact(contactData: ContactData): Promise<Contact> {
    // Create HubSpot contact with applicant information
  }
  
  async updateContact(contactId: string, properties: Record<string, any>): Promise<Contact> {
    // Update existing contact with booking status changes
  }
  
  async createCustomObject(objectType: string, properties: Record<string, any>): Promise<CustomObject> {
    // Create booking as custom object linked to contact
  }
}

// Data transformation utilities
export const transformBookingToHubSpot = (booking: BookingWithContext): HubSpotData => {
  // Map booking data to HubSpot contact properties
  // Handle partial data by falling back to application data
  // Format dates, clean phone numbers, etc.
};
```

**WebSocket Message Handlers** (in `index.ts`):

```typescript
// New message types
if (data.type === 'request_bookings') {
  // Handle bookings request similar to applications
  const bookings = await getBookings(data.page, data.pageSize, data.searchTerm, data.status);
  ws.send(JSON.stringify({
    type: 'bookings_data',
    data: bookings
  }));
}

if (data.type === 'sync_booking_to_hubspot') {
  // Sync single booking to HubSpot
  const result = await syncBookingToHubSpot(data.bookingId);
  ws.send(JSON.stringify({
    type: 'hubspot_sync_result',
    data: result
  }));
}

if (data.type === 'bulk_sync_bookings') {
  // Bulk sync multiple bookings
  const results = await bulkSyncBookingsToHubSpot(data.bookingIds);
  ws.send(JSON.stringify({
    type: 'bulk_hubspot_sync_result',
    data: results
  }));
}
```

**Environment Variables**:
```
HUBSPOT_API_KEY=your_hubspot_private_app_token
HUBSPOT_PORTAL_ID=your_portal_id
HUBSPOT_CONTACT_SYNC_ENABLED=true
HUBSPOT_AUTO_SYNC_BOOKINGS=true
HUBSPOT_CUSTOM_OBJECT_TYPE=booking
HUBSPOT_RATE_LIMIT_DELAY=100  # ms between API calls
```

### 3. Frontend Dashboard Expansion

**New Data Types** (in `Dashboard.elm`):

```elm
type alias BookingRow =
    { id : String
    , bookingId : String
    , applicationId : Maybe String
    , name : Maybe String
    , email : String
    , phone : Maybe String
    , status : BookingStatus
    , url : String
    , event : Maybe String
    , dateCreated : String
    , hubspotContactId : Maybe String
    , hubspotSyncStatus : Maybe HubSpotSyncStatus
    , hasApplicationData : Bool
    }

type BookingStatus
    = Pending
    | Confirmed
    | Cancelled
    | Completed

type HubSpotSyncStatus
    = Pending
    | Success
    | Failed String
    | Retrying
```

**Model Updates**:

```elm
type alias Model =
    { -- existing fields
    , currentView : DashboardView
    , bookings : List BookingRow
    , bookingsPagination : PaginationInfo
    , isBookingsLoading : Bool
    , selectedBookingForSync : Maybe String
    , hubspotSyncInProgress : Set String
    }

type DashboardView
    = ApplicationsView
    | BookingsView
```

**View Components**:

1. **View Toggle**:
```elm
viewToggle : Model -> Html Msg
viewToggle model =
    div [ class "flex space-x-4 mb-4" ]
        [ button 
            [ class toggleButtonClass ApplicationsView model.currentView
            , onClick (SetView ApplicationsView)
            ]
            [ text "Applications" ]
        , button 
            [ class toggleButtonClass BookingsView model.currentView
            , onClick (SetView BookingsView)
            ]
            [ text "Bookings" ]
        ]
```

2. **Bookings Table**:
```elm
viewBookingsTable : Model -> Html Msg
viewBookingsTable model =
    if model.isBookingsLoading then
        viewLoadingSpinner
    else if List.isEmpty model.bookings then
        div [ class "p-4 text-center text-gray-500" ]
            [ text "No bookings found" ]
    else
        table [ class "min-w-full divide-y divide-gray-200" ]
            [ thead [ class "bg-gray-50" ]
                [ tr []
                    [ th [ class "py-3 px-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Name" ]
                    , th [] [ text "Email" ]
                    , th [] [ text "Phone" ]
                    , th [] [ text "Status" ]
                    , th [] [ text "Event" ]
                    , th [] [ text "Date Created" ]
                    , th [] [ text "HubSpot Status" ]
                    , th [] [ text "Actions" ]
                    ]
                ]
            , tbody [ class "bg-white divide-y divide-gray-200" ]
                (List.map (viewBookingRow model) model.bookings)
            ]
```

3. **Booking Row with HubSpot Actions**:
```elm
viewBookingRow : Model -> BookingRow -> Html Msg
viewBookingRow model booking =
    tr [ class "border-b hover:bg-gray-50" ]
        [ td [ class "py-3 px-4" ] [ text (booking.name |> Maybe.withDefault "") ]
        , td [] [ text booking.email ]
        , td [] [ text (booking.phone |> Maybe.withDefault "") ]
        , td [] [ viewBookingStatus booking.status ]
        , td [] [ text (booking.event |> Maybe.withDefault "") ]
        , td [] [ text booking.dateCreated ]
        , td [] [ viewHubSpotSyncStatus booking.hubspotSyncStatus ]
        , td []
            [ if Set.member booking.id model.hubspotSyncInProgress then
                button [ class "btn btn-sm btn-warning" ] [ text "Syncing..." ]
              else if booking.hubspotSyncStatus == Just (Failed _) then
                button 
                    [ class "btn btn-sm btn-error"
                    , onClick (RetryHubSpotSync booking.id)
                    ]
                    [ text "Retry" ]
              else if booking.hubspotContactId == Nothing then
                button 
                    [ class "btn btn-sm btn-primary"
                    , onClick (SyncBookingToHubSpot booking.id)
                    , disabled (not (canSyncBooking booking))
                    ]
                    [ text "Sync to HubSpot" ]
              else
                button 
                    [ class "btn btn-sm btn-success"
                    , onClick (ViewHubSpotContact booking.hubspotContactId)
                    ]
                    [ text "View in HubSpot" ]
            ]
        ]
```

**New Messages**:
```elm
type Msg
    = -- existing messages
    | SetView DashboardView
    | RequestBookings Int Int String (Maybe String)
    | BookingsData BookingsResponse
    | SyncBookingToHubSpot String
    | HubSpotSyncResult String HubSpotSyncResult
    | RetryHubSpotSync String
    | BulkSyncBookings (List String)
```

### 4. HubSpot Data Mapping

**Contact Properties**:
```json
{
  "firstname": "applicant_info.f_name",
  "lastname": "applicant_info.l_name", 
  "email": "applicant_info.email || booking.email",
  "phone": "applicant_info.phone || booking.phone",
  "lifecyclestage": "lead",  // or "customer" based on booking status
  "createdate": "booking.createdAt",
  "hs_object_id": "booking.id"
}
```

**Custom Properties** (to be created in HubSpot):
```json
{
  "booking_id": "booking.id",
  "booking_status": "booking.status",
  "booking_url": "booking.url",
  "booking_event": "booking.event",
  "application_naic": "related_application.naic",
  "medicare_number": "data.medicare_information.medicare_number",
  "effective_date": "data.medicare_information.effective_date",
  "csg_verification_status": "related_csg.verificationStatus",
  "insurance_carrier": "lookup(naic)",
  "last_sync_date": "current_timestamp"
}
```

**Custom Object** (optional - "Insurance Booking"):
- Linked to Contact via association
- Contains full booking data as properties
- Timeline of status changes
- Documents and attachments

### 5. Integration Workflow

**Automatic Sync Flow**:
1. Booking created via Calendly/booking system
2. Webhook or API call creates booking record
3. If `HUBSPOT_AUTO_SYNC_BOOKINGS=true`, immediately attempt HubSpot sync
4. Create/update contact in HubSpot
5. Create custom object if configured
6. Update `hubspot_contact_id` and `hubspot_sync_status` fields
7. Send WebSocket notification to dashboard

**Manual Sync Flow**:
1. User clicks "Sync to HubSpot" button on booking row
2. Frontend sends `sync_booking_to_hubspot` WebSocket message
3. Backend calls HubSpot API
4. Update booking record with sync results
5. Send `hubspot_sync_result` WebSocket message
6. Update UI with success/error state

**Bulk Sync Flow**:
1. Admin selects multiple bookings via checkboxes
2. Click "Bulk Sync to HubSpot"
3. Backend processes bookings with rate limiting
4. Progress updates via WebSocket
5. Final results displayed in modal

**Error Handling & Retry**:
- Store error details in `hubspot_sync_error` field
- Automatic retry for transient errors (network, rate limits)
- Manual retry button for failed syncs
- Exponential backoff for repeated failures
- Admin dashboard to view sync history and retry failed bookings

### 6. Configuration & Admin Features

**Environment Configuration**:
```bash
# HubSpot Settings
HUBSPOT_API_KEY=pat-na1-your-private-app-token
HUBSPOT_PORTAL_ID=1234567

# Sync Behavior
HUBSPOT_AUTO_SYNC_BOOKINGS=true
HUBSPOT_CONTACT_SYNC_ENABLED=true
HUBSPOT_CUSTOM_OBJECT_ENABLED=false
HUBSPOT_RATE_LIMIT_DELAY=100

# Data Mapping
HUBSPOT_CUSTOM_PROPERTIES_FILE=./config/hubspot-properties.json
```

**Admin Dashboard Features**:
- Toggle between Applications and Bookings views
- Filter bookings by HubSpot sync status
- Bulk sync operations with progress tracking
- Sync error log with retry capabilities
- Configuration panel for HubSpot settings
- Test connection button for HubSpot API

**Security & Compliance**:
- API key stored in environment variables only
- Rate limiting to prevent API abuse
- Audit logging of all HubSpot operations
- GDPR compliance for contact data handling
- Error handling that doesn't expose sensitive data

### 7. Testing Requirements

**Unit Tests**:
- HubSpot data transformation functions
- Booking query functions
- Status determination logic for bookings
- WebSocket message handlers

**Integration Tests**:
- End-to-end booking creation to HubSpot sync
- Error scenarios (invalid API key, rate limits)
- Bulk sync with mixed success/failure
- WebSocket real-time updates

**E2E Tests**:
- Dashboard view toggle functionality
- Manual sync button interactions
- Search and filtering on bookings
- Bulk operations with loading states

### 8. Deployment & Rollout

**Phase 1: Schema & Backend** (Week 1)
- Add data field to bookings table
- Implement booking query functions
- Create HubSpot integration module
- Add WebSocket handlers

**Phase 2: Frontend Dashboard** (Week 2)
- Add bookings view and toggle
- Implement bookings table component
- Add manual sync UI controls
- Basic error handling and loading states

**Phase 3: HubSpot Configuration** (Week 3)
- Set up HubSpot custom properties
- Configure data mappings
- Test automatic sync workflows
- Add bulk sync capabilities

**Phase 4: Production Rollout** (Week 4)
- Deploy with feature flags
- Monitor sync success rates
- Gather user feedback
- Performance optimization

### 9. Success Metrics

**Technical Metrics**:
- HubSpot sync success rate > 95%
- Average sync time < 2 seconds per booking
- Dashboard load time < 3 seconds for 100 bookings
- WebSocket message delivery rate 100%

**Business Metrics**:
- Bookings visible in dashboard within 5 seconds of creation
- HubSpot contacts created for 90%+ of confirmed bookings
- Sales team access to complete customer data
- Reduced manual data entry by 80%

**User Experience**:
- Clear visual distinction between applications and bookings
- Intuitive sync status indicators
- Easy retry mechanism for failed syncs
- Comprehensive error messaging without technical jargon

## Risks & Mitigations

**Risk**: HubSpot API rate limits
**Mitigation**: Implement exponential backoff, queueing system, batch operations

**Risk**: Data mapping inconsistencies
**Mitigation**: Comprehensive testing, configurable mappings, validation functions

**Risk**: Booking data incompleteness
**Mitigation**: Smart fallback to application data, data validation, admin overrides

**Risk**: Security of API credentials
**Mitigation**: Environment variables, secret scanning, access controls

**Risk**: Performance impact on dashboard
**Mitigation**: Lazy loading, pagination, efficient queries, caching

## Future Enhancements

**Phase 2 Features**:
- HubSpot timeline integration (activities, notes)
- Two-way sync (updates from HubSpot back to system)
- Advanced reporting and analytics
- Custom object associations (applications → bookings → contacts)
- Webhook integration for real-time HubSpot updates

**Integration Opportunities**:
- Calendly webhook for automatic booking creation
- Email automation triggers based on booking status
- Sales sequences triggered by booking completion
- Lead scoring based on application data

This PRD provides a comprehensive roadmap for expanding the dashboard functionality while establishing robust CRM integration capabilities. The phased approach ensures incremental value delivery while maintaining system stability.