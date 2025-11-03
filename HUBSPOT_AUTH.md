# HubSpot Authentication Setup Guide

This document explains how to implement HubSpot authentication and webhook integration in your application, based on the implementation in the MDS Portal.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Authentication Setup](#authentication-setup)
4. [API Client Implementation](#api-client-implementation)
5. [Webhook Integration](#webhook-integration)
6. [Environment Configuration](#environment-configuration)
7. [Testing](#testing)
8. [Security Considerations](#security-considerations)

## Overview

The HubSpot integration uses **Private App API tokens** (Bearer authentication) to:
- Retrieve contact information from HubSpot CRM
- Update contact properties
- Process webhook events for real-time updates
- Access custom objects and associations

**Authentication Method**: Bearer token authentication via Private Apps (recommended for server-to-server integrations)

## Prerequisites

### 1. HubSpot Account Setup

You need:
- A HubSpot account with appropriate permissions
- Access to create Private Apps
- CRM contacts and custom objects (if applicable)

### 2. Required Dependencies (OCaml)

```ocaml
(* In your dune file *)
(libraries
  base
  lwt
  lwt.unix
  cohttp
  cohttp-lwt-unix
  yojson
  stdio
)
```

For other languages, you'll need:
- HTTP client library (cohttp for OCaml, axios/fetch for Node.js, requests for Python)
- JSON parsing library
- Async/Promise support

## Authentication Setup

### Step 1: Create a HubSpot Private App

1. Log in to your HubSpot account
2. Navigate to **Settings** → **Integrations** → **Private Apps**
3. Click **Create a private app**
4. Configure your app:
   - **Name**: Your application name (e.g., "MDS Portal")
   - **Description**: Brief description of what your app does
   - **Logo**: Optional app logo

### Step 2: Configure Scopes

Select the required scopes for your integration:

**Contact Scopes** (minimum required):
- `crm.objects.contacts.read` - Read contact information
- `crm.objects.contacts.write` - Update contact properties

**Custom Object Scopes** (if using custom objects):
- `crm.objects.custom.read` - Read custom objects
- `crm.objects.custom.write` - Write custom objects

**Additional Scopes** (optional):
- `crm.schemas.contacts.read` - Read contact property definitions
- `crm.schemas.custom.read` - Read custom object schemas

### Step 3: Generate API Token

1. After configuring scopes, click **Create app**
2. Click **Show token** to reveal your Private App access token
3. **IMPORTANT**: Copy this token immediately - you won't be able to see it again
4. Store the token securely (see [Environment Configuration](#environment-configuration))

### Step 4: Get Client Secret (for Webhooks)

1. In your Private App settings, find the **Client Secret**
2. Copy this secret - you'll need it for webhook signature validation
3. Store alongside your API key

## API Client Implementation

### Configuration Structure

```ocaml
(* hubspot.ml *)
type config = {
  api_key: string;          (* Your Private App access token *)
  client_secret: string;    (* Your Private App client secret *)
  base_url: string;         (* HubSpot API base URL *)
}

(* Lazy configuration loading from environment variables *)
let config_ref = ref None

let get_config () =
  match !config_ref with
  | Some config -> config
  | None ->
    let config = {
      api_key =
        (match Stdlib.Sys.getenv_opt "HUBSPOT_API_KEY" with
        | Some key -> key
        | None -> failwith "HUBSPOT_API_KEY environment variable is required");
      client_secret =
        (match Stdlib.Sys.getenv_opt "HUBSPOT_CLIENT_SECRET" with
        | Some secret -> secret
        | None -> failwith "HUBSPOT_CLIENT_SECRET environment variable is required");
      base_url = "https://api.hubapi.com";
    } in
    config_ref := Some config;
    config
```

**For Node.js/JavaScript**:
```javascript
// hubspot.js
class HubSpotClient {
  constructor() {
    this.apiKey = process.env.HUBSPOT_API_KEY;
    this.clientSecret = process.env.HUBSPOT_CLIENT_SECRET;
    this.baseUrl = 'https://api.hubapi.com';

    if (!this.apiKey || !this.clientSecret) {
      throw new Error('HUBSPOT_API_KEY and HUBSPOT_CLIENT_SECRET are required');
    }
  }
}
```

**For Python**:
```python
# hubspot.py
import os

class HubSpotClient:
    def __init__(self):
        self.api_key = os.environ.get('HUBSPOT_API_KEY')
        self.client_secret = os.environ.get('HUBSPOT_CLIENT_SECRET')
        self.base_url = 'https://api.hubapi.com'

        if not self.api_key or not self.client_secret:
            raise ValueError('HUBSPOT_API_KEY and HUBSPOT_CLIENT_SECRET are required')
```

### Making Authenticated Requests

#### GET Request

```ocaml
(* OCaml implementation *)
let get_request path =
  let config = get_config () in
  let headers = Header.init_with "Authorization" ("Bearer " ^ config.api_key) in
  let uri = Uri.of_string (config.base_url ^ path) in

  let* (resp, body) = Client.get ~headers uri in
  let status = Response.status resp in

  if Code.code_of_status status <> 200 then
    let* body_str = Cohttp_lwt.Body.to_string body in
    Lwt.fail_with (Printf.sprintf "HubSpot API error: %s" body_str)
  else
    let* body_str = Cohttp_lwt.Body.to_string body in
    Lwt.return (Yojson.Safe.from_string body_str)
```

**Node.js/JavaScript**:
```javascript
async function getRequest(path) {
  const response = await fetch(`${this.baseUrl}${path}`, {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${this.apiKey}`,
      'Content-Type': 'application/json'
    }
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`HubSpot API error: ${error}`);
  }

  return response.json();
}
```

**Python**:
```python
import requests

def get_request(self, path):
    headers = {
        'Authorization': f'Bearer {self.api_key}',
        'Content-Type': 'application/json'
    }

    response = requests.get(f'{self.base_url}{path}', headers=headers)

    if response.status_code != 200:
        raise Exception(f'HubSpot API error: {response.text}')

    return response.json()
```

#### PATCH Request (Update Contact)

```ocaml
(* OCaml implementation *)
let patch_request path data =
  let config = get_config () in
  let headers = Header.init_with "Authorization" ("Bearer " ^ config.api_key)
  |> fun h -> Header.add h "Content-Type" "application/json" in
  let uri = Uri.of_string (config.base_url ^ path) in
  let body = Cohttp_lwt.Body.of_string (Yojson.Safe.to_string data) in

  let* (resp, body) = Client.patch ~headers ~body uri in
  let status = Response.status resp in

  if Code.code_of_status status <> 200 then
    let* body_str = Cohttp_lwt.Body.to_string body in
    Lwt.fail_with (Printf.sprintf "HubSpot API error: %s" body_str)
  else
    let* body_str = Cohttp_lwt.Body.to_string body in
    Lwt.return (Yojson.Safe.from_string body_str)
```

**Node.js/JavaScript**:
```javascript
async function patchRequest(path, data) {
  const response = await fetch(`${this.baseUrl}${path}`, {
    method: 'PATCH',
    headers: {
      'Authorization': `Bearer ${this.apiKey}`,
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`HubSpot API error: ${error}`);
  }

  return response.json();
}
```

### Common API Operations

#### Get Contact by ID

```ocaml
(* OCaml *)
let get_contact ?(with_history=false) contact_id =
  let base_properties = "email,firstname,lastname,zip,date_of_birth" in
  let path =
    if with_history then
      Printf.sprintf "/crm/v3/objects/contacts/%s?properties=%s&propertiesWithHistory=supplemental_plan_status"
        contact_id base_properties
    else
      Printf.sprintf "/crm/v3/objects/contacts/%s?properties=%s"
        contact_id base_properties
  in
  let* json = get_request path in
  Lwt.return (parse_contact json)
```

**Node.js/JavaScript**:
```javascript
async getContact(contactId, withHistory = false) {
  const properties = 'email,firstname,lastname,zip,date_of_birth';
  let path = `/crm/v3/objects/contacts/${contactId}?properties=${properties}`;

  if (withHistory) {
    path += '&propertiesWithHistory=supplemental_plan_status';
  }

  const data = await this.getRequest(path);
  return this.parseContact(data);
}
```

#### Update Contact Property

```ocaml
(* OCaml *)
let update_verification_link contact_id link =
  let path = Printf.sprintf "/crm/v3/objects/contacts/%s" contact_id in
  let data = `Assoc [
    ("properties", `Assoc [
      ("plan_status_link", `String link)
    ])
  ] in
  let* _ = patch_request path data in
  Lwt.return_unit
```

**Node.js/JavaScript**:
```javascript
async updateVerificationLink(contactId, link) {
  const path = `/crm/v3/objects/contacts/${contactId}`;
  const data = {
    properties: {
      plan_status_link: link
    }
  };

  await this.patchRequest(path, data);
}
```

#### Get Custom Object Associations

```ocaml
(* OCaml - Get objects associated with a contact *)
let get_custom_object_data contact_id object_type =
  let object_id = object_name_to_id object_type in
  let path = Printf.sprintf "/crm/v4/objects/contacts/%s/associations/%s?limit=1"
    contact_id object_id in

  let* associations_json = get_request path in
  let open Yojson.Safe.Util in
  let associations = associations_json |> member "results" |> to_list in

  match associations with
  | assoc :: _ ->
    (match assoc |> member "toObjectId" |> to_int_option with
    | Some assoc_id_int ->
      let assoc_id = Int.to_string assoc_id_int in
      let object_path = Printf.sprintf "/crm/v4/objects/%s/%s?properties=%s"
        object_id assoc_id properties in
      let* object_json = get_request object_path in
      Lwt.return (parse_plan_data object_json)
    | None -> Lwt.return None)
  | [] -> Lwt.return None
```

## Webhook Integration

### Step 1: Configure Webhooks in HubSpot

1. Navigate to **Settings** → **Integrations** → **Private Apps**
2. Select your Private App
3. Click on the **Webhooks** tab
4. Configure webhook subscriptions:
   - **Target URL**: Your application's webhook endpoint (e.g., `https://yourdomain.com/webhook/hubspot`)
   - **Subscription Type**: Select events to subscribe to (e.g., `contact.creation`, `contact.propertyChange`)

### Step 2: Webhook Signature Validation

**IMPORTANT**: HubSpot uses **v1 signature validation** for CRM webhooks:

```ocaml
(* OCaml implementation *)
let validate_webhook_signature ~request_body ~signature =
  let config = get_config () in

  (* HubSpot v1 signature validation:
     1. Concatenate client_secret + request_body
     2. Create SHA-256 hash (not HMAC)
     3. Compare with provided signature in X-HubSpot-Signature header
  *)

  (* NOTE: Requires SHA-256 library - digestif or mirage-crypto *)
  let source_string = config.client_secret ^ request_body in
  let expected_signature = Digestif.SHA256.(to_hex (digest_string source_string)) in

  String.equal expected_signature signature
```

**Node.js/JavaScript**:
```javascript
const crypto = require('crypto');

function validateWebhookSignature(requestBody, signature) {
  // HubSpot v1: SHA-256(client_secret + request_body)
  const sourceString = this.clientSecret + requestBody;
  const expectedSignature = crypto
    .createHash('sha256')
    .update(sourceString)
    .digest('hex');

  return expectedSignature === signature;
}
```

**Python**:
```python
import hashlib

def validate_webhook_signature(self, request_body, signature):
    # HubSpot v1: SHA-256(client_secret + request_body)
    source_string = self.client_secret + request_body
    expected_signature = hashlib.sha256(source_string.encode()).hexdigest()

    return expected_signature == signature
```

### Step 3: Webhook Handler

```ocaml
(* OCaml webhook handler *)
let handle_webhook_hubspot state request =
  let* body = Dream.body request in

  (* Validate webhook signature *)
  let signature = Dream.header request "X-HubSpot-Signature" in
  (match signature with
  | Some sig_header ->
    if not (Hubspot.validate_webhook_signature ~request_body:body ~signature:sig_header) then
      error_response (Errors.Validation_error "Invalid webhook signature")
    else
      (* Parse webhook payload - HubSpot sends an array of events *)
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in

      let events =
        try to_list json
        with _ -> [json]  (* Single object fallback *)
      in

      (* Process event *)
      let object_id = match events with
        | [] -> raise (Failure "No events in webhook payload")
        | event :: _ ->
          let id = event |> member "objectId" in
          match id with
          | `Int i -> Int.to_string i
          | `Intlit s -> s
          | `String s -> s
          | _ -> raise (Failure "Invalid objectId type")
      in

      (* Handle the event *)
      handle_contact_event object_id

  | None ->
    error_response (Errors.Validation_error "Missing webhook signature header"))
```

**Node.js/JavaScript**:
```javascript
async function handleWebhook(req, res) {
  const signature = req.headers['x-hubspot-signature'];
  const body = JSON.stringify(req.body);

  // Validate signature
  if (!this.validateWebhookSignature(body, signature)) {
    return res.status(401).json({ error: 'Invalid signature' });
  }

  // Parse events
  const events = Array.isArray(req.body) ? req.body : [req.body];

  for (const event of events) {
    const objectId = event.objectId.toString();
    await this.handleContactEvent(objectId);
  }

  res.json({ status: 'processed' });
}
```

### Webhook Event Structure

HubSpot sends webhook events in this format:

```json
[
  {
    "objectId": 12345,
    "propertyName": "supplemental_plan_status",
    "propertyValue": "Active",
    "changeSource": "CRM",
    "eventId": 3816279340,
    "subscriptionId": 654321,
    "portalId": 987654,
    "appId": 123456,
    "occurredAt": 1699564800000,
    "subscriptionType": "contact.propertyChange",
    "attemptNumber": 0
  }
]
```

## Environment Configuration

### Required Environment Variables

Create a `.env` file in your project root:

```bash
# HubSpot API Configuration
HUBSPOT_API_KEY=pat-na1-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
HUBSPOT_CLIENT_SECRET=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

# Webhook Configuration
WEBHOOK_BASE_URL=https://yourdomain.com

# Server Configuration
PORT=8081
HOST=0.0.0.0
```

### Loading Environment Variables

**OCaml** (custom loader):
```ocaml
let load_env_file () =
  try
    if Stdlib.Sys.file_exists ".env" then (
      let lines = Stdio.In_channel.read_lines ".env" in
      List.iter lines ~f:(fun line ->
        match String.split line ~on:'=' with
        | [key; value] -> Unix.putenv (String.strip key) (String.strip value)
        | _ -> ()
      )
    )
  with _ -> ()
```

**Node.js**:
```javascript
require('dotenv').config();
```

**Python**:
```python
from dotenv import load_dotenv
load_dotenv()
```

## Testing

### Test Webhook Locally with ngrok

1. **Install ngrok**:
   ```bash
   brew install ngrok  # macOS
   # or download from https://ngrok.com
   ```

2. **Start your application**:
   ```bash
   PORT=8081 npm start  # or your start command
   ```

3. **Start ngrok tunnel**:
   ```bash
   ngrok http 8081
   ```

4. **Configure HubSpot webhook**:
   - Copy the ngrok HTTPS URL (e.g., `https://abc123.ngrok.io`)
   - In HubSpot webhook settings, set target URL to: `https://abc123.ngrok.io/webhook/hubspot`

5. **Test the webhook**:
   - Make a change to a contact in HubSpot
   - Check your application logs to see the webhook event

### Manual Testing with curl

#### Test GET Request:
```bash
curl -X GET \
  "https://api.hubapi.com/crm/v3/objects/contacts/12345?properties=email,firstname" \
  -H "Authorization: Bearer YOUR_API_KEY"
```

#### Test PATCH Request:
```bash
curl -X PATCH \
  "https://api.hubapi.com/crm/v3/objects/contacts/12345" \
  -H "Authorization: Bearer YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "properties": {
      "plan_status_link": "https://example.com/verify?token=abc123"
    }
  }'
```

#### Test Webhook Endpoint:
```bash
# Generate test signature
echo -n "YOUR_CLIENT_SECRETtest_payload" | shasum -a 256

# Send test webhook
curl -X POST \
  "http://localhost:8081/webhook/hubspot" \
  -H "X-HubSpot-Signature: GENERATED_SIGNATURE" \
  -H "Content-Type: application/json" \
  -d '[{"objectId": 12345, "propertyName": "test"}]'
```

### Test Endpoint (Development Only)

Create a development-only test endpoint that bypasses signature validation:

```ocaml
(* OCaml *)
let handle_test_webhook state request =
  let* body = Dream.body request in
  (* Skip signature validation for testing *)
  let json = Yojson.Safe.from_string body in
  (* Process webhook... *)
  json_response (`Assoc [("status", `String "processed")])

(* Register route only in development *)
let routes = [
  Dream.post "/test/webhook" (handle_test_webhook state);
  (* ... other routes *)
]
```

## Security Considerations

### 1. API Key Storage
- ✅ **DO**: Store in environment variables or secure secret management (AWS Secrets Manager, HashiCorp Vault)
- ❌ **DON'T**: Commit API keys to version control
- ❌ **DON'T**: Hardcode keys in application code
- ✅ **DO**: Use `.env` files with `.gitignore` for local development

### 2. Webhook Signature Validation
- ✅ **ALWAYS** validate webhook signatures in production
- ❌ **NEVER** skip signature validation except in isolated test endpoints
- ✅ **DO** use timing-safe comparison for signatures
- ✅ **DO** log failed validation attempts for security monitoring

### 3. Rate Limiting

HubSpot API has rate limits:
- **Private Apps**: 100 requests per 10 seconds
- Implement exponential backoff for rate limit errors (429 status)

```javascript
async function requestWithRetry(path, options, retries = 3) {
  for (let i = 0; i < retries; i++) {
    try {
      return await this.request(path, options);
    } catch (error) {
      if (error.status === 429 && i < retries - 1) {
        const delay = Math.pow(2, i) * 1000; // Exponential backoff
        await new Promise(resolve => setTimeout(resolve, delay));
        continue;
      }
      throw error;
    }
  }
}
```

### 4. Data Privacy
- Only request minimum necessary scopes
- Only store required contact data
- Implement data retention policies
- Follow GDPR/privacy regulations for contact data

### 5. Error Handling
- Don't expose API keys in error messages
- Log errors securely (redact sensitive data)
- Return generic error messages to clients

```ocaml
(* Bad - exposes details *)
let error = Printf.sprintf "Failed with key: %s" api_key

(* Good - generic message *)
let error = "HubSpot API request failed. Check application logs for details"
```

## Advanced Topics

### Working with Custom Objects

HubSpot custom objects require mapping object names to IDs:

```ocaml
let object_name_to_id = function
  | "supplemental_plan" -> "2-8483761"
  | "prescription_drug_plan" -> "2-8567541"
  | "medicare_advantage_plan" -> "2-7775359"
  | name -> name
```

Find your custom object IDs:
1. HubSpot Settings → Objects → Custom Objects
2. Click on your custom object
3. The ID is in the URL: `/objects/{portalId}/editor/{objectId}`

### Batch Operations

For processing multiple records efficiently:

```javascript
async batchUpdateContacts(updates) {
  const path = '/crm/v3/objects/contacts/batch/update';
  const data = {
    inputs: updates.map(update => ({
      id: update.contactId,
      properties: update.properties
    }))
  };

  return this.postRequest(path, data);
}
```

### OAuth (Alternative to Private Apps)

For user-facing applications, consider OAuth instead:

```javascript
// Redirect user to HubSpot OAuth
const authUrl = `https://app.hubspot.com/oauth/authorize?` +
  `client_id=${CLIENT_ID}&` +
  `redirect_uri=${REDIRECT_URI}&` +
  `scope=${SCOPES}`;

// Handle callback and exchange code for token
async function exchangeCodeForToken(code) {
  const response = await fetch('https://api.hubapi.com/oauth/v1/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      client_id: CLIENT_ID,
      client_secret: CLIENT_SECRET,
      redirect_uri: REDIRECT_URI,
      code: code
    })
  });

  return response.json(); // { access_token, refresh_token, ... }
}
```

## Troubleshooting

### Common Issues

**401 Unauthorized**
- Check API key is correct and not expired
- Verify `Authorization: Bearer` header format
- Ensure Private App has required scopes

**403 Forbidden**
- Private App lacks necessary scope permissions
- Contact property doesn't exist or isn't accessible

**404 Not Found**
- Contact ID doesn't exist
- Incorrect API endpoint path
- Custom object ID is wrong

**429 Rate Limited**
- Implement exponential backoff
- Reduce request frequency
- Consider batching operations

**Webhook signature validation fails**
- Verify client secret is correct
- Use raw request body (not parsed JSON)
- Check SHA-256 hash implementation
- Ensure string concatenation order: `client_secret + body`

### Debug Mode

Enable detailed logging for troubleshooting:

```javascript
class HubSpotClient {
  constructor(debug = false) {
    this.debug = debug;
    // ...
  }

  async request(path, options) {
    if (this.debug) {
      console.log('Request:', path, options);
    }

    const response = await fetch(this.baseUrl + path, options);

    if (this.debug) {
      console.log('Response:', response.status, await response.clone().text());
    }

    return response;
  }
}
```

## Resources

- [HubSpot API Documentation](https://developers.hubspot.com/docs/api/overview)
- [Private Apps Guide](https://developers.hubspot.com/docs/api/private-apps)
- [Webhooks Documentation](https://developers.hubspot.com/docs/api/webhooks)
- [CRM API v3](https://developers.hubspot.com/docs/api/crm/contacts)
- [API Rate Limits](https://developers.hubspot.com/docs/api/usage-details)

## License

This documentation is based on the MDS Portal implementation and is provided as-is for educational purposes.
