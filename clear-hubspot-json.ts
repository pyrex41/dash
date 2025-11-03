/**
 * Script to clear booking_json_data field from all HubSpot contacts
 */

const HUBSPOT_API_KEY = process.env.HUBSPOT_API_KEY;
const HUBSPOT_BASE_URL = 'https://api.hubapi.com';

if (!HUBSPOT_API_KEY) {
  console.error('Error: HUBSPOT_API_KEY not set in environment');
  process.exit(1);
}

interface Contact {
  id: string;
  properties: {
    email?: string;
    booking_json_data?: string;
  };
}

async function getAllContacts(): Promise<Contact[]> {
  const contacts: Contact[] = [];
  let after: string | undefined;

  do {
    const url = new URL('/crm/v3/objects/contacts', HUBSPOT_BASE_URL);
    url.searchParams.append('limit', '100');
    url.searchParams.append('properties', 'email,booking_json_data');
    if (after) {
      url.searchParams.append('after', after);
    }

    console.log(`Fetching contacts (after: ${after || 'start'})...`);

    const response = await fetch(url.toString(), {
      headers: {
        'Authorization': `Bearer ${HUBSPOT_API_KEY}`,
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      throw new Error(`HubSpot API error: ${response.status} ${await response.text()}`);
    }

    const data = await response.json();
    contacts.push(...data.results);
    after = data.paging?.next?.after;

    console.log(`Fetched ${data.results.length} contacts, total: ${contacts.length}`);
  } while (after);

  return contacts;
}

async function clearBookingJsonData(contacts: Contact[]): Promise<void> {
  // Filter contacts that have booking_json_data
  const contactsWithJsonData = contacts.filter(c => c.properties.booking_json_data);

  console.log(`\nFound ${contactsWithJsonData.length} contacts with booking_json_data`);

  if (contactsWithJsonData.length === 0) {
    console.log('No contacts to update!');
    return;
  }

  // Batch update in chunks of 100 (HubSpot limit)
  const chunkSize = 100;
  for (let i = 0; i < contactsWithJsonData.length; i += chunkSize) {
    const chunk = contactsWithJsonData.slice(i, i + chunkSize);

    const inputs = chunk.map(contact => ({
      id: contact.id,
      properties: {
        booking_json_data: '', // Clear the field
      },
    }));

    console.log(`\nClearing booking_json_data for batch ${Math.floor(i / chunkSize) + 1} (${chunk.length} contacts)...`);

    const response = await fetch(`${HUBSPOT_BASE_URL}/crm/v3/objects/contacts/batch/update`, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${HUBSPOT_API_KEY}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ inputs }),
    });

    if (!response.ok) {
      const errorText = await response.text();
      console.error(`Batch update error: ${response.status} ${errorText}`);
      throw new Error(`Failed to update batch ${Math.floor(i / chunkSize) + 1}`);
    }

    const result = await response.json();
    console.log(`✅ Successfully cleared ${result.results.length} contacts`);

    // Rate limit: wait 1 second between batches
    if (i + chunkSize < contactsWithJsonData.length) {
      console.log('Waiting 1s before next batch...');
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
  }
}

async function main() {
  try {
    console.log('🚀 Starting HubSpot booking_json_data cleanup...\n');

    // Get all contacts
    const contacts = await getAllContacts();
    console.log(`\n📊 Total contacts in HubSpot: ${contacts.length}`);

    // Clear booking_json_data field
    await clearBookingJsonData(contacts);

    console.log('\n✅ Cleanup complete!');
  } catch (error) {
    console.error('\n❌ Error:', error);
    process.exit(1);
  }
}

main();
