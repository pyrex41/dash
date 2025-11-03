/**
 * Test script for HubSpot configuration
 *
 * Run with: bun run config/test-config.ts
 */

import { readFileSync } from 'fs';
import { resolve } from 'path';

// Load configuration manually for testing
const configPath = resolve(__dirname, 'hubspot-properties.json');
const configJson = JSON.parse(readFileSync(configPath, 'utf-8'));

interface PropertyMapping {
  hubspotProperty: string;
  sources: string[];
  required: boolean;
  validate?: string;
}

interface HubSpotConfig {
  version: string;
  description: string;
  mappings: PropertyMapping[];
  validators: Record<string, string>;
  customProperties: any[];
  notes: Record<string, string>;
}

const hubspotConfig = configJson as HubSpotConfig;

/**
 * Get value from nested object using dot notation path
 */
function getNestedValue(obj: any, path: string): any {
  const parts = path.split('.');
  let current = obj;

  for (const part of parts) {
    if (current === null || current === undefined) {
      return undefined;
    }
    current = current[part];
  }

  return current;
}

/**
 * Validate value against a validator regex
 */
function validateValue(value: any, validatorName?: string): boolean {
  if (!validatorName || !value) {
    return true;
  }

  const validatorRegex = hubspotConfig.validators[validatorName];
  if (!validatorRegex) {
    console.warn(`Validator '${validatorName}' not found`);
    return true;
  }

  try {
    const regex = new RegExp(validatorRegex);
    return regex.test(String(value));
  } catch (error) {
    console.error(`Error validating '${validatorName}':`, error);
    return false;
  }
}

/**
 * Extract HubSpot properties from booking data
 */
function extractPropertiesFromConfig(bookingData: any): Record<string, any> {
  const properties: Record<string, any> = {};
  const validationErrors: string[] = [];

  for (const mapping of hubspotConfig.mappings) {
    let value: any = undefined;

    // Try each source in order
    for (const sourcePath of mapping.sources) {
      value = getNestedValue(bookingData, sourcePath);
      if (value !== undefined && value !== null && value !== '') {
        break;
      }
    }

    // Check required fields
    if (mapping.required && (value === undefined || value === null || value === '')) {
      validationErrors.push(`Required field '${mapping.hubspotProperty}' is missing`);
      continue;
    }

    // Skip empty values
    if (value === undefined || value === null || value === '') {
      continue;
    }

    // Validate if needed
    if (mapping.validate) {
      const isValid = validateValue(value, mapping.validate);
      if (!isValid) {
        console.warn(`Invalid value for '${mapping.hubspotProperty}': ${value}`);
        continue;
      }
    }

    properties[mapping.hubspotProperty] = value;
  }

  if (validationErrors.length > 0) {
    console.error('Validation errors:', validationErrors);
  }

  return properties;
}

// Test data mimicking real booking structure
const testBooking = {
  booking: {
    id: 'test-booking-123',
    email: 'john.doe@example.com',
    phone: '+15551234567',
    data: {
      applicant_info: {
        f_name: 'John',
        l_name: 'Doe',
        applicant_dob: '1960-05-15',
        zip5: '90210',
        address_city: 'Los Angeles',
        address_state: 'CA',
        phone: '+15551234567',
      },
      medicare_information: {
        effective_date: '2025-01-01',
      },
    },
  },
  application: {
    name: 'Doe, John',
    data: {
      applicant_info: {
        effective_date: '2025-01-01',
      },
    },
  },
};

console.log('='.repeat(60));
console.log('HubSpot Configuration Test');
console.log('='.repeat(60));
console.log('\nConfiguration version:', hubspotConfig.version);
console.log('Total mappings:', hubspotConfig.mappings.length);
console.log('Validators:', Object.keys(hubspotConfig.validators));
console.log('Custom properties:', hubspotConfig.customProperties.length);

console.log('\n' + '='.repeat(60));
console.log('Testing Property Extraction');
console.log('='.repeat(60));

const extractedProperties = extractPropertiesFromConfig(testBooking);

console.log('\nExtracted properties:');
console.log(JSON.stringify(extractedProperties, null, 2));

console.log('\n' + '='.repeat(60));
console.log('Validation Tests');
console.log('='.repeat(60));

const validationTests = [
  { name: 'Valid email', value: 'test@example.com', validator: 'email', expected: true },
  { name: 'Invalid email', value: 'not-an-email', validator: 'email', expected: false },
  { name: 'Valid phone', value: '+15551234567', validator: 'phone', expected: true },
  { name: 'Invalid phone', value: '123', validator: 'phone', expected: false },
  { name: 'Valid ZIP', value: '90210', validator: 'zip', expected: true },
  { name: 'Valid ZIP+4', value: '90210-1234', validator: 'zip', expected: true },
  { name: 'Invalid ZIP', value: '9021', validator: 'zip', expected: false },
  { name: 'Valid state', value: 'CA', validator: 'state', expected: true },
  { name: 'Invalid state', value: 'California', validator: 'state', expected: false },
  { name: 'Valid date', value: '2025-01-01', validator: 'date', expected: true },
  { name: 'Invalid date', value: '01/01/2025', validator: 'date', expected: false },
];

let passCount = 0;
let failCount = 0;

for (const test of validationTests) {
  const result = validateValue(test.value, test.validator);
  const passed = result === test.expected;

  if (passed) {
    console.log(`✓ ${test.name}: ${test.value}`);
    passCount++;
  } else {
    console.log(`✗ ${test.name}: ${test.value} (expected ${test.expected}, got ${result})`);
    failCount++;
  }
}

console.log('\n' + '='.repeat(60));
console.log('Test Summary');
console.log('='.repeat(60));
console.log(`Passed: ${passCount}/${validationTests.length}`);
console.log(`Failed: ${failCount}/${validationTests.length}`);

if (failCount === 0) {
  console.log('\n✓ All tests passed!');
  process.exit(0);
} else {
  console.log('\n✗ Some tests failed');
  process.exit(1);
}
