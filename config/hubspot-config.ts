/**
 * HubSpot Property Mapping Configuration
 *
 * This module provides type-safe access to HubSpot property mappings
 * defined in hubspot-properties.json
 */

import { readFileSync } from 'fs';
import { resolve } from 'path';

export interface PropertyMapping {
  hubspotProperty: string;
  sources: string[];
  required: boolean;
  validate?: 'email' | 'phone' | 'zip' | 'state' | 'date';
}

export interface CustomProperty {
  name: string;
  label: string;
  type: string;
  fieldType: string;
  groupName: string;
  description: string;
}

export interface HubSpotConfig {
  version: string;
  description: string;
  mappings: PropertyMapping[];
  validators: Record<string, string>;
  customProperties: CustomProperty[];
  notes: Record<string, string>;
}

// Load configuration from JSON file
const configPath = resolve(__dirname, 'hubspot-properties.json');
const configJson = JSON.parse(readFileSync(configPath, 'utf-8'));

export const hubspotConfig = configJson as HubSpotConfig;

/**
 * Get value from nested object using dot notation path
 * @param obj - Source object
 * @param path - Dot notation path (e.g., 'booking.data.applicant_info.f_name')
 * @returns Value at path or undefined
 */
export function getNestedValue(obj: any, path: string): any {
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
 * @param value - Value to validate
 * @param validatorName - Name of validator from config
 * @returns true if valid or no validator specified
 */
export function validateValue(value: any, validatorName?: string): boolean {
  if (!validatorName || !value) {
    return true; // No validation or no value
  }

  const validatorRegex = hubspotConfig.validators[validatorName];
  if (!validatorRegex) {
    console.warn(`[HubSpot Config] Validator '${validatorName}' not found`);
    return true;
  }

  try {
    const regex = new RegExp(validatorRegex);
    const isValid = regex.test(String(value));

    if (!isValid) {
      console.warn(`[HubSpot Config] Validation failed for '${validatorName}': ${value}`);
    }

    return isValid;
  } catch (error) {
    console.error(`[HubSpot Config] Error validating '${validatorName}':`, error);
    return false;
  }
}

/**
 * Extract HubSpot properties from booking data using configuration
 * @param bookingData - Combined booking and application data
 * @returns HubSpot properties object
 */
export function extractPropertiesFromConfig(bookingData: any): Record<string, any> {
  const properties: Record<string, any> = {};
  const validationErrors: string[] = [];

  for (const mapping of hubspotConfig.mappings) {
    let value: any = undefined;

    // Try each source in order until we find a value
    for (const sourcePath of mapping.sources) {
      value = getNestedValue(bookingData, sourcePath);
      if (value !== undefined && value !== null && value !== '') {
        break;
      }
    }

    // Check if required field is missing
    if (mapping.required && (value === undefined || value === null || value === '')) {
      validationErrors.push(`Required field '${mapping.hubspotProperty}' is missing`);
      continue;
    }

    // Skip if no value found
    if (value === undefined || value === null || value === '') {
      continue;
    }

    // Validate if validator specified
    if (mapping.validate) {
      const isValid = validateValue(value, mapping.validate);
      if (!isValid) {
        console.warn(
          `[HubSpot Config] Skipping invalid value for '${mapping.hubspotProperty}': ${value}`
        );
        continue;
      }
    }

    // Add to properties
    properties[mapping.hubspotProperty] = value;
  }

  // Log validation errors
  if (validationErrors.length > 0) {
    console.error('[HubSpot Config] Validation errors:', validationErrors);
  }

  return properties;
}

/**
 * Get custom properties that need to be created in HubSpot
 */
export function getCustomProperties(): CustomProperty[] {
  return hubspotConfig.customProperties;
}

/**
 * Get configuration notes
 */
export function getConfigNotes(): Record<string, string> {
  return hubspotConfig.notes;
}
