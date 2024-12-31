// formatter.ts
import { cleanCarrierName, formatPhoneNumber, formatDate } from './utils'
import * as fs from 'fs'
import * as path from 'path'

// Add type for zip data
type ZipData = {
  [zip: string]: {
    cities: string[];
    state: string;
  }
}

// Load zip data
const zipDataPath = path.join(__dirname, '../static/zipData.json')
const zipData: ZipData = JSON.parse(fs.readFileSync(zipDataPath, 'utf-8'))

// Helper function to lookup city/state from zip
function lookupZipData(zip5: string): { city: string, state: string } {
  const data = zipData[zip5] || { cities: [], state: '' }
  return {
    city: data.cities[0] || '',
    state: data.state || ''
  }
}

// Helper to format dates consistently 
function formatDate(dateString: string | null | undefined): string | null {
  if (!dateString) return null
  // Remove the time component if it exists
  return dateString.split('T')[0]
}

// Helper to format phone numbers into components
function formatPhoneNumber(phone: any): { 
  area_code: string;
  central_office_code: string;
  station_code: string;
} {
  if (typeof phone === 'object' && phone !== null) {
    return {
      area_code: phone.area_code || '',
      central_office_code: phone.central_office_code || '',
      station_code: phone.station_code || ''
    }
  }
  const digits = (phone || '').replace(/\D/g, '')
  return {
    area_code: digits.slice(0, 3),
    central_office_code: digits.slice(3, 6),
    station_code: digits.slice(6, 10)
  }
}

// Helper to calculate Medicare-related dates
function calculateMedicareDates(birthDate: string, effectiveDate: string, partADate?: string, partBDate?: string) {
  const birth = new Date(birthDate)
  const effective = new Date(effectiveDate)
  
  // Calculate turning 65 date
  const t65Date = new Date(birth)
  t65Date.setFullYear(birth.getFullYear() + 65)
  
  // Calculate 6 month windows
  const turn65Upper = new Date(effective)
  turn65Upper.setDate(turn65Upper.getDate() + 180)
  
  const turn65Lower = new Date(effective) 
  turn65Lower.setDate(turn65Lower.getDate() - 150)

  // Check if within 6 months of turning 65
  const t65SixMonths = t65Date >= turn65Lower && t65Date <= turn65Upper

  // Check Part B date if exists
  let partBSixMonths = null
  if (partBDate) {
    const partB = new Date(partBDate)
    partBSixMonths = partB >= turn65Lower && partB <= turn65Upper
  }

  return {
    t65Date,
    t65SixMonths,
    partBSixMonths,
    effectiveAfter65: effective > t65Date
  }
}

export function getCarrierName(naic: string): string {
  console.log('getCarrierName', naic)
  const carrierMap: Record<string, string> = {
    "79413": "UnitedHealthcare",
    "78700": "Aetna",
    "72052": "Aetna",
    "68500": "Aetna",
    "60380": "Allstate",
    "82538": "Allstate",
    "60534": "Allstate",
    "20699": "Chubb"
  }
  return carrierMap[naic] || "Unknown"
}

export async function format_application(application: any) {
  const carrier = getCarrierName(application.naic)
  
  // Choose formatter based on carrier
  switch (carrier) {
    case 'UnitedHealthcare':
      return formatUHCApplication(application)
    case 'Aetna':
      return formatAetnaApplication(application)
    case 'Allstate':
      return formatAllstateApplication(application)
    case 'Chubb':
      return formatACEApplication(application)
    default:
      throw new Error(`Unsupported carrier: ${carrier}`)
  }
}

// Base formatter that extracts common fields
function extractBaseData(application: any) {
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data

  const applicantInfo = data.applicant_info || {}
  const medicareInfo = data.medicare_information || {}
  const paymentInfo = data.payment || {}
  const producerInfo = data.producer || {}
  
  return {
    applicant_info: {
      f_name: applicantInfo.f_name,
      l_name: applicantInfo.l_name,
      address_line1: applicantInfo.address_line1,
      zip5: applicantInfo.zip5,
      phone: formatPhoneNumber(applicantInfo.phone || applicantInfo.applicant_phone),
      applicant_dob: formatDate(applicantInfo.applicant_dob),
      gender: applicantInfo.gender,
      effective_date: formatDate(applicantInfo.effective_date),
      tobacco_usage: applicantInfo.tobacco_usage || false,
    },
    medicare_information: {
      medicare_information_claim_number: medicareInfo.medicareNumber,
      medicare_information_ssn: medicareInfo.max_ssn,
      medicare_part_a_coverage: !!medicareInfo.medicare_part_a,
      medicare_part_b_coverage: !!medicareInfo.medicare_part_b,
      medicare_part_a_eff_date: formatDate(medicareInfo.medicare_part_a),
      medicare_part_b_eff_date: formatDate(medicareInfo.medicare_part_b),
      enroll_part_b_more_than_once: false,
      renal_failure: false,
      electronic_combined: false,
      apply_guaranteed_issue: false
    },
    producer: {
      business_type: "new",
      has_other_inforce_policies: false,
      deliver_policy_to: "APP",
      policy_delivery_type: "paper",
      replacement_notice_copy: true,
      electronic_combined: false
    },
    payment: paymentInfo,
  }
}

function formatUHCApplication(application: any) {
  const baseData = extractBaseData(application)
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data

  const formatted_data = {
    applicant_info: {
      ...baseData.applicant_info,
      poa: true,
      enroll_kit: true
    },
    medicare_information: {
      ...baseData.medicare_information,
      medicare_active: true
    },
    producer: {
      policy_delivery_type: "Mail",
      // Add other UHC-specific producer fields
    }
  }

  if (!data.hhd_information) {
    formatted_data.hhd_information = { hhd: false }
  }

  return formatted_data
}

function formatAetnaApplication(application: any) {
  const baseData = extractBaseData(application)
  const { medicationInfo } = parseMedicationInfo(application)
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data

  const formatted_data = {
    applicant_info: baseData.applicant_info,
    medicare_information: {
      ...baseData.medicare_information,
      apply_guaranteed_issue: false
    },
    producer: {
      deliver_policy_to: "applicant",
      e_delivery: false,
      accurate_recording: true,
      interviewed_applicants: true,
      application_provided: true,
      replacement_notice_copy: true,
      agent_requests_split_commissions: false
    },
    medication_info: medicationInfo
  }

  if (!data.hhd_information) {
    formatted_data.hhd_information = {
      household_resident: false,
      household_resident_has_carrier: false
    }
  }

  return formatted_data
}

function formatAllstateApplication(application: any) {
  const baseData = extractBaseData(application)
  const { medicationInfo } = parseMedicationInfo(application)
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data

  const formatted_data = {
    ...baseData,
    producer: {
      sale: "internet",
      deliver_policy_to: "applicant",
      agent_reviewed: true,
      applicant_reviewed: true,
      replacement_notice_copy: true
    },
    payment: {
      ...baseData.payment,
      payment_mode: "monthly"
    },
    hhd_information: {
      hhd: false,
      activity_tracker: false
    },
    medication_info: medicationInfo
  }

  if (!data.hhd_information) {
    formatted_data.hhd_information = {
      hhd: false,
      activity_tracker: false
    }
  }

  return formatted_data
}

function formatACEApplication(application: any) {
  const baseData = extractBaseData(application)
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data

  const formatted_data = {
    applicant_info: {
      ...baseData.applicant_info,
      applicant_phone: baseData.applicant_info.phone,
      address_line2: data.applicant_info?.address_line2,
      address_city: data.applicant_info?.address_city,
      applicant_email: data.applicant_info?.applicant_email,
    },
    medicare_information: {
      ...baseData.medicare_information,
      Electronic_Combined: baseData.medicare_information.electronic_combined,
    },
    producer: {
      ...baseData.producer,
      producer_first_name: data.producer?.producer_first_name,
      producer_last_name: data.producer?.producer_last_name,
      producer_phone: data.producer?.producer_phone,
      producer_email: data.producer?.producer_email,
      agent_address_line1: data.producer?.agent_address_line1,
      agent_zip5: data.producer?.agent_zip5,
      agent_address_city: data.producer?.agent_address_city,
      agent_address_state: data.producer?.agent_address_state,
      Electronic_Combined: baseData.producer.electronic_combined,
    },
    payment: baseData.payment,
    hhd_information: { hhd: false },
    existing_coverage: data.existing_coverage || {},
    health_information: data.health_information || {},
    medication_information: data.medication_information || {},
  }

  // Remove undefined values and format dates
  return removeUndefinedAndFormatDates(formatted_data)
}

function removeUndefinedAndFormatDates(obj: any): any {
  if (Array.isArray(obj)) {
    return obj.map(removeUndefinedAndFormatDates)
  }
  if (typeof obj === 'object' && obj !== null) {
    return Object.fromEntries(
      Object.entries(obj)
        .filter(([_, v]) => v !== undefined)
        .map(([k, v]) => [k, removeUndefinedAndFormatDates(v)])
    )
  }
  if (typeof obj === 'string' && obj.includes('T00:00:00')) {
    return obj.split('T')[0]
  }
  return obj
}

// Helper to extract generic name from drug string
function extractGenericName(drugName: string): string {
  if (!drugName) return ''
  
  // Split on first uppercase word (SOL, TAB, etc)
  const parts = drugName.split(' ')
  const genericParts = []
  
  for (const part of parts) {
    if (part.toUpperCase() === part) {
      break // Stop at first uppercase word
    }
    genericParts.push(part)
  }

  return genericParts.join(' ')
}

// Helper to extract dosage from drug string 
function extractDosage(drugName: string): string {
  if (!drugName) return ''

  const parts = drugName.split(' ')
  const dosageParts = []
  let foundUpper = false
  
  for (const part of parts) {
    if (!foundUpper && part.toUpperCase() === part) {
      foundUpper = true
      continue // Skip the uppercase word itself
    }
    if (foundUpper) {
      dosageParts.push(part)
    }
  }

  return dosageParts.join(' ').replace('/', ';')
}

// Update parseMedicationInfo to use these helpers
function parseMedicationInfo(application: any) {
  const data = typeof application.data === 'string' ? 
    JSON.parse(application.data) : application.data
    
  if (!data.medication_information?.prescription_drug_list) {
    return { medicationInfo: null }
  }

  const prescriptions = data.medication_information.prescription_drug_list.map((rx: any) => {
    const drugName = rx.drug?.drugName || ''
    return {
      medName: extractGenericName(drugName),
      dosage: extractDosage(drugName),
      diagnosis: rx.diagnosis,
      frequency: rx.frequency,
      quantity: rx.quantity,
      using: true
    }
  })

  return {
    medicationInfo: {
      prescriptions
    }
  }
}

// Add other helper functions as needed