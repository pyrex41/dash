// utils.ts

export function formatPhoneNumber(phone: string): { 
  area_code: string;
  central_office_code: string;
  station_code: string;
} {
  const digits = phone.replace(/\D/g, '');
  return {
    area_code: digits.slice(0, 3),
    central_office_code: digits.slice(3, 6),
    station_code: digits.slice(6, 10)
  };
}

export function formatDate(date: string): string {
  if (!date) return '';
  if (date.includes('T')) {
    return date.split('T')[0];
  }
  return date;
}

export function extractGenericName(drugName: string): string {
  const parts = drugName.split(' ');
  const index = parts.findIndex(part => part.toUpperCase() === part);
  return parts.slice(0, index).join(' ');
}

export function extractDosage(drugName: string): string {
  const parts = drugName.split(' ');
  const index = parts.findIndex(part => part.toUpperCase() === part);
  return parts.slice(index + 1).join(' ').replace('/', ';');
}

export function calculateAge(dob: string, currentTime?: Date): number {
  if (!dob) return 0;
  
  const birthDate = new Date(dob);
  const now = currentTime || new Date();
  
  let age = now.getFullYear() - birthDate.getFullYear();
  const monthDiff = now.getMonth() - birthDate.getMonth();
  
  if (monthDiff < 0 || (monthDiff === 0 && now.getDate() < birthDate.getDate())) {
    age--;
  }
  
  return age;
}