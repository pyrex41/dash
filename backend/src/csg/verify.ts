import puppeteer, { Browser, Page } from 'puppeteer';
import axios from 'axios';
import { getToken, handleTokenError, makeCSGRequest } from './token';
import { getDb } from '../db';
import { eq } from 'drizzle-orm';
import { csgApplications, applications } from '../db/schema';

let browserInstance: Browser | null = null;
let lastLoginTime: number = 0;
const LOGIN_TIMEOUT = 1000 * 60 * 30; // 30 minutes
const LOAD_ASSETS = true; // Toggle for loading images, stylesheets and fonts

async function getBrowser(): Promise<Browser> {
  if (!browserInstance) {
    browserInstance = await puppeteer.launch({
      headless: true,
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
  }
  return browserInstance;
}

async function loginToCSG(page: Page, debug: boolean = false): Promise<void> {
  const log = debug ? console.log : () => {};
  
  log('Logging into CSG...');
  await page.goto('https://tools.csgactuarial.com/medicareschool/auth/signin', {
    waitUntil: 'networkidle0',
    timeout: 60000
  });

  await page.waitForSelector('input.input-group-top', { visible: true, timeout: 30000 });
  const usernameInput = await page.waitForSelector('input.input-group-top', { timeout: 30000 });
  await usernameInput?.type(process.env.CSG_USERNAME || 'josh@enlightnu.com');

  const passwordInput = await page.waitForSelector("input[type='password']", { timeout: 30000 });
  await passwordInput?.type(process.env.CSG_PASSWORD || 'Medicare#1');

  const signInButton = await page.waitForSelector('button', { timeout: 30000 });
  await signInButton?.click();

  await page.waitForNavigation({ 
    waitUntil: 'networkidle0',
    timeout: 60000
  });

  lastLoginTime = Date.now();
  log('Successfully logged in and updated session');
}

// Initialize CSG session
export async function initializeCSG(debug: boolean = false): Promise<void> {
  const log = debug ? console.log : () => {};
  log('Initializing CSG session...');
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    await loginToCSG(page, debug);
    log('CSG session initialized successfully');
  } catch (error) {
    console.error('Failed to initialize CSG session:', error);
    throw error;
  } finally {
    await page.close();
  }
}

export async function getAuthenticatedPage(debug: boolean = false): Promise<Page> {
  const log = debug ? console.log : () => {};
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  // Set up timeouts
  const timeout = 60000;
  page.setDefaultTimeout(timeout);
  page.setDefaultNavigationTimeout(timeout);

  // Set up request interception
  await page.setRequestInterception(true);
  page.on('request', (request) => {
    if (!LOAD_ASSETS && ['image', 'stylesheet', 'font'].includes(request.resourceType())) {
      request.abort();
    } else {
      request.continue();
    }
  });

  // Check if we need to login again
  const now = Date.now();
  if (now - lastLoginTime > LOGIN_TIMEOUT) {
    log('Session expired or not logged in. Logging in...');
    await loginToCSG(page, debug);
  } else {
    log('Using existing session');
  }

  return page;
}

// New function to handle Chubb zip code workaround
export async function fixChubbZipCode(page: Page, urlSlug: string, debug: boolean = false): Promise<void> {
  const log = debug ? console.log : () => {};
  
  try {
    log('Applying Chubb zip code workaround...');
    
    // Get the zip code from our database
    const db = getDb();
    const [csgApp] = await db
      .select()
      .from(csgApplications)
      .where(eq(csgApplications.key, urlSlug));

    if (!csgApp) {
      throw new Error('CSG application not found in database');
    }

    const [application] = await db
      .select()
      .from(applications)
      .where(eq(applications.id, csgApp.applicationId));

    if (!application) {
      throw new Error('Application not found in database');
    }

    const formattedData = application.formattedData as Record<string, any>;
    const zipCode = formattedData?.applicant_info?.zip5;
    if (!zipCode) {
      throw new Error('Zip code not found in application formatted data');
    }

    log(`Found zip code ${zipCode} for application ${application.id}`);
    
    // Navigate to the application page first
    const applicationUrl = `https://eapp.csgactuarial.com/applications/${urlSlug}`;
    log(`Navigating to application page: ${applicationUrl}`);
    await page.goto(applicationUrl, {
      waitUntil: 'networkidle0',
      timeout: 60000
    });
    log('Successfully loaded application page');

    // Wait for the content to load
    log('Waiting for content to load...');
    await page.waitForSelector('#content', { timeout: 30000 });
    log('Content loaded');

    // Find and click the zip code field container
    const zipCodeSelector = 'div[id*="string_search_field-section-applicant_info-field-zip5"]';
    log('Waiting for zip code field...');
    await page.waitForSelector(zipCodeSelector);
    log('Clicking zip code field...');
    await page.click(zipCodeSelector);
    log('Clicked zip code field');

    // Wait for and find the input field
    const inputSelector = '#react-select-2-input';
    log('Waiting for zip code input field...');
    await page.waitForSelector(inputSelector);
    log('Found zip code input field');

    // Fill in the zip code
    log(`Typing zip code: ${zipCode}`);
    await page.type(inputSelector, zipCode);
    log('Finished typing zip code');

    // Click the continue button
    log('Looking for continue button...');
    const continueButton = await page.waitForSelector('#content button');
    log('Clicking continue button...');
    await continueButton?.click();
    log('Clicked continue button');

    // Wait for navigation to complete
    log('Waiting for navigation after continue...');
    await page.waitForNavigation({ waitUntil: 'networkidle0' });
    log('Navigation complete');

    log('Successfully applied Chubb zip code workaround');
  } catch (error) {
    console.error('Error applying Chubb zip code workaround:', error);
    throw error;
  }
}

interface VerifyOptions {
  headless?: boolean;
  slowMo?: number;
  debug?: boolean;
}

interface CSGApplicationData {
  in_good_order: boolean;
  naic?: string;
  [key: string]: any;
}

export async function verifyCSGApplication(urlSlug: string, options: VerifyOptions = {}) {
  const {
    debug = false
  } = options;

  const log = debug ? console.log : () => {};
  
  try {
    const page = await getAuthenticatedPage(debug);
    
    log('Setting initial viewport...');
    await page.setViewport({
      width: 2166,
      height: 1363
    });

    // Fetch the application data to check NAIC
    const applicationData = await makeCSGRequest<CSGApplicationData>({
      method: 'GET',
      url: `/v1/e_app/enrollment_applications/${urlSlug}.json`
    });

    // If this is a Chubb application, apply the zip code workaround
    if (applicationData.naic === '20699') {
      await fixChubbZipCode(page, urlSlug, debug);
    }

    const verifyUrl = `https://eapp.csgactuarial.com/applications/${urlSlug}/verify`;
    log(`Navigating to: ${verifyUrl}`);
    
    await page.goto(verifyUrl, {
      waitUntil: 'networkidle0',
      timeout: 60000
    });

    try {
      log('Waiting for page content...');
      await page.waitForSelector('#content', { 
        visible: true,
        timeout: 30000 
      });

      // Ensure the page is fully rendered
      log('Waiting additional time for rendering...'); 
      await new Promise(resolve => setTimeout(resolve, 5000));

      // Set a very tall viewport to capture everything
      log('Setting tall viewport for full page capture...');
      await page.setViewport({
        width: 2166,
        height: 5000  // Very tall to capture everything
      });

      // Always take screenshot now
      log('Taking screenshot...');
      const screenshot = await page.screenshot({
        fullPage: true,
        encoding: 'base64'
      });

      // Check for yellow highlighted error elements
      const hasErrors = await page.evaluate(() => {
        const elements = document.querySelectorAll('*');
        for (const element of elements) {
          const style = window.getComputedStyle(element);
          const backgroundColor = style.backgroundColor;
          // Check for yellow background (could be rgba or hex)
          if (backgroundColor.includes('255, 255, 0') || backgroundColor === 'rgb(255, 255, 0)' || backgroundColor === '#ffff00') {
            return true;
          }
        }
        return false;
      });

      // Fetch the application data to check in_good_order
      const applicationData = await makeCSGRequest<CSGApplicationData>({
        method: 'GET',
        url: `/v1/e_app/enrollment_applications/${urlSlug}.json`
      });

      const inGoodOrder = applicationData.in_good_order === true;
      const verificationStatus = inGoodOrder && !hasErrors ? 'verified' : 'failed';
      const verificationError = hasErrors ? 'Verification page shows highlighted errors' : !inGoodOrder ? 'Application is not in good order' : null;

      // Save verification results to database
      const db = getDb();
      await db.update(csgApplications)
        .set({
          verificationStatus,
          verificationScreenshot: screenshot,
          verificationError,
          lastVerifiedAt: new Date(),
          updatedAt: new Date()
        })
        .where(eq(csgApplications.key, urlSlug));

      if (hasErrors || !inGoodOrder) {
        return { 
          success: false, 
          screenshot: screenshot,
          verifyUrl,
          error: verificationError
        };
      }

      return { 
        success: true, 
        screenshot: screenshot,
        verifyUrl
      };
    } catch (error) {
      if (debug) {
        // Ensure tall viewport for error screenshot too
        await page.setViewport({
          width: 2166,
          height: 5000
        });
        const errorScreenshot = await page.screenshot({
          fullPage: true,
          encoding: 'base64'
        });

        // Save error state to database
        const db = getDb();
        await db.update(csgApplications)
          .set({
            verificationStatus: 'failed',
            verificationScreenshot: errorScreenshot,
            verificationError: error instanceof Error ? error.message : 'Unknown error occurred',
            lastVerifiedAt: new Date(),
            updatedAt: new Date()
          })
          .where(eq(csgApplications.key, urlSlug));

        return {
          success: false,
          screenshot: errorScreenshot,
          verifyUrl,
          error: error instanceof Error ? error.message : 'Unknown error occurred'
        };
      }
      throw error;
    } finally {
      await page.close(); // Close just this page, not the browser
    }
  } catch (error) {
    console.error('Verification failed:', error);
    throw new Error(`Verification failed: ${error.message}`);
  }
} 

// Cleanup function to close browser on server shutdown
export async function cleanup() {
  if (browserInstance) {
    await browserInstance.close();
    browserInstance = null;
  }
} 