import puppeteer, { Browser, Page } from 'puppeteer';
import axios from 'axios';
import { getToken, handleTokenError, makeCSGRequest } from './token';
import { getDb } from '../db';
import { eq } from 'drizzle-orm';
import { csgApplications, applications } from '../db/schema';
import { broadcastVerificationUpdate } from '../index';

let browserInstance: Browser | null = null;
let lastLoginTime: number = 0;
const LOGIN_TIMEOUT = 1000 * 60 * 30; // 30 minutes
const LOAD_ASSETS = true; // Toggle for loading images, stylesheets and fonts
const VERIFICATION_TIMEOUT = 120000; // 2 minutes timeout

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

    // Find and click the zip code field container - updated selector for React Select
    const zipCodeSelector = '.css-13cymwt-control';
    log('Waiting for zip code field...');
    await page.waitForSelector(zipCodeSelector, { timeout: 30000 });
    log('Clicking zip code field...');
    await page.click(zipCodeSelector);
    log('Clicked zip code field');

    // Wait for and find the input field - updated selector for React Select
    const inputSelector = '#react-select-2-input';
    log('Waiting for zip code input field...');
    await page.waitForSelector(inputSelector, { timeout: 30000 });
    log('Found zip code input field');

    // Clear any existing value first
    log('Clearing existing zip code...');
    for (let i = 0; i < 5; i++) {
      await page.keyboard.press('Backspace');
    }
    log('Cleared existing zip code');

    // Fill in the zip code
    log(`Typing zip code: ${zipCode}`);
    await page.type(inputSelector, zipCode);
    log('Finished typing zip code');

    // Wait for 3 seconds after entering zip code
    log('Waiting 3 seconds...');
    await new Promise(resolve => setTimeout(resolve, 3000));
    log('Wait complete');

    // Click the continue button
    log('Looking for continue button...');
    const continueButton = await page.waitForSelector('#content button');
    log('Clicking continue button...');
    await continueButton?.click();
    log('Clicked continue button');

    // Navigate to the prelude page
    const preludeUrl = `https://eapp.csgactuarial.com/applications/${urlSlug}/med_supp_tool/prelude`;
    log(`Navigating to prelude page: ${preludeUrl}`);
    await page.goto(preludeUrl, {
      waitUntil: 'networkidle0',
      timeout: 60000
    });
    log('Successfully loaded prelude page');

    // Verify the zip code was set correctly
    log('Verifying zip code was set correctly...');
    const updatedData = await makeCSGRequest<any>({
      method: 'GET',
      url: `/v1/e_app/enrollment_applications/${urlSlug}.json`
    });

    const updatedZip = updatedData?.values?.applicant_info?.zip5;
    if (updatedZip !== zipCode) {
      console.warn(`⚠️ Zip code verification failed. Expected ${zipCode} but got ${updatedZip || 'undefined'}`);
    } else {
      log(`✓ Zip code verified: ${updatedZip}`);
    }

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

interface VerificationResult {
  success: boolean;
  screenshot: string | null;
  verifyUrl: string | null;
  error: string | null;
}

interface VerificationResponse {
  success: boolean;
  screenshot: string | null;
  verifyUrl: string | null;
  error: string | null;
}

export async function verifyCSGApplication(urlSlug: string, options: VerifyOptions = {}): Promise<VerificationResponse> {
  const {
    debug = false
  } = options;

  const log = debug ? console.log : () => {};
  
  try {
    const page = await getAuthenticatedPage(debug);
    
    // Create a timeout promise
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error('Verification timed out after 2 minutes'));
      }, VERIFICATION_TIMEOUT);
    });

    // Create the verification promise
    const verificationPromise = (async () => {
      try {
        log('Setting initial viewport...');
        await page.setViewport({
          width: 2166,
          height: 1363
        });

        // Fetch the application data to check NAIC
        const initialAppData = await makeCSGRequest<CSGApplicationData>({
          method: 'GET',
          url: `/v1/e_app/enrollment_applications/${urlSlug}.json`
        });

        // If this is a Chubb application, apply the zip code workaround
        if (initialAppData.naic === '20699' && !/^\d{5}$/.test(initialAppData.values.applicant_info.zip5)) {
          await fixChubbZipCode(page, urlSlug, debug);
        }

        const verifyUrl = `https://eapp.csgactuarial.com/applications/${urlSlug}/verify`;
        log(`Navigating to: ${verifyUrl}`);
        
        await page.goto(verifyUrl, {
          waitUntil: 'networkidle0',
          timeout: 60000
        });

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
        }) as string;

        // Click the E-sign button (handles different text variations)
        log('Looking for E-sign button...');
        const buttonSelectors = [
          '#e_sign',
          'button:has-text("Continue to E-Sign")',
          'button:has-text("Lock and E-Sign")',
          'button:has-text("Lock and Esign")',
          'button:has-text("Continue to Esign")'
        ];

        // Try each selector until we find one that works
        let clicked = false;
        for (const selector of buttonSelectors) {
          try {
            const button = await page.waitForSelector(selector, { timeout: 1000 });
            if (button) {
              log(`Found button with selector: ${selector}`);
              await button.click();
              clicked = true;
              break;
            }
          } catch (error) {
            // Button not found with this selector, try next one
            continue;
          }
        }

        if (!clicked) {
          log('Warning: Could not find E-sign button with any selector');
        } else {
          log('Clicked E-sign button');
          // Wait for navigation after click
          await page.waitForNavigation({ timeout: 30000 }).catch(error => {
            log('Warning: Navigation timeout after clicking E-sign button');
          });
        }

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
        const finalAppData = await makeCSGRequest<CSGApplicationData>({
          method: 'GET',
          url: `/v1/e_app/enrollment_applications/${urlSlug}.json`
        });

        const inGoodOrder = finalAppData.in_good_order === true;
        const verificationStatus = inGoodOrder && !hasErrors ? 'verified' : 'failed';
        const verificationError = hasErrors ? 'Verification page shows highlighted errors' : !inGoodOrder ? 'Application is not in good order' : null;

        // First update application status to verifying
        const db = getDb();
        await db.update(applications)
          .set({
            status: 'verifying',
            updatedAt: new Date()
          })
          .where(
            eq(applications.id, 
              db.select({ id: applications.id })
                .from(applications)
                .innerJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
                .where(eq(csgApplications.key, urlSlug))
                .limit(1)
            )
          );

        // Then save verification results to database
        await Promise.all([
          db.update(csgApplications)
            .set({
              verificationStatus,
              verificationScreenshot: screenshot,
              verificationError,
              lastVerifiedAt: new Date(),
              updatedAt: new Date()
            })
            .where(eq(csgApplications.key, urlSlug)),
          
          // Update final application status
          db.update(applications)
            .set({
              status: inGoodOrder && !hasErrors ? 'awaiting_signature' : 'submission_issue',
              updatedAt: new Date()
            })
            .where(
              eq(applications.id, 
                db.select({ id: applications.id })
                  .from(applications)
                  .innerJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
                  .where(eq(csgApplications.key, urlSlug))
                  .limit(1)
              )
            )
        ]);

        // Get the application ID for broadcasting
        const [csgApp] = await db
          .select()
          .from(csgApplications)
          .where(eq(csgApplications.key, urlSlug));

        if (csgApp?.applicationId) {
          // First broadcast verifying status
          broadcastVerificationUpdate(csgApp.applicationId, {
            status: 'verifying',
            key: urlSlug,
            applicationStatus: 'verifying'
          });

          // Then broadcast final verification status
          broadcastVerificationUpdate(csgApp.applicationId, {
            status: verificationStatus,
            key: urlSlug,
            error: verificationError,
            screenshot: screenshot,
            applicationStatus: inGoodOrder && !hasErrors ? 'awaiting_signature' : 'submission_issue'
          });
        }

        if (hasErrors || !inGoodOrder) {
          return { 
            success: false, 
            screenshot,
            verifyUrl,
            error: verificationError || 'Unknown verification error'
          } satisfies VerificationResponse;
        }

        return { 
          success: true, 
          screenshot,
          verifyUrl,
          error: null
        } satisfies VerificationResponse;
      } catch (error) {
        // Handle errors during verification
        const db = getDb();
        const errorMessage = error instanceof Error ? error.message : 'Unknown error occurred';
        
        // Take error screenshot if possible
        let errorScreenshot: string | null = null;
        try {
          await page.setViewport({
            width: 2166,
            height: 5000
          });
          const screenshot = await page.screenshot({
            fullPage: true,
            encoding: 'base64'
          });
          errorScreenshot = screenshot as string;
        } catch (screenshotError) {
          log('Failed to capture error screenshot:', screenshotError);
        }

        // Save error state to database
        await Promise.all([
          db.update(csgApplications)
            .set({
              verificationStatus: 'failed',
              verificationScreenshot: errorScreenshot,
              verificationError: errorMessage,
              lastVerifiedAt: new Date(),
              updatedAt: new Date()
            })
            .where(eq(csgApplications.key, urlSlug)),
          
          // Update application status
          db.update(applications)
            .set({
              status: 'submission_issue',
              updatedAt: new Date()
            })
            .where(
              eq(applications.id, 
                db.select({ id: applications.id })
                  .from(applications)
                  .innerJoin(csgApplications, eq(applications.id, csgApplications.applicationId))
                  .where(eq(csgApplications.key, urlSlug))
                  .limit(1)
              )
            )
        ]);

        // Get application ID for broadcasting error
        const [errorCsgApp] = await db
          .select()
          .from(csgApplications)
          .where(eq(csgApplications.key, urlSlug));

        if (errorCsgApp?.applicationId) {
          // Broadcast verification failure
          broadcastVerificationUpdate(errorCsgApp.applicationId, {
            status: 'failed',
            key: urlSlug,
            error: errorMessage,
            applicationStatus: 'submission_issue'
          });
        }

        return {
          success: false,
          screenshot: errorScreenshot,
          verifyUrl: null,
          error: errorMessage
        } satisfies VerificationResponse;
      } finally {
        await page.close();
      }
    })();

    // Race between verification and timeout
    const result = await Promise.race([verificationPromise, timeoutPromise]) as VerificationResponse;
    return result;
  } catch (error) {
    // Handle timeout or other errors
    const errorMessage = error instanceof Error ? error.message : 'Unknown error occurred';
    console.error('Verification failed:', errorMessage);
    
    // Update database with timeout/error status
    const db = getDb();
    await db.update(csgApplications)
      .set({
        verificationStatus: 'failed',
        verificationError: errorMessage,
        lastVerifiedAt: new Date(),
        updatedAt: new Date()
      })
      .where(eq(csgApplications.key, urlSlug));

    return {
      success: false,
      screenshot: null,
      verifyUrl: null,
      error: errorMessage
    } satisfies VerificationResponse;
  }
} 

// Cleanup function to close browser on server shutdown
export async function cleanup() {
  if (browserInstance) {
    await browserInstance.close();
    browserInstance = null;
  }
} 