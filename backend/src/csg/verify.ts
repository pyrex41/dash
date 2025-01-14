import puppeteer, { Browser, Page } from 'puppeteer';
import axios from 'axios';
import { getToken, handleTokenError, makeCSGRequest } from './token';

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

async function getAuthenticatedPage(debug: boolean = false): Promise<Page> {
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

interface VerifyOptions {
  headless?: boolean;
  slowMo?: number;
  debug?: boolean;
}

interface CSGApplicationData {
  in_good_order: boolean;
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

      if (hasErrors || !inGoodOrder) {
        return { 
          success: false, 
          screenshot: screenshot,
          verifyUrl,
          error: hasErrors ? 'Verification page shows highlighted errors' : 'Application is not in good order'
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