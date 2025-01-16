const puppeteer = require('puppeteer'); // v22.0.0 or later

(async () => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    const timeout = 5000;
    page.setDefaultTimeout(timeout);

    {
        const targetPage = page;
        await targetPage.setViewport({
            width: 2592,
            height: 1309
        })
    }
    {
        const targetPage = page;
        const promises = [];
        const startWaitingForEvents = () => {
            promises.push(targetPage.waitForNavigation());
        }
        startWaitingForEvents();
        await targetPage.goto('https://eapp.csgactuarial.com/');
        await Promise.all(promises);
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator('div.css-19bb58m'),
            targetPage.locator('::-p-xpath(//*[@id=\\"string_search_field-section-applicant_info-field-zip5-index-0-string-search-select\\"]/div/div[1]/div[2])'),
            targetPage.locator(':scope >>> div.css-19bb58m')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 43,
                y: 13.34375,
              },
            });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator('#react-select-2-input'),
            targetPage.locator('::-p-xpath(//*[@id=\\"react-select-2-input\\"])'),
            targetPage.locator(':scope >>> #react-select-2-input')
        ])
            .setTimeout(timeout)
            .fill('66210');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator('::-p-aria( Continue)'),
            targetPage.locator('#content button'),
            targetPage.locator('::-p-xpath(//*[@id=\\"content\\"]/div/div/div[3]/div/div/div/div/button)'),
            targetPage.locator(':scope >>> #content button'),
            targetPage.locator('::-p-text(Continue)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 54.125,
                y: 23,
              },
            });
    }
    {
        const targetPage = page;
        const promises = [];
        const startWaitingForEvents = () => {
            promises.push(targetPage.waitForNavigation());
        }
        startWaitingForEvents();
        await targetPage.goto('https://eapp.csgactuarial.com/applications/agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKy2j__DCQyiAQVlX2FwcA/verify');
        await Promise.all(promises);
    }

    await browser.close();

})().catch(err => {
    console.error(err);
    process.exit(1);
});
