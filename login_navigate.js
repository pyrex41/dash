
const puppeteer = require('puppeteer'); // v23.0.0 or later

(async () => {
    const browser = await puppeteer.launch({
        headless: false,
        slowMo: 50
    });
    const page = await browser.newPage();
    const timeout = 5000;
    page.setDefaultTimeout(timeout);

    {
        const targetPage = page;
        await targetPage.setViewport({
            width: 2166,
            height: 1363
        })
    }
    {
        const targetPage = page;
        await targetPage.goto('https: //tools.csgactuarial.com/medicareschool/auth/signin');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Username)'),
            targetPage.locator('input.input-group-top'),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                1
            ])'),
            targetPage.locator(':scope >>> input.input-group-top'),
            targetPage.locator(': :-p-text(josh@enlightnu.com)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 162,
                y: 27.609375,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Username)'),
            targetPage.locator('input.input-group-top'),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                1
            ])'),
            targetPage.locator(':scope >>> input.input-group-top'),
            targetPage.locator(': :-p-text(josh@enlightnu.com)')
        ])
            .setTimeout(timeout)
            .click({
              count: 2,
              offset: {
                x: 164,
                y: 24.609375,
            },
        });
    }
    {
        const targetPage = page;
        await targetPage.keyboard.down('Meta');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.down('a');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.up('Meta');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.up('a');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Username)'),
            targetPage.locator('input.input-group-top'),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                1
            ])'),
            targetPage.locator(':scope >>> input.input-group-top'),
            targetPage.locator(': :-p-text(josh@enlightnu.com)')
        ])
            .setTimeout(timeout)
            .fill('josh@');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.up('2');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Username)'),
            targetPage.locator('input.input-group-top'),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                1
            ])'),
            targetPage.locator(':scope >>> input.input-group-top'),
            targetPage.locator(': :-p-text(josh@enlightnu.com)')
        ])
            .setTimeout(timeout)
            .fill('josh@enlightnu.com');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.down('Tab');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.up('Tab');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Password)'),
            targetPage.locator("input[type='password']"),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                2
            ])'),
            targetPage.locator(":scope >>> input[type='password']"),
            targetPage.locator(': :-p-text(Medicare#1)')
        ])
            .setTimeout(timeout)
            .fill('M');
    }
    {
        const targetPage = page;
        await targetPage.keyboard.up('m');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Password)'),
            targetPage.locator("input[type='password']"),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/input[
                2
            ])'),
            targetPage.locator(":scope >>> input[type='password']"),
            targetPage.locator(': :-p-text(Medicare#1)')
        ])
            .setTimeout(timeout)
            .fill('Medicare#1');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(Sign in)'),
            targetPage.locator('button'),
            targetPage.locator(': :-p-xpath(/html/body/div[
                1
            ]/div[
                2
            ]/div/form/div/div[
                1
            ]/button)'),
            targetPage.locator(':scope >>> button'),
            targetPage.locator(': :-p-text(Sign in)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 172,
                y: 7.609375,
            },
        });
    }
    {
        const targetPage = page;
        await targetPage.goto('https: //eapp.csgactuarial.com/applications/agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKzm2f2rCAyiAQVlX2FwcA/');
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator("[data-testid='prelude']"),
            targetPage.locator(': :-p-xpath( //*[@data-testid=\\"prelude\\"])'),
            targetPage.locator(":scope >>> [data-testid='prelude']"),
            targetPage.locator(': :-p-text(Select Underwriting)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 125,
                y: 20.453125,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator("[data-testid='prelude']"),
            targetPage.locator(': :-p-xpath( //*[@data-testid=\\"prelude\\"])'),
            targetPage.locator(":scope >>> [data-testid='prelude']"),
            targetPage.locator(': :-p-text(Select Underwriting)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 147,
                y: 27.453125,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator('div:nth-of-type(4) div:nth-of-type(2) > label'),
            targetPage.locator(': :-p-xpath( //*[@id=\\"underwritingType-agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKzm2f2rCAyiAQVlX2FwcA\\"]/div[2]/label)'),
            targetPage.locator(':scope >>> div:nth-of-type(4) div:nth-of-type(2) > label'),
            targetPage.locator(': :-p-text(Open Enrollment)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 41,
                y: 9.609375,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator('#underwritingType-agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKzm2f2rCAyiAQVlX2FwcA > div:nth-of-type(1) > label'),
            targetPage.locator(': :-p-xpath( //*[@id=\\"underwritingType-agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKzm2f2rCAyiAQVlX2FwcA\\"]/div[1]/label)'),
            targetPage.locator(':scope >>> #underwritingType-agZjc2dhcGlyIgsSFUVucm9sbG1lbnRBcHBsaWNhdGlvbhiAgKzm2f2rCAyiAQVlX2FwcA > div:nth-of-type(1) > label')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 52,
                y: 6.609375,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(  Continue)'),
            targetPage.locator('div.fixed button'),
            targetPage.locator(': :-p-xpath( //*[@id=\\"content\\"]/div/div/div[7]/div/div/div/div/button)'),
            targetPage.locator(':scope >>> div.fixed button'),
            targetPage.locator(': :-p-text(Continue)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 81.109375,
                y: 28,
            },
        });
    }
    {
        const targetPage = page;
        await puppeteer.Locator.race([
            targetPage.locator(': :-p-aria(  Continue)'),
            targetPage.locator('div.fixed button'),
            targetPage.locator(': :-p-xpath( //*[@id=\\"content\\"]/div/div/div[7]/div/div/div/div/button)'),
            targetPage.locator(':scope >>> div.fixed button'),
            targetPage.locator(': :-p-text(Continue)')
        ])
            .setTimeout(timeout)
            .click({
              offset: {
                x: 34.109375,
                y: 27,
            },
        });
    }
    {
        const targetPage = page;
        const promises = [];
        const startWaitingForEvents = () => {
            promises.push(targetPage.waitForNavigation());
        }
        await puppeteer.Locator.race([
            targetPage.locator("[data-testid='review-lock-application']"),
            targetPage.locator(': :-p-xpath( //*[@data-testid=\\"review-lock-application\\"])'),
            targetPage.locator(":scope >>> [data-testid='review-lock-application']"),
            targetPage.locator(': :-p-text(Review and Lock)')
        ])
            .setTimeout(timeout)
            .on('action', () => startWaitingForEvents())
            .click({
              offset: {
                x: 93,
                y: 26.453125,
            },
        });
        await Promise.all(promises);
    }

    await browser.close();
})().catch(err => {
    console.error(err);
    process.exit(1);
});
