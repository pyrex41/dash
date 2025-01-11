// Add type declarations for Vite env variables
interface ImportMetaEnv {
    readonly VITE_LAPRO_USERNAME: string
    readonly VITE_LAPRO_PASSWORD: string
    readonly VITE_LAPRO_GRANT_TYPE: string
    readonly VITE_LAPRO_CLIENT_ID: string
    readonly VITE_LAPRO_CLIENT_SECRET: string
}

interface ImportMeta {
    readonly env: ImportMetaEnv
}

// Log to confirm script loading
console.log('Main script loading...');

import './style.css';
import { Elm } from './Main.elm';
import producerConfigJson from '../producer_config.json';

console.log('Imports completed');

// Convert producer config to the format expected by Elm
const producerConfig = producerConfigJson;

// Add token acquisition function
async function getLAProToken() {
    try {
        const response = await fetch('/api/lapro/token', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            }
        });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const auth = await response.json();
        // Set cookie with a reasonable expiry (e.g., 1 hour)
        const expiryDate = new Date();
        expiryDate.setTime(expiryDate.getTime() + (60 * 60 * 1000));
        document.cookie = `lapro_token=${auth.access_token}; expires=${expiryDate.toUTCString()}; path=/`;
        return auth.access_token;
    } catch (error) {
        console.error('Error getting LAPRO token:', error);
        return '';
    }
}

document.addEventListener('DOMContentLoaded', async () => {
    console.log('DOM loaded, initializing Elm...');
    const target = document.getElementById('app');
    
    if (!target) {
        console.error('Target element #app not found!');
        return;
    }

    console.log('Producer config TS >>>>>>>>>:', producerConfig);

    const producerConfigDb = await fetch('/api/producer-config')
        .then(response => response.json())
        .then(data => data.producers);

    console.log('Producer config DB >>>>>>>>>:', producerConfigDb);

    try {
        const app = Elm.Main.init({
            node: target,
            flags: { producers: producerConfigDb } //producerConfig.producers }
        });

        console.log('Elm app initialized');

        // Add logging to debug the port communication
        app.ports.requestApplication?.subscribe(async ({ id }) => {
            console.log('Requesting application:', id);
            try {
                const response = await fetch(`/api/applications/${id}`)
                if (!response.ok) {
                    throw new Error(`HTTP error! status: ${response.status}`)
                }
                const application = await response.json()
                // Add producer config to the application response
                console.log('Sending application:', application);
                app.ports.receiveApplication.send(application)
            } catch (error) {
                console.error('Error fetching application:', error)
                app.ports.receiveApplication.send({
                    error: "Failed to load application"
                })
            }
        });

        // Port handlers for interacting with Elm
        app.ports.requestRefresh?.subscribe(({ page, pageSize, searchTerm, hasContactFilter, naics }) => {
            console.log('Search params:', { 
                page,
                pageSize,
                searchTerm, 
                length: searchTerm?.length || 0,
                hasContactFilter,
                naics
            });
            const params = new URLSearchParams({
                page: page.toString(),
                pageSize: pageSize.toString(),
                searchTerm: searchTerm || '',
                hasContactFilter: hasContactFilter.toString()
            });
            if (naics && naics.length > 0) {
                naics.forEach(naic => params.append('naics', naic));
            }
            fetch(`/api/applications?${params.toString()}`)
                .then(response => response.json())
                .then(data => {
                    console.log('Data received');
                    app.ports.receiveApplications.send(data);
                })
                .catch(error => {
                    console.error('Error fetching applications:', error);
                    app.ports.receiveApplications.send({
                        applications: [],
                        pagination: {
                            total: 0,
                            page: 0,
                            pageSize: 20,
                            totalPages: 0
                        }
                    });
                });
        });

        app.ports.exportToCsv?.subscribe(({ searchTerm, hasContactFilter, hasCSGFilter }) => {
            window.location.href = `/api/applications/export?searchTerm=${searchTerm}&hasContactFilter=${hasContactFilter}&hasCSGFilter=${hasCSGFilter}`;
        });
        
        app.ports.saveApplication?.subscribe(({ id, data, medications }) => {
            console.log('Saving application:', { id, data, medications });
            
            fetch(`/api/applications/${id}/formatted`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ 
                    data,
                    rawMedications: medications
                })
            })
            .then(response => response.json())
            .then(result => {
                app.ports.saveApplicationResponse.send(result);
            })
            .catch(error => {
                console.error('Error saving application:', error);
                app.ports.saveApplicationResponse.send({
                    success: false,
                    error: 'Failed to save application'
                });
            });
        });

        app.ports.forceRefreshLAProToken?.subscribe(async () => {
            console.log('forceRefreshLAProToken port triggered');
            const newToken = await getLAProToken();
            if (newToken) {
                console.log('Successfully obtained new token, sending to Elm:', newToken);
                app.ports.getLAProTokenResponse.send(newToken);
            } else {
                console.error('Failed to obtain token');
                app.ports.getLAProTokenResponse.send('');
            }
        });

        app.ports.submitToCSG?.subscribe(([applicationId, producerId]: [string, number]) => {
            fetch(`/api/applications/${applicationId}/submit`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ producerId })
            })
            .then(response => response.json())
            .then(result => {
                app.ports.submitToCSGResponse.send(result);
            })
            .catch(error => {
                app.ports.submitToCSGResponse.send({
                    success: false,
                    error: error.message || 'Failed to submit to CSG'
                });
            });
        });

        // Add debug logging for port availability
        console.log('Available ports:', {
            getLAProToken: !!app.ports.getLAProToken,
            getLAProTokenResponse: !!app.ports.getLAProTokenResponse,
        });

        // Also verify the port is properly set up
        console.log('Port setup check:', {
            getLAProToken: typeof app.ports.getLAProToken,
            getLAProTokenResponse: typeof app.ports.getLAProTokenResponse,
        });
    } catch (error) {
        console.error('Error initializing Elm app:', error);
    }
});