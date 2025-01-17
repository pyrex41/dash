// Add type declarations for Vite env variables
interface ImportMetaEnv {
    readonly VITE_LAPRO_USERNAME: string
    readonly VITE_LAPRO_PASSWORD: string
    readonly VITE_LAPRO_GRANT_TYPE: string
    readonly VITE_LAPRO_CLIENT_ID: string
    readonly VITE_LAPRO_CLIENT_SECRET: string
    readonly VITE_API_WS_URL: string
}

interface ImportMeta {
    readonly env: ImportMetaEnv;
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

async function getProducerConfig(retries = 3, delay = 1000): Promise<any> {
    try {
        const response = await fetch('/api/producer-config');
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        const data = await response.json();
        return data.producers;
    } catch (error) {
        if (retries > 0) {
            console.log(`Failed to fetch producer config, retrying... (${retries} attempts left)`);
            await new Promise(resolve => setTimeout(resolve, delay));
            return getProducerConfig(retries - 1, delay * 1.5);
        }
        throw error;
    }
}

// Add polling functionality for verification status
async function pollVerificationStatus(applicationId: string, key: string, app: any) {
    let attempts = 0;
    const maxAttempts = 24; // 2 minutes (5s * 24)
    const pollInterval = 5000; // 5 seconds

    const poll = async () => {
        if (attempts >= maxAttempts) {
            app.ports.verificationReceived.send([applicationId, {
                success: false,
                error: 'Verification timed out',
                screenshot: null,
                verifyUrl: null,
                verificationStatus: 'failed'
            }]);
            return;
        }

        try {
            const response = await fetch(`/api/csg-application/${key}/verify`);
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const result = await response.json();

            // If verification is complete or failed, send the result
            if (result.success || result.error) {
                app.ports.verificationReceived.send([applicationId, {
                    success: result.success,
                    error: result.error || null,
                    screenshot: result.screenshot,
                    verifyUrl: result.verifyUrl,
                    verificationStatus: result.success ? 'verified' : 'failed'
                }]);
                return;
            }

            // Otherwise, continue polling
            attempts++;
            setTimeout(poll, pollInterval);
        } catch (error) {
            app.ports.verificationReceived.send([applicationId, {
                success: false,
                error: error instanceof Error ? error.message : 'Failed to verify application',
                screenshot: null,
                verifyUrl: null,
                verificationStatus: 'failed'
            }]);
        }
    };

    // Start polling
    poll();
}

// Add polling functionality for all pending verifications
async function pollPendingVerifications(app: any) {
    const pollInterval = 5000; // 5 seconds

    const poll = async () => {
        try {
            const response = await fetch('/api/applications');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const data = await response.json();
            app.ports.receiveApplications.send(data);

            // Check if there are any pending verifications
            const hasPending = data.applications.some(
                (app: any) => app.csgApplication?.verificationStatus === 'pending'
            );

            // Continue polling if there are pending verifications
            if (hasPending) {
                setTimeout(poll, pollInterval);
            }
        } catch (error) {
            console.error('Error polling applications:', error);
            // On error, try again after interval
            setTimeout(poll, pollInterval);
        }
    };

    // Start polling
    poll();
}

// WebSocket connection handler
function setupWebSocket(app: any) {
    let wsUrl = import.meta.env.VITE_API_WS_URL;
    
    // If the URL is relative (starts with /), make it absolute
    if (wsUrl?.startsWith('/')) {
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        wsUrl = `${wsProtocol}//${window.location.host}${wsUrl}`;
    }
    
    console.log('Connecting to WebSocket:', wsUrl);
    
    const socket = new WebSocket(wsUrl, ['json']);
    
    // Add connection state tracking
    let isConnected = false;
    let reconnectAttempts = 0;
    const maxReconnectAttempts = 5;
    const baseReconnectDelay = 1000; // Start with 1 second
    
    socket.onopen = () => {
        console.log('WebSocket connected');
        isConnected = true;
        reconnectAttempts = 0; // Reset reconnect attempts on successful connection
    };
    
    socket.onmessage = (event) => {
        try {
            const message = JSON.parse(event.data);
            console.log('WebSocket message received:', message);
            
            // Handle ping/pong
            if (message.type === 'ping') {
                socket.send(JSON.stringify({ type: 'pong' }));
                return;
            }
            
            if (message.type === 'verification_update') {
                // First, send the verification update through verificationReceived port
                if (app.ports?.verificationReceived?.send) {
                    // Construct verification result matching the Elm decoder
                    const verificationResult = {
                        applicationId: message.applicationId,
                        key: message.body.csg_id,
                        verificationStatus: message.body.status === 'verified',
                        verificationError: message.body.error || null,
                        verificationScreenshot: message.body.screenshot || null,
                        lastVerifiedAt: new Date().toISOString(),
                    };
                    console.log('Sending verification update to Elm:', verificationResult);
                    
                    app.ports.verificationReceived.send(verificationResult);
                } else {
                    console.error('verificationReceived port not available');
                }

                // Then, refresh the applications list to update the UI
                /*
                if (app.ports?.requestRefresh?.send) {
                    // Get current search params from URL
                    const urlParams = new URLSearchParams(window.location.search);
                    const currentState = {
                        page: Number(urlParams.get('page')) || 0,
                        pageSize: Number(urlParams.get('pageSize')) || 20,
                        searchTerm: urlParams.get('searchTerm') || '',
                        hasContactFilter: urlParams.get('hasContactFilter') === 'true',
                        naics: urlParams.getAll('naics')
                    };
                    
                    console.log('Requesting refresh with state:', currentState);
                    app.ports.requestRefresh.send(currentState);
                }
                */
            }
        } catch (error) {
            console.error('Error handling WebSocket message:', error);
        }
    };
    
    socket.onclose = (event) => {
        console.log(`WebSocket disconnected with code ${event.code}`, event.reason);
        isConnected = false;
        
        if (reconnectAttempts < maxReconnectAttempts) {
            // Exponential backoff for reconnection
            const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
            console.log(`Attempting to reconnect in ${delay}ms... (Attempt ${reconnectAttempts + 1}/${maxReconnectAttempts})`);
            
            setTimeout(() => {
                if (!isConnected) {
                    reconnectAttempts++;
                    setupWebSocket(app);
                }
            }, delay);
        } else {
            console.error('Max reconnection attempts reached. Please refresh the page.');
        }
    };
    
    socket.onerror = (error) => {
        console.error('WebSocket error:', error);
    };
    
    // Add cleanup on page unload
    window.addEventListener('beforeunload', () => {
        if (socket.readyState === WebSocket.OPEN) {
            socket.close();
        }
    });
    
    return socket;
}

document.addEventListener('DOMContentLoaded', async () => {
    console.log('DOM loaded, initializing Elm...');
    const target = document.getElementById('app');

    if (!target) {
        console.error('Target element #app not found!');
        return;
    }

    try {
        const producerConfigDb = await getProducerConfig();
        console.log('Producer config DB:', producerConfigDb);

        const app = Elm.Main.init({
            node: target,
            flags: { producers: producerConfigDb }
        });

        console.log('Elm app initialized');

        // Set up WebSocket connection
        setupWebSocket(app);

        // Remove the polling setup since we're using WebSocket now
        // pollPendingVerifications(app);

        // Add logging to debug the port communication
        app.ports.requestApplication?.subscribe(async ({ id }) => {
            console.log('Requesting application:', id);
            try {
                const response = await fetch(`/api/applications/${id}`)
                if (!response.ok) {
                    throw new Error(`HTTP error! status: ${response.status}`)
                }
                const application = await response.json()
                console.log('Application data from server:', {
                    id: application.id,
                    rawMedications: application.rawMedications,
                    data: application.data,
                    formattedData: application.formattedData,
                });
                // Add producer config to the application response
                console.log('Sending application to Elm:', application);
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
                    console.log('Save response:', result);
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
                    // Send the full response with all fields
                    app.ports.submitToCSGResponse.send({
                        success: result.success || false,
                        error: result.error || null,
                        existingSubmission: result.existingSubmission || null,
                        key: result.key || null,
                        verificationStatus: result.verificationStatus || null
                    });
                })
                .catch(error => {
                    app.ports.submitToCSGResponse.send({
                        success: false,
                        error: error.message || 'Failed to submit to CSG',
                        existingSubmission: null,
                        key: null,
                        verificationStatus: null
                    });
                });
        });

        // Update verification port handler to use polling
        app.ports.verifyCSGApplication?.subscribe(([applicationId, key]: [string, string]) => {
            // Start polling for verification status
            pollVerificationStatus(applicationId, key, app);
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