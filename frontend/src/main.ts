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

// Add polling for CSG application status
async function pollCSGStatus(key: string, app: any) {
    const pollInterval = 30000; // 30 seconds
    const maxAttempts = 120; // 1 hour total
    let attempts = 0;

    const poll = async () => {
        if (attempts >= maxAttempts) {
            console.log('Stopping CSG status polling after max attempts');
            return;
        }

        try {
            const response = await fetch(`/api/csg-application/${key}`);
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const data = await response.json();
            console.log('CSG application status:', data);

            // Refresh applications list to get updated status
            if (app.ports?.receiveApplications?.send) {
                const applicationsResponse = await fetch('/api/applications');
                if (applicationsResponse.ok) {
                    const applicationsData = await applicationsResponse.json();
                    app.ports.receiveApplications.send(applicationsData);
                }
            }

            // Continue polling if not in a final state
            if (data.status !== 'approved' && data.status !== 'declined') {
                attempts++;
                setTimeout(poll, pollInterval);
            }
        } catch (error) {
            console.error('Error polling CSG status:', error);
            attempts++;
            setTimeout(poll, pollInterval);
        }
    };

    // Start polling
    poll();
}

// Add type for verification update message
interface VerificationUpdateMessage {
    type: 'verification_update';
    msgId: string;
    timestamp: string;
    applicationId: string;
    body: {
        status: string;
        applicationStatus: string;
        csg_id: string;
        error?: string;
        screenshot?: string;
        verifyUrl?: string;
        signatureUrl?: string;
    };
}

// Add type definitions for Elm ports
interface ElmPorts {
    receiveApplications?: {
        send: (data: any) => void;
        subscribe: (callback: (data: any) => void) => void;
    };
    requestRefresh?: {
        subscribe: (callback: (data: { page: number; pageSize: number; searchTerm: string; hasContactFilter: boolean; naics: string[]; }) => void) => void;
    };
    requestApplication?: {
        subscribe: (callback: (data: { id: string }) => void) => void;
    };
    receiveApplication?: {
        send: (data: any) => void;
    };
    saveApplication?: {
        subscribe: (callback: (data: { id: string; data: any; medications: any[] }) => void) => void;
    };
    saveApplicationResponse?: {
        send: (data: { success: boolean; error: string | null }) => void;
    };
    submitToCSG?: {
        subscribe: (callback: (data: [string, number]) => void) => void;
    };
    submitToCSGResponse?: {
        send: (data: { success: boolean; error: string | null; existingSubmission: any | null; key: string | null; verificationStatus: string | null }) => void;
    };
    verifyCSGApplication?: {
        subscribe: (callback: (data: [string, string]) => void) => void;
    };
    verifyCSGApplicationResponse?: {
        send: (data: any) => void;
    };
    forceRefreshLAProToken?: {
        subscribe: (callback: () => void) => void;
    };
    getLAProTokenResponse?: {
        send: (token: string) => void;
    };
    statusUpdate?: {
        send: (data: { id: string; status: string }) => void;
    };
    exportToCsv?: {
        subscribe: (callback: (data: { searchTerm: string; hasContactFilter: boolean; hasCSGFilter: boolean }) => void) => void;
    };
}

interface ElmApp {
    ports: ElmPorts;
}

// WebSocket connection handler
function setupWebSocket(app: any) {
    let wsUrl = import.meta.env.VITE_API_WS_URL || 'ws://localhost:3000/ws';
    
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
    let currentSubscriptions = new Set<string>();

    // Track current view state
    let currentViewState = {
        page: 0,
        pageSize: 20,
        searchTerm: '',
        hasContactFilter: false,
        naics: [] as string[]
    };
    
    socket.onopen = () => {
        console.log('WebSocket connected');
        isConnected = true;
        reconnectAttempts = 0;
        
        // Make initial request for applications
        socket.send(JSON.stringify({
            type: 'request_applications',
            page: 0,
            pageSize: 20,
            searchTerm: '',
            hasContactFilter: false,
            naics: []
        }));
    };
    
    socket.onmessage = async (event) => {
        const receivedAt = new Date().toISOString();
        try {
            const message = JSON.parse(event.data);
            console.log(`[${receivedAt}] WebSocket message received:`, message);

            // Handle ping/pong
            if (message.type === 'ping') {
                socket.send(JSON.stringify({ type: 'pong' }));
                return;
            }

            // Handle subscription confirmations
            if (message.type === 'subscribed') {
                console.log(`[${receivedAt}] Subscription confirmed for:`, message.applicationIds);
                message.applicationIds.forEach((id: string) => currentSubscriptions.add(id));
                return;
            }

            if (message.type === 'unsubscribed') {
                console.log(`[${receivedAt}] Unsubscribed from:`, message.applicationIds);
                message.applicationIds.forEach((id: string) => currentSubscriptions.delete(id));
                return;
            }

            // Handle applications data
            if (message.type === 'applications_data') {
                console.log('Received applications data:', message);
                
                if (app.ports?.receiveApplications?.send) {
                    app.ports.receiveApplications.send({
                        applications: message.applications,
                        pagination: message.pagination,
                        receivedAt
                    });
                } else {
                    console.error('receiveApplications port not available');
                }
                return;
            }

            // Handle application data responses
            if (message.type === 'application_data') {
                if (app.ports?.receiveApplication?.send) {
                    console.log(`[${receivedAt}] Application data received:`, {
                        id: message.applicationId,
                        hasApplication: !!message.application,
                        applicationFields: message.application ? Object.keys(message.application) : [],
                        hasOnboardingData: !!message.onboarding_data,
                        onboardingFields: message.onboarding_data ? Object.keys(message.onboarding_data) : []
                    });
                    
                    const applicationData = {
                        ...message.application,
                        onboarding_data: message.onboarding_data || {}
                    };
                    app.ports.receiveApplication.send(applicationData);
                } else {
                    console.error(`[${receivedAt}] receiveApplication port not available`);
                }
                return;
            }

            // Handle save application responses
            if (message.type === 'save_application_response') {
                if (app.ports?.saveApplicationResponse?.send) {
                    console.log(`[${receivedAt}] Sending save response to Elm:`, message);
                    app.ports.saveApplicationResponse.send({
                        success: message.success,
                        error: message.error
                    });
                }
                return;
            }

            // Handle CSG submission responses
            if (message.type === 'submit_to_csg_response') {
                if (app.ports?.submitToCSGResponse?.send) {
                    console.log(`[${receivedAt}] Sending CSG submission response to Elm:`, message);
                    app.ports.submitToCSGResponse.send({
                        success: message.success || false,
                        error: message.error || null,
                        existingSubmission: message.existingSubmission || null,
                        key: message.key || null,
                        verificationStatus: message.verificationStatus || null
                    });
                }
                return;
            }

            // Handle CSG verification responses
            if (message.type === 'verify_csg_application_response') {
                if (app.ports?.verifyCSGApplicationResponse?.send) {
                    console.log(`[${receivedAt}] Sending CSG verification response to Elm:`, message);
                    app.ports.verifyCSGApplicationResponse.send(message.result);
                }
                return;
            }

            // Handle LAPro token refresh responses
            if (message.type === 'refresh_lapro_token_response') {
                console.log('Processing LAPro token refresh response:', {
                    success: message.success,
                    error: message.error,
                    token: message.token ? 'present' : 'missing'
                });
                if (message.success && message.token) {
                    // Set cookie with a reasonable expiry (e.g., 1 hour)
                    const expiryDate = new Date();
                    expiryDate.setTime(expiryDate.getTime() + (60 * 60 * 1000));
                    document.cookie = `lapro_token=${message.token}; expires=${expiryDate.toUTCString()}; path=/`;
                    
                    app.ports.getLAProTokenResponse.send(message.token);
                    
                    // Request a refresh of applications to get latest data
                    socket.send(JSON.stringify({
                        type: 'request_applications',
                        ...currentViewState
                    }));
                } else {
                    console.error('Failed to refresh LAPro token:', message.error);
                }
                return;
            }
        } catch (error) {
            console.error(`[${receivedAt}] Error handling WebSocket message:`, error);
        }
    };
    
    socket.onclose = (event) => {
        console.log(`WebSocket disconnected with code ${event.code}`, event.reason);
        isConnected = false;
        
        if (reconnectAttempts < maxReconnectAttempts) {
            // Exponential backoff for reconnection
            const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
            console.log(`[${new Date().toISOString()}] Attempting to reconnect in ${delay}ms... (Attempt ${reconnectAttempts + 1}/${maxReconnectAttempts})`);
            
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

    // Replace HTTP requests with WebSocket messages
    if (app.ports?.requestApplication?.subscribe) {
        app.ports.requestApplication.subscribe(({ id }) => {
            console.log(`[${new Date().toISOString()}] Requesting application:`, {
                id,
                socketState: socket.readyState,
                isOpen: socket.readyState === WebSocket.OPEN
            });
            
            if (socket.readyState === WebSocket.OPEN) {
                // First subscribe to the application if not already subscribed
                if (!currentSubscriptions.has(id)) {
                    socket.send(JSON.stringify({
                        type: 'subscribe',
                        applicationIds: [id]
                    }));
                }
                
                // Then request its data
                socket.send(JSON.stringify({
                    type: 'request_application',
                    applicationId: id
                }));
            } else {
                console.error(`WebSocket not open (state: ${socket.readyState}) when requesting application:`, id);
            }
        });
    } else {
        console.error('requestApplication port not available');
    }

    function requestApplications(params: { page: number; pageSize: number; searchTerm: string; hasContactFilter: boolean; naics: string[] }) {
        socket.send(JSON.stringify({
            type: "request_applications",
            page: params.page,
            pageSize: params.pageSize,
            searchTerm: params.searchTerm,
            hasContactFilter: params.hasContactFilter,
            naics: params.naics
        }));
    }

    if (app.ports?.requestRefresh?.subscribe) {
        app.ports.requestRefresh.subscribe(({ page, pageSize, searchTerm, hasContactFilter, naics }) => {
            if (socket.readyState === WebSocket.OPEN) {
                // Update current view state
                currentViewState = {
                    page,
                    pageSize,
                    searchTerm,
                    hasContactFilter,
                    naics
                };
                
                requestApplications({ page, pageSize, searchTerm, hasContactFilter, naics });
            }
        });
    }

    if (app.ports?.saveApplication?.subscribe) {
        app.ports.saveApplication.subscribe(({ id, data, medications }) => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'save_application',
                    id,
                    formData: data,
                    medications
                }));
            }
        });
    }
    
    return socket;
}

// Add automatic token refresh every 45 minutes
function setupTokenRefresh(socket: WebSocket) {
    // Initial token refresh
    socket.send(JSON.stringify({
        type: 'refresh_lapro_token'
    }));

    // Set up periodic refresh (45 minutes)
    setInterval(() => {
        if (socket.readyState === WebSocket.OPEN) {
            socket.send(JSON.stringify({
                type: 'refresh_lapro_token'
            }));
        }
    }, 45 * 60 * 1000);
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
        }) as ElmApp;

        console.log('Elm app initialized');

        // Set up WebSocket connection
        const socket = setupWebSocket(app);

        // Set up automatic token refresh
        setupTokenRefresh(socket);

        // Handle file exports (keep as HTTP since it's a file download)
        app.ports.exportToCsv?.subscribe(({ searchTerm, hasContactFilter, hasCSGFilter }) => {
            window.location.href = `/api/applications/export?searchTerm=${searchTerm}&hasContactFilter=${hasContactFilter}&hasCSGFilter=${hasCSGFilter}`;
        });

        // Handle application list refresh requests
        app.ports.requestRefresh?.subscribe(({ page, pageSize, searchTerm, hasContactFilter, naics }) => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'request_applications',
                    page,
                    pageSize,
                    searchTerm,
                    hasContactFilter,
                    naics
                }));
            }
        });

        // Handle application saves
        app.ports.saveApplication?.subscribe(({ id, data, medications }) => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'save_application',
                    id,
                    formData: data,
                    medications
                }));
            }
        });

        // Handle CSG submissions
        app.ports.submitToCSG?.subscribe(([applicationId, producerId]: [string, number]) => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'submit_to_csg',
                    applicationId,
                    producerId
                }));
            }
        });

        // Handle CSG verifications
        app.ports.verifyCSGApplication?.subscribe(([applicationId, key]: [string, string]) => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'verify_csg_application',
                    key
                }));
            }
        });

        // Handle LAPro token refresh
        app.ports.forceRefreshLAProToken?.subscribe(() => {
            if (socket.readyState === WebSocket.OPEN) {
                socket.send(JSON.stringify({
                    type: 'refresh_lapro_token'
                }));
            }
        });

    } catch (error) {
        console.error('Error initializing Elm app:', error);
    }
});