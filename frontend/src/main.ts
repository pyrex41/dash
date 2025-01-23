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
console.log('Imports completed');



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
        subscribe: (callback: (data: { page: number; pageSize: number; searchTerm: string; hasContactFilter: boolean; naics: string[]; status: string }) => void) => void;
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
    requestApplicationStats?: {
        subscribe: (callback: () => void) => void;
    };
    receiveApplicationStats?: {
        send: (data: any) => void;
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

    // Add message queue for requests before connection is ready
    let messageQueue: any[] = [];
    
    // Helper to send or queue message
    const sendOrQueueMessage = (message: any) => {
        if (socket.readyState === WebSocket.OPEN) {
            socket.send(JSON.stringify(message));
            console.log('Sent message:', message);
        } else {
            console.log('Queueing message for when socket is ready:', message);
            messageQueue.push(message);
        }
    };

    // Wrap socket.send to log all requests
    const originalSend = socket.send;
    socket.send = function(data: string) {
        console.log(`[${new Date().toISOString()}] WebSocket request:`, JSON.parse(data));
        return originalSend.call(this, data);
    };
    
    // Add connection state tracking
    let isConnected = false;
    let reconnectAttempts = 0;
    const maxReconnectAttempts = 5;
    const baseReconnectDelay = 1000; // Start with 1 second
    let currentSubscriptions = new Set<string>();

    
    socket.onopen = () => {
        console.log('WebSocket connected');
        isConnected = true;
        reconnectAttempts = 0;
        
        // Process any queued messages
        while (messageQueue.length > 0) {
            const message = messageQueue.shift();
            console.log('Processing queued message:', message);
            socket.send(JSON.stringify(message));
        }
        
        // Send initial ping
        socket.send(JSON.stringify({ type: 'ping' }));
        
        // Set up token refresh after connection is established
        setupTokenRefresh(socket);
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

            if (message.type === 'heartbeat' || message.type === 'pong') {
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
                        receivedAt,
                        isLoading: false  // Add explicit loading state
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
                    
                    // Remove unnecessary applications refresh
                } else {
                    console.error('Failed to refresh LAPro token:', message.error);
                }
                return;
            }

            // Handle applications list requests
            if (message.type === 'request_applications') {
                console.log('Request for application received:', message.applicationId);
                sendOrQueueMessage({
                    type: 'request_application',
                    applicationId: message.applicationId
                });
            }

            // Handle application stats
            if (message.type === 'application_stats') {
                if (app.ports?.receiveApplicationStats?.send) {
                    console.log('Sending application stats to Elm:', message);
                    app.ports.receiveApplicationStats.send(message.stats);
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
            console.log('Request for application received:', id);
            sendOrQueueMessage({
                type: 'request_application',
                applicationId: id
            });
        });
    }

    // Add requestApplicationStats subscription
    if (app.ports?.requestApplicationStats?.subscribe) {
        app.ports.requestApplicationStats.subscribe(() => {
            console.log('Requesting application stats');
            sendOrQueueMessage({
                type: 'request_application_stats'
            });
        });
    }

    // Add submitToCSG port subscription
    if (app.ports?.submitToCSG?.subscribe) {
        app.ports.submitToCSG.subscribe(([applicationId, producerId]) => {
            if (socket.readyState === WebSocket.OPEN) {
                console.log('Sending submit to CSG request:', { applicationId, producerId });
                socket.send(JSON.stringify({
                    type: 'submit_to_csg',
                    applicationId,
                    producerId
                }));
            } else {
                console.error('WebSocket not connected, cannot submit to CSG');
                if (app.ports?.submitToCSGResponse?.send) {
                    app.ports.submitToCSGResponse.send({
                        success: false,
                        error: 'WebSocket connection not available',
                        existingSubmission: null,
                        key: null,
                        verificationStatus: null
                    });
                }
            }
        });
    }

    if (app.ports?.requestRefresh?.subscribe) {
        app.ports.requestRefresh.subscribe(({ page, pageSize, searchTerm, hasContactFilter, naics, status }) => {
            console.log('Requesting applications refresh:', { page, pageSize, searchTerm, hasContactFilter, naics, status });
            sendOrQueueMessage({
                type: 'request_applications',
                page,
                pageSize,
                searchTerm,
                hasContactFilter,
                naics,
                status
            });
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
    if (socket.readyState !== WebSocket.OPEN) {
        console.warn('WebSocket not ready for token refresh');
        return;
    }

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

        // Handle file exports (keep as HTTP since it's a file download)
        app.ports.exportToCsv?.subscribe(({ searchTerm, hasContactFilter, hasCSGFilter }) => {
            window.location.href = `/api/applications/export?searchTerm=${searchTerm}&hasContactFilter=${hasContactFilter}&hasCSGFilter=${hasCSGFilter}`;
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