declare module '*.elm' {
    export const Elm: {
        Main: {
            init: (options: {
                node: HTMLElement;
                flags: {
                    producers: any;
                };
            }) => {
                ports: {
                    // Application Data Ports
                    receiveApplications?: {
                        send: (data: any) => void;
                        subscribe: (callback: (data: any) => void) => void;
                    };
                    requestRefresh?: {
                        subscribe: (callback: (data: { 
                            page: number;
                            pageSize: number;
                            searchTerm: string;
                            hasContactFilter: boolean;
                            naics: string[];
                        }) => void) => void;
                    };

                    // Single Application Management
                    requestApplication?: {
                        subscribe: (callback: (data: { id: string }) => void) => void;
                    };
                    receiveApplication?: {
                        send: (data: any) => void;
                    };
                    saveApplication?: {
                        subscribe: (callback: (data: { 
                            id: string;
                            data: any;
                            medications: any;
                        }) => void) => void;
                    };
                    saveApplicationResponse?: {
                        send: (data: { success: boolean; error?: string }) => void;
                    };

                    // WebSocket Ports
                    wsSubscribe?: {
                        subscribe: (callback: (data: string[]) => void) => void;
                    };
                    wsUnsubscribe?: {
                        subscribe: (callback: (data: string[]) => void) => void;
                    };
                    wsSubscribed?: {
                        send: (data: string[]) => void;
                    };
                    wsUnsubscribed?: {
                        send: (data: string[]) => void;
                    };
                    wsError?: {
                        send: (data: string) => void;
                    };

                    // CSG Integration Ports
                    submitToCSG?: {
                        subscribe: (callback: (data: [string, number]) => void) => void;
                    };
                    submitToCSGResponse?: {
                        send: (data: {
                            success: boolean;
                            error?: string;
                            existingSubmission?: boolean;
                            key?: string;
                            verificationStatus?: string;
                        }) => void;
                    };

                    // Token Management
                    forceRefreshLAProToken?: {
                        subscribe: (callback: () => void) => void;
                    };
                    getLAProTokenResponse?: {
                        send: (data: string) => void;
                    };

                    // Export
                    exportToCsv?: {
                        subscribe: (callback: (data: {
                            searchTerm: string;
                            hasContactFilter: boolean;
                            hasCSGFilter: boolean;
                        }) => void) => void;
                    };
                };
            };
        };
    };
} 