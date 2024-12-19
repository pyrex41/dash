// Log to confirm script loading
console.log('Main script loading...');

import './style.css';
import { Elm } from './Main.elm';

console.log('Imports completed');

document.addEventListener('DOMContentLoaded', () => {
    console.log('DOM loaded, initializing Elm...');
    const target = document.getElementById('app');
    
    if (!target) {
        console.error('Target element #app not found!');
        return;
    }

    try {
        const app = Elm.Main.init({
            node: target,
            flags: {
                // Add any flags you need to pass to Elm here
            }
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
                console.log('Received application:', application);
                app.ports.receiveApplication.send(application)
            } catch (error) {
                console.error('Error fetching application:', error)
                app.ports.receiveApplication.send({
                    error: "Failed to load application"
                })
            }
        });

        // Port handlers for interacting with Elm
        app.ports.requestRefresh?.subscribe(({ page, pageSize, searchTerm, hasContactFilter }) => {
            console.log('Refresh requested:', { page, pageSize, searchTerm, hasContactFilter });
            fetch(`/api/applications?page=${page}&pageSize=${pageSize}&searchTerm=${searchTerm}&hasContactFilter=${hasContactFilter}`)
                .then(response => response.json())
                .then(data => {
                    console.log('Data received:', data);
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
    } catch (error) {
        console.error('Error initializing Elm app:', error);
    }
});