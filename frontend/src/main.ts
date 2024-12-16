// Log to confirm script loading
console.log('Main script loading...');

import './style.css';
import { Elm } from './Dashboard.elm';

console.log('Imports completed');

document.addEventListener('DOMContentLoaded', () => {
    console.log('DOM loaded, initializing Elm...');
    const target = document.getElementById('app');
    
    if (!target) {
        console.error('Target element #app not found!');
        return;
    }

    try {
        const app = Elm.Dashboard.init({
            node: target
        });

        console.log('Elm app initialized');

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