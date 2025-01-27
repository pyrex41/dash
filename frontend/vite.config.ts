import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  plugins: [elmPlugin()],
  base: '/', // Explicitly set base URL
  resolve: {
    extensions: ['.ts', '.js', '.elm']
  },
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true,
        secure: false,
        ws: true,
        configure: (proxy, options) => {
          proxy.on('error', (err, req, res) => {
            console.log('proxy error', err);
          });
          proxy.on('proxyReq', (proxyReq, req, res) => {
            console.log('Sending Request to the Target:', proxyReq.method, proxyReq.path);
          });
          proxy.on('proxyRes', (proxyRes, req, res) => {
            console.log('Received Response from the Target:', proxyRes.statusCode);
          });
        }
      }
    }
  },
  
  build: {
    // Build to the backend's static directory
    outDir: '../dist',
    emptyOutDir: true,
    assetsDir: 'assets',
    copyPublicDir: true,
    rollupOptions: {
      output: {
        assetFileNames: (assetInfo) => {
          // Put all assets (including public files) into assets directory
          return 'assets/[name][extname]';
        }
      }
    }
  },
  publicDir: 'public'
});