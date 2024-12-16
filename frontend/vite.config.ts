import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  plugins: [elmPlugin()],
  resolve: {
    extensions: ['.ts', '.js', '.elm']
  },
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true
      }
    }
  },
  
  build: {
    // Build to the backend's static directory
    outDir: '../dist',
    emptyOutDir: true
  }
});