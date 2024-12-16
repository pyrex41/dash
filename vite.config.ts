import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elmPlugin()],
  esbuild: {
    loader: 'tsx',
    target: 'esnext'
  }
})