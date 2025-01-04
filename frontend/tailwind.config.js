/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx,elm}",
  ],
  theme: {
    extend: {
      colors: {
        'cyber': {
          'primary': '#00fff5',    // Bright cyan
          'secondary': '#ff00ff',  // Magenta
          'accent': '#f700ff',     // Hot pink
          'dark': '#0d1117',       // Deep space black
          'light': '#1c1c1c',      // Lighter black
          'error': '#ff0044',      // Neon red
          'success': '#00ff9f',    // Neon green
          'text': '#ffffff',       // White text
          'muted': '#8b8b8b',      // Muted text
        },
        'gradient': {
          'start': '#00fff5',
          'end': '#ff00ff',
        }
      },
      boxShadow: {
        'cyber': '0 0 10px rgba(0, 255, 245, 0.3)',
        'cyber-hover': '0 0 20px rgba(0, 255, 245, 0.5)',
        'cyber-error': '0 0 10px rgba(255, 0, 68, 0.3)',
      },
      animation: {
        'pulse-slow': 'pulse 4s cubic-bezier(0.4, 0, 0.6, 1) infinite',
      }
    },
  },
  plugins: [],
}

