/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx,elm}",
  ],
  theme: {
    extend: {
      colors: {
        'tokyo-orange': '#FF6347'
      },
    },
  },
  plugins: [],
}