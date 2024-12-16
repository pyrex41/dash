import { serve } from "bun";
import { join } from "path";

const PROJECT_ROOT = import.meta.dir;
const DIST_PATH = join(PROJECT_ROOT, "dist");

console.log(`Serving static files from: ${DIST_PATH}`);

serve({
  port: process.env.PORT || 3000,
  fetch(req) {
    let path = new URL(req.url).pathname;
    // Serve index.html for the root path
    if (path === "/") {
      path = "/index.html";
    }

    // Try to serve the file from the dist directory
    const filePath = join(DIST_PATH, path);
    const file = Bun.file(filePath);

    return new Response(file);
  },
});