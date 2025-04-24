import { defineConfig } from 'vite'

export default defineConfig({
  root: "./src",
  base: '/MoodifyCleanFinal/',
  build: {
    outDir: "../dist",
    emptyOutDir: true
  }
})