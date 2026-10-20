# Build stage — needs compiler for better-sqlite3 native module
FROM node:22-alpine AS builder
WORKDIR /app
RUN apk add --no-cache python3 make g++
COPY package*.json ./
RUN npm ci --omit=dev

# Runtime stage — clean image, no compiler
FROM node:22-alpine
WORKDIR /app
COPY --from=builder /app/node_modules ./node_modules
COPY server.js ./
# The Shiny OAuth relay imports helpers from this directory at startup.
# Omitting it causes the container to crash-loop and turns course login
# requests into 502 responses at the reverse proxy.
COPY lib/ ./lib/
COPY public/ ./public/
COPY templates/ ./templates/
RUN mkdir -p /app/data
EXPOSE 3000
CMD ["node", "server.js"]
