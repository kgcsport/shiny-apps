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
COPY public/ ./public/
RUN mkdir -p /app/data
EXPOSE 3000
CMD ["node", "server.js"]
