FROM ghcr.io/kgcsport/shiny-apps-base:latest

# Copy app code (fast layer — rebuilds whenever apps/ changes)
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/
