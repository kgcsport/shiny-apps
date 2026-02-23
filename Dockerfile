FROM ghcr.io/kgcsport/shiny-apps:latest

# Clear out anything in site dir except appdata
RUN find /srv/shiny-server -mindepth 1 -maxdepth 1 ! -name 'appdata' -exec rm -rf {} +

# Copy only your apps
COPY apps/ /srv/shiny-server/