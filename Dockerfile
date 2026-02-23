FROM ghcr.io/kgcsport/shiny-apps:latest

# Copy your Shiny apps into the image
COPY apps/ /srv/shiny-server/

# (optional) copy config if you want it baked in too
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf