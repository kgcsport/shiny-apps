FROM ghcr.io/kgcsport/shiny-apps-base:latest

# Copy app code (fast layer — rebuilds whenever apps/ changes)
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/
COPY shiny-server.conf.template /etc/shiny-server/shiny-server.conf.template
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
