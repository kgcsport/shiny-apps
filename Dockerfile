FROM ghcr.io/kgcsport/shiny-apps-base:latest

# Test-only packages (not in base image)
RUN R -e "install.packages(c('testthat', 'httr2'), repos='https://cloud.r-project.org')"

# Copy app code (fast layer — rebuilds whenever apps/ changes)
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/
COPY tests/ /srv/shiny-server/tests/
COPY shiny-server.conf.template /etc/shiny-server/shiny-server.conf.template
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
