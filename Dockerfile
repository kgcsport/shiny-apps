FROM ghcr.io/kgcsport/shiny-apps-base:latest

# Test-only packages (not in base image)
RUN R -e "install.packages(c('testthat', 'httr2'), repos='https://cloud.r-project.org')"

# Copy app code (fast layer — rebuilds whenever apps/ changes)
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/
COPY tests/ /srv/shiny-server/tests/
COPY scripts/ /srv/shiny-server/scripts/
COPY shiny-server.conf.template /etc/shiny-server/shiny-server.conf.template
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
# Fail the image build before publication if the main Shiny app cannot even
# be parsed. Runtime startup is exercised separately by the staging smoke test.
RUN Rscript --vanilla -e "parse(file='/srv/shiny-server/class-job-market/app.R'); cat('class-job-market parse OK\n')"
RUN Rscript --vanilla -e "parse(file='/srv/shiny-server/bonus-pot/app.R'); cat('bonus-pot parse OK\n')"
RUN Rscript --vanilla -e "parse(file='/srv/shiny-server/price-war/app.R'); cat('price-war parse OK\n')"
RUN Rscript --vanilla -e "parse(file='/srv/shiny-server/club-insurance-game/app.R'); cat('risk-insurance parse OK\n')"
RUN Rscript --vanilla -e "parse(file='/srv/shiny-server/coordination-games/app.R'); cat('coordination-games redirect parse OK\n')"
RUN Rscript --vanilla /srv/shiny-server/tests/critical-live-score-audit.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-insurance-market.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-token-spend-precision.R
RUN Rscript --vanilla /srv/shiny-server/tests/risk-insurance-game.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-assignment-wage-award.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-gradebook-ux.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-account-accordion.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-settings-organization.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-bonus-pot-unification.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-game-navigation.R
RUN Rscript --vanilla /srv/shiny-server/tests/price-war-student-play.R
RUN Rscript --vanilla /srv/shiny-server/tests/critical-bonus-pot-private-names.R
RUN Rscript --vanilla /srv/shiny-server/tests/admin-roles.R
RUN Rscript --vanilla /srv/shiny-server/tests/bonus-pot-public-good.R

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
