FROM rocker/shiny:latest

# System deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages(c('pacman','DT','bcrypt','dplyr','tibble','readr','stringr','DBI','RSQLite','googledrive','googlesheets4','future','promises','digest','jsonlite','ggplot2','tidyr','lubridate','forcats'), repos='https://cloud.r-project.org'); gc()"

# Copy your apps into the image
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/

# (optional) bake in shiny-server.conf; if you mount it, you can skip this
# COPY shiny-server.conf /etc/shiny-server/shiny-server.conf