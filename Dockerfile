FROM rocker/shiny:latest

# System deps for google auth + SSL + SQLite
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# Install only the R packages actually used by the app
RUN R -e "install.packages(c('pacman','DT','bcrypt','dplyr','tibble','readr','stringr','DBI','RSQLite','googledrive','googlesheets4','future','promises','digest'), repos='https://cloud.r-project.org')"
