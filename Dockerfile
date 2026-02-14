FROM rocker/shiny-verse:latest

# System deps that commonly matter for tidyverse + google auth + SSL

RUN apt-get update && apt-get install -y --no-install-recommends \

    libsqlite3-dev \

    libcurl4-openssl-dev \

    libssl-dev \

    libxml2-dev \

  && rm -rf /var/lib/apt/lists/*
# Install R packages needed by your apps

RUN R -e "install.packages(c('pacman','DT','bcrypt','DBI','RSQLite','pool','base64enc','glue','googledrive','googlesheets4','future','promises','digest'), repos='https://cloud.r-project.org')"
