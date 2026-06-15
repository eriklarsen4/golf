# Use a stable R base image
FROM rocker/r-ver:4.3.3

# System dependencies for Shiny + htmlwidgets + DBI + tidyverse
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglpk-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /srv/shiny-server/

# Copy renv infrastructure
COPY renv.lock renv.lock
COPY renv/ renv/

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore the shinyanalytics environment
RUN R -e "renv::restore()"

# Copy the Shiny app
COPY . .

# Expose Shiny port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
