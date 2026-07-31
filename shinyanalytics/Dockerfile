# Use a stable R base image
FROM rocker/r-ver:4.3.3

# System dependencies for Shiny + htmlwidgets + DBI + tidyverse
RUN apt-get update && apt-get install -y \
    libuv1 \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev \
    libwebpmux3 \
    libwebpdemux2 \
    && echo "force rebuild $(date)" \
    && rm -rf /var/lib/apt/lists/*

# in case of need to switch to rstudio server    
#RUN apt-get update && apt-get install -y gdebi-core \
#    && wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.20.1002-amd64.deb \
#    && gdebi -n shiny-server-1.5.20.1002-amd64.deb \
#    && rm shiny-server-1.5.20.1002-amd64.deb

# Create app directory
WORKDIR /srv/shiny-server/

# new order
COPY . .

ENV R_PROFILE_USER=/dev/null
ENV RENV_CONFIG_AUTOLOAD=FALSE
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
ENV RENV_CONFIG_PROJECT=NULL
ENV RENV_CONFIG_INHERIT=FALSE
ENV RENV_CONFIG_USER_PROFILE=FALSE

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

RUN R -e "renv::load(project = NULL); renv::restore(lockfile = 'renv.lock', project = NULL)"

# Re-enable normal R behavior for runtime
ENV R_PROFILE_USER=
ENV RENV_CONFIG_AUTOLOAD=
ENV RENV_CONFIG_SANDBOX_ENABLED=
ENV RENV_CONFIG_PROJECT=
ENV RENV_CONFIG_INHERIT=
ENV RENV_CONFIG_USER_PROFILE=

# old order
# Copy renv infrastructure
#COPY renv.lock renv.lock
#COPY renv/ renv/

# Install renv
#RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore the shinyanalytics environment
#RUN R -e "renv::restore()"

# Copy the Shiny app
#COPY . .

# Expose Shiny port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "port <- as.numeric(Sys.getenv('PORT', 3838)); shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=port)"]
