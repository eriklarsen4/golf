FROM ghcr.io/rocker-org/shiny:4.3.2

# System dependencies required by ggplot2, ggridges, apexcharter
RUN apt-get update && apt-get install -y \
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
    libuv1-dev \
    libnode-dev \
    g++ \
    make \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
# Copy only the Shiny app folder
COPY . /app


# Install the R packages
RUN R -e "install.packages(c('shiny','shinyWidgets','dplyr','ggplot2','data.table','plotly','bslib','tibble','ggridges', 'lubridate'), repos='https://cloud.r-project.org')"
RUN R -e "remotes::install_github('dreamRs/apexcharter')"

EXPOSE 3838

CMD ["R", "-e", "setwd('/app'); shiny::runApp('/app', host='0.0.0.0', port=3838)"]
