FROM ghcr.io/rocker-org/shiny:4.3.2

WORKDIR /srv/shiny-server/app

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

# Copy your Shiny app into the image
COPY shinyanalytics/ .

# Install the R packages
RUN R -e "install.packages(c('shiny','shinyWidgets','dplyr','ggplot2','data.table','plotly','bslib','tibble','apexcharter','ggridges'), repos='https://cloud.r-project.org')"

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]

