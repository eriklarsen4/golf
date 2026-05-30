FROM rocker/shiny:4.3.2

WORKDIR /srv/shiny-server/app

# 1. Copy your Shiny app into the image
COPY . .

# 2. Install the R packages
RUN R -e "install.packages(c('shiny','shinyWidgets','dplyr','ggplot2','data.table','plotly','bslib','tibble','apexcharter','ggridges'), repos='https://cloud.r-project.org')"

# 3. Expose port + run shiny-server
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
