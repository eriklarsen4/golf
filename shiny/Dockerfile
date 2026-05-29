FROM rocker/shiny:4.3.2

WORKDIR /srv/shiny-server/app

# copy the Shiny app
COPY . .

# copy the repo-root renv.lock into the image
COPY ../renv.lock ./renv.lock

# install renv + restore
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
