# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev \
libudunits2-dev \
libgdal-dev \
libgeos-dev \
libproj-dev

## update system libraries
RUN apt-get update && \
apt-get upgrade -y && \
apt-get clean

# copy necessary files
## app folder
COPY /app ./app
## .R to setup the packages
COPY setup_docker.R ./setup_docker.R

# install renv & restore packages
RUN Rscript setup_docker.R

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '127.0.0.1', port = 3838)"]