FROM rocker/shiny:3.6.3

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    # for magick R package
    libmagick++-dev


RUN R -e 'install.packages(c(\
              "devtools",   \
              "data.table", \
              "dplyr",      \
              "ggplot2",    \
              "lubridate",  \
              "magrittr",   \
              "magick",     \
              "shiny",      \
              "shinydashboard",     \
              "shinydashboardPlus", \
              "shinyWidgets",       \
              "shinybusy",          \
              "stringr",            \
              "R6",                 \
              "rjson",              \
              "rlang",              \
              "RSQLite"             \
          ), \
          repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/2021-06-25")'

RUN apt-get install -y git

RUN R -e "devtools::install_github('shaliulab/behavr@0.3.3')"
RUN R -e "devtools::install_github('shaliulab/scopr@0.3.3')"
RUN R -e "devtools::install_github('shaliulab/sleepr@0.3.3')"
RUN R -e "devtools::install_github('shaliulab/ggetho@0.3.6')"
RUN R -e "devtools::install_github('shaliulab/zeitgebr@0.3.1')"
RUN R -e "devtools::install_github('shaliulab/esquisse@1.0.1.9400')"

# install ethoscope_imager
RUN git clone https://github.com/shaliulab/ethoscope-imager /opt/ethoscope-imager
RUN echo `pwd`

ARG CACHE_DATE=not_a_date

RUN git clone https://github.com/shaliulab/fslretho /opt/fslretho
RUN R -e "devtools::install('/opt/fslretho')"


# copy the app directory into the image
COPY ./inst/shiny-app/* /srv/shiny-server

# set up configuration files
# TODO They should be created on the spot as needed
COPY inst/configuration/scopr.conf /etc/scopr.conf
COPY inst/configuration/fslretho.conf /etc/fslretho.conf
#RUN chown $USER /etc/scopr.conf
#RUN chown $USER /etc/fslretho.conf


# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# set up data folders
RUN sudo mkdir -p /DAM_data/results /ethoscope_data/results /ethoscope_data/cache /fslretho_data/logs /fslretho_data/sessions 

# run app
CMD ["/usr/bin/shiny-server"]

