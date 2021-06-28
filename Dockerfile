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
RUN R -e "devtools::install_github('shaliulab/zeitgebr@0.3.2')"
RUN R -e "devtools::install_github('shaliulab/esquisse@1.0.1.9400')"

RUN apt-get install -y python3
RUN which python3

# install ethoscope_imager
RUN git clone https://github.com/shaliulab/ethoscope-imager /opt/ethoscope-imager
RUN echo `pwd`


# set up configuration files
# TODO They should be created on the spot as needed
COPY inst/configuration/scopr.conf /etc/scopr.conf
COPY inst/configuration/fslretho.conf /etc/fslretho.conf



ARG USER_ID=1000
ARG GROUP_ID=1000

RUN userdel -f shiny &&\
    if getent group shiny ; then groupdel shiny; fi &&\
    groupadd -g ${GROUP_ID} shiny &&\
    useradd -l -u ${USER_ID} -g shiny shiny &&\
    install -d -m 0755 -o shiny -g shiny /home/shiny &&\
    chown --changes --silent --no-dereference --recursive \
          --from=999:999 ${USER_ID}:${GROUP_ID} \
        /home/shiny \
        /srv/shiny-server

RUN chown shiny:shiny /etc/scopr.conf
RUN chown shiny:shiny /etc/fslretho.conf


RUN git clone https://github.com/shaliulab/fslretho /opt/fslretho
RUN R -e "devtools::install('/opt/fslretho')"

# copy the app directory into the image
COPY ./inst/shiny-app/* /srv/shiny-server

# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# set up data folders
RUN sudo mkdir -p /DAM_data /ethoscope_data /fslretho_data 

ARG CACHE_DATE=not_a_date

RUN sudo chown shiny:shiny /DAM_data
RUN sudo chown shiny:shiny /ethoscope_data
RUN sudo chown shiny:shiny /fslretho_data

# run app
CMD ["/usr/bin/shiny-server"]

