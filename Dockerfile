FROM rocker/shiny:3.6.3

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    # for magick R package
    libmagick++-dev \
    imagemagick \
    ffmpeg \
    git \
    # for ethoscope imager
    python3


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

# install ethoscope_imager
RUN git clone https://github.com/shaliulab/ethoscope-imager /opt/ethoscope-imager

ARG GIT_TOKEN=not_a_token
RUN git config --global credential.helper \
    '!f() { echo username=antortjim; echo "password=$GIT_TOKEN"; };f'

COPY .Renviron .Renviron

RUN git clone --recursive -b deployment https://github.com/shaliulab/behavr /opt/behavr
RUN git clone --recursive -b deployment https://github.com/shaliulab/scopr /opt/scopr
RUN git clone --recursive -b deployment https://github.com/shaliulab/damr /opt/damr
RUN git clone --recursive -b deployment https://github.com/shaliulab/sleepr /opt/sleepr
RUN git clone --recursive -b deployment https://github.com/shaliulab/ggetho /opt/ggetho
RUN git clone --recursive -b deployment https://github.com/shaliulab/zeitgebr /opt/zeitgebr
RUN git clone --recursive -b deployment https://github.com/shaliulab/esquisse /opt/esquisse
RUN git clone https://github.com/shaliulab/fslretho /opt/fslretho

COPY inst/find_global_dependencies.sh find_global_dependencies.sh 
COPY inst/install_dependencies.R install_dependencies.R 
RUN /bin/bash find_global_dependencies.sh && Rscript install_dependencies.R

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


RUN sudo apt-get -y install sqlite3

# copy the app directory into the image
COPY ./inst/shiny-app/* /srv/shiny-server

# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# set up data folders
RUN sudo mkdir -p /DAM_data /ethoscope_data /fslretho_data 
RUN sudo chown shiny:shiny /DAM_data
RUN sudo chown shiny:shiny /ethoscope_data
RUN sudo chown shiny:shiny /fslretho_data


RUN R -e "devtools::install('/opt/behavr')"
RUN R -e "devtools::install('/opt/scopr')"
RUN R -e "devtools::install('/opt/damr')"
RUN R -e "devtools::install('/opt/sleepr')"
RUN R -e "devtools::install('/opt/ggetho')"
RUN R -e "devtools::install('/opt/zeitgebr')"

RUN cd /opt/esquisse && git pull
RUN cd /opt/fslretho && git pull

RUN R -e "devtools::install('/opt/esquisse')"
RUN R -e "devtools::install('/opt/fslretho')"

# run app
CMD ["/usr/bin/shiny-server"]

