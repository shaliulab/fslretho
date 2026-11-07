FROM rocker/shiny:3.6.3

# Build from a checkout with submodules populated:
#   git submodule update --init
#   docker build -t fslretho .
# The rethomics packages are installed from inst/dependencies/, i.e. exactly
# the commits pinned by the submodules (and mirrored in DESCRIPTION Remotes).

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    # for magick R package
    libmagick++-dev \
    imagemagick \
    ffmpeg \
    git \
    sqlite3 \
    # for ethoscope imager
    python3 \
    && rm -rf /var/lib/apt/lists/*


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
              "RSQLite",            \
              "waiter",             \
              "isoband"             \
          ), \
          repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/2021-06-25")'

# install ethoscope_imager
RUN git clone https://github.com/shaliulab/ethoscope-imager /opt/ethoscope-imager

COPY .Renviron .Renviron

# rethomics dependencies (git submodules), copied first so this layer is only
# rebuilt when a submodule pin changes
COPY inst/dependencies /opt/fslretho/inst/dependencies
COPY inst/find_global_dependencies.sh inst/install_dependencies.R /tmp/deps/
RUN /bin/bash /tmp/deps/find_global_dependencies.sh /opt/fslretho/inst/dependencies \
    && Rscript /tmp/deps/install_dependencies.R /opt/fslretho/inst/dependencies

# install in dependency order, without letting the installer fetch other versions
RUN for PKG in behavr zeitgebr ggetho damr scopr sleepr esquisse; do \
        R -e "devtools::install('/opt/fslretho/inst/dependencies/${PKG}', dependencies = FALSE, upgrade = 'never')" || exit 1; \
    done

# the app itself
COPY . /opt/fslretho
RUN R -e "devtools::install('/opt/fslretho', dependencies = FALSE, upgrade = 'never')"

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

RUN chown shiny:shiny /etc/scopr.conf /etc/fslretho.conf

# copy the app directory into the image
COPY ./inst/shiny-app/* /srv/shiny-server

# select port
EXPOSE 3838
# allow permission
RUN chown -R shiny:shiny /srv/shiny-server

# set up data folders
RUN mkdir -p /DAM_data /ethoscope_data /fslretho_data \
    && chown shiny:shiny /DAM_data /ethoscope_data /fslretho_data


# run app
CMD ["/usr/bin/shiny-server"]
