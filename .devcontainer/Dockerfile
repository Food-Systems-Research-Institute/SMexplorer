FROM docker.io/rocker/r-ver:4.6.1

WORKDIR /src

# No renv cache, symlinks - installs to project library
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# System dependencies
RUN apt-get update && \
    apt-get install -y chromium cmake gdal-bin git curl libabsl-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev libuv1-dev libxml2-dev make pandoc xz-utils zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
    | tee /usr/local/lib/R/etc/Rprofile.site \
    | tee /usr/lib/R/etc/Rprofile.site

ENV RENV_PATHS_LIBRARY=renv/library

EXPOSE 3000
