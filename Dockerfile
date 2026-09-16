FROM docker.io/rocker/r-ver:4.6.1

WORKDIR /src

# No renv cache, symlinks - installs to project library
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# System dependencies
RUN apt-get update && \
    apt-get install -y cmake gdal-bin git curl libabsl-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev libuv1-dev libxml2-dev make pandoc xz-utils zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
    | tee /usr/local/lib/R/etc/Rprofile.site \
    | tee /usr/lib/R/etc/Rprofile.site

# Move renv reqs into src for restore
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore dependencies
ENV RENV_PATHS_LIBRARY=renv/library
RUN R -s -e "renv::restore()"

# Copy only what's needed to install the package and build data,
# Avoid rebuilds later
COPY DESCRIPTION NAMESPACE app.R ./
COPY R/ R/
COPY man/ man/
COPY inst/ inst/
COPY data/ data/
COPY data-raw/ data-raw/
RUN R -e 'options(warn = 2); renv::install(".")'
RUN R -e 'source("data-raw/create_duckdb.R"); source("data-raw/spatial_data.R")'

# Everything else (docs, tests, etc)
COPY . .

# Create non-root user
RUN useradd --create-home --shell /bin/bash shiny_user
USER shiny_user

EXPOSE 3000
CMD ["R", "-e", "options('shiny.port'=3000,shiny.host='0.0.0.0');library(SMexplorer);SMexplorer::run_app()"]
