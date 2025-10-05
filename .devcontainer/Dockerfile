# Use Rocker’s prebuilt tidyverse devcontainer (includes R + RStudio Server)
ARG VARIANT="4.2"
FROM ghcr.io/rocker-org/devcontainer/tidyverse:${VARIANT}

# Install Quarto CLI
RUN apt-get update && apt-get install -y wget gdebi-core \
    && wget https://quarto.org/download/latest/quarto-linux-amd64.deb -O /tmp/quarto.deb \
    && gdebi -n /tmp/quarto.deb \
    && rm /tmp/quarto.deb \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install extra R packages
RUN install2.r --error --skipinstalled -n -1 \
        statip \
        patchwork \
        paletteer \
        here \
        doParallel \
        plotly \
        markdown \
    && rm -rf /tmp/downloaded_packages \
    && R -q -e 'remotes::install_github("dcomtois/summarytools@0-8-9")'

