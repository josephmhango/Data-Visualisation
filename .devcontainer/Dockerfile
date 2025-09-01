# Use Rocker’s prebuilt tidyverse devcontainer (includes R + RStudio Server)
ARG VARIANT="4.2"
FROM ghcr.io/rocker-org/devcontainer/tidyverse:${VARIANT}

# Install extra R packages
RUN install2.r --error --skipinstalled -n -1 \
        statip \
        patchwork \
        paletteer \
        here \
        doParallel \
        janitor \
        vip \
        ranger \
        palmerpenguins \
        skimr \
        nnet \
        kernlab \
        plotly \
        factoextra \
        cluster \
        tidymodels \
        markdown \
        ottr \
    && rm -rf /tmp/downloaded_packages \
    && R -q -e 'remotes::install_github("dcomtois/summarytools@0-8-9")'
