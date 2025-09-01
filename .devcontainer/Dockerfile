# Start from GitHub’s universal Codespaces base image
FROM mcr.microsoft.com/vscode/devcontainers/universal:latest

# Install R and system dependencies
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        r-base \
        gdebi-core \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        wget \
        sudo \
        software-properties-common \
        dirmngr \
    && rm -rf /var/lib/apt/lists/*

# Add Posit (RStudio) apt repository and install RStudio Server
RUN wget -qO- https://repos.rstudio.com/rstudio-pubkey.gpg | gpg --dearmor > /usr/share/keyrings/rstudio.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/rstudio.gpg] https://repos.rstudio.com/rstudio/ubuntu jammy main" > /etc/apt/sources.list.d/rstudio.list \
    && apt-get update \
    && apt-get install -y rstudio-server \
    && rm -rf /var/lib/apt/lists/*

# Create an rstudio user with password "rstudio" (change for production/teaching)
RUN useradd -m rstudio \
    && echo "rstudio:rstudio" | chpasswd \
    && adduser rstudio sudo

# Expose RStudio Server default port
EXPOSE 8787

# Default to bash shell
CMD [ "bash" ]

