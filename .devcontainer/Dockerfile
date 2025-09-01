# Start from GitHub’s default Codespaces image (has VS Code goodies preinstalled)
FROM mcr.microsoft.com/vscode/devcontainers/universal:latest

# Install R and dependencies
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        r-base \
        gdebi-core \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        sudo \
        wget \
    && rm -rf /var/lib/apt/lists/*

# Download and install RStudio Server
RUN wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2023.12.1-402-amd64.deb \
    && sudo gdebi -n rstudio-server-2023.12.1-402-amd64.deb \
    && rm rstudio-server-2023.12.1-402-amd64.deb

# Create rstudio user with password "rstudio" (you can change this)
RUN useradd -m rstudio \
    && echo "rstudio:rstudio" | chpasswd \
    && adduser rstudio sudo

# Expose RStudio Server port
EXPOSE 8787

# Default command
CMD [ "bash" ]
