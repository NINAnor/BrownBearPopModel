# Use the official R 4.4 image from the Rocker project
FROM rocker/shiny:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /srv/shiny-server/

# Copy the app directory to the image
COPY app/ /srv/shiny-server/

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'DT', \
    'shinycssloaders', \
    'data.table', \
    'tibble', \
    'dplyr', \
    'httr', \
    'maditr', \
    'scales', \
    'tidyr', \
    'stringr', \
    'shinybusy', \
    'MESS', \
    'htmltools', \
    'plotly', \
    'popbio', \
    'shinyvalidate', \
    'readr', \
    'shinyBS', \
    'shinyscreenshot', \
    'ggplot2', \
    'ggdist', \
    'markdown', \
    'lubridate' \
))"

# remove the fluff that is not needed
RUN rm -rf /srv/shiny-server/sample-apps /srv/shiny-server/index.html /srv/shiny-server/[0-9]*

# Switch to the non-root user
USER shiny

# Expose the port the app runs on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
