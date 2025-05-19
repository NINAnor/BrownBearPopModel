# Use the official R 4.4 image from the Rocker project
FROM rocker/shiny:4.4.0

    # Install system dependencies, clean up, and set permissions
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* \
    && mkdir -p /srv/shiny-server/ && chown -R shiny:shiny /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server/

# Copy the app directory to the image
COPY app/ /srv/shiny-server/

# Install R packages and clean up temporary files
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
))" \
    && rm -rf /tmp/*

# Remove unnecessary files in the Shiny server directory
RUN rm -rf /srv/shiny-server/sample-apps /srv/shiny-server/index.html /srv/shiny-server/[0-9]* /srv/shiny-server/.* || true

# Switch to the non-root user
USER shiny

# Expose the port the app runs on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
