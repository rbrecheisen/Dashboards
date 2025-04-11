FROM rocker/shiny:latest

# Install R packages (add your own as needed)
RUN install2.r --error shiny shinyjs shinythemes dplyr ggplot2 httr R6 jsonlite tidyverse janitor lubridate DT devtools

# Copy your Shiny app
COPY ./*.R /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose default Shiny port
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
