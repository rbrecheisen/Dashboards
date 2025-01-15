FROM rocker/shiny:latest

ENV SHINY_PORT=3838 \
    APP_DIR=/srv/shiny-server/castordashboard \
    DATA_DIR=/home/shiny/.castordashboard

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p ${APP_DIR} \
    %% mkdir -p ${DATA_DIR}
    
COPY ./dependencies.R ${APP_DIR}

WORKDIR ${APP_DIR}

RUN chown -R shiny:shiny ${APP_DIR} && chown shiny:shiny ${DATA_DIR}

RUN Rscript dependencies.R

COPY ./app.R ${APP_DIR}

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]