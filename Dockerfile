FROM rocker/geospatial:latest

WORKDIR /code

RUN apt-get update && apt-get -y install librsvg2-dev

RUN install2.r --error \
    remotes \
    bsicons \
    bslib \
    duckdbfs \
    fontawesome \
    gt \
    markdown \
    shiny \
    shinybusy \
    tidyverse \
    colourpicker \
    mapgl \
    rphylopic


RUN installGithub.r tidyverse/ellmer cboettig/duckdbfs

COPY app .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=8080)"]
