FROM rocker/shiny
LABEL maintainer "Meinhard Ploner <meinhard.ploner@gmail.com>"

WORKDIR /srv/shiny-server

RUN apt-get update \
    && apt-get install -y libsasl2-dev libssl-dev 

RUN echo 'options(repos=list(CRAN="https://cloud.r-project.org/"))' > ".Rprofile"
RUN R -e "install.packages(c('dplyr','tidyr', 'plotly'))"

COPY ./ ./ 
COPY ./shiny-server.sh /usr/bin/shiny-server.sh

EXPOSE 3838

RUN chmod -R 777 .

CMD ["/usr/bin/shiny-server.sh"]
