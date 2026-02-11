FROM node:latest as builder

WORKDIR /tmp/web_scraper

COPY web_scraper/scraper.js .

COPY web_scraper/package*.json .

RUN npm install

RUN node scraper.js

FROM rocker/shiny:latest

#everything will be loaded into /app/ and mimic the paths in our repository

COPY --from=builder /tmp/web_scraper /srv/shiny-server/scheduler_app/web_scraper

COPY ./Syllabus_Scheduler/ /srv/shiny-server/scheduler_app

RUN apt-get update && apt-get install -y \
	libssl-dev \
	libcurl4-gnutls-dev \
	libxml2-dev \
	tesseract-ocr \
	libtesseract-dev \
	libleptonica-dev \
	libpoppler-cpp-dev \
	poppler-data
	

RUN R -e "install.packages(c('jsonlite','tidyverse', 'here', 'lubridate', 'pdftools', 'tesseract'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')" 

RUN chown -R shiny:shiny /srv/shiny-server/scheduler_app

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
