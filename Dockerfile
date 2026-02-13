FROM node:latest as builder

WORKDIR /tmp/web_scraper

COPY web_scraper/scraper.js .

COPY web_scraper/package*.json .

RUN npm install

RUN node scraper.js

FROM rocker/shiny:latest

#everything will be loaded into /app/ and mimic the paths in our repository

COPY ./Syllabus_Scheduler/ /srv/shiny-server/Syllabus_Scheduler

COPY --from=builder /tmp/web_scraper /srv/shiny-server/Syllabus_Scheduler/web_scraper


RUN apt-get update && apt-get install -y \
	libfreetype6-dev\
	libpng-dev\
	libtiff5-dev \
	libjpeg-dev \
	libwebp-dev \
	libharfbuzz-dev \
	libfribidi-dev \
	libfontconfig1-dev \
	libssl-dev \
	libcurl4-gnutls-dev \
	libxml2-dev \
	tesseract-ocr \
	libtesseract-dev \
	libleptonica-dev \
	libpoppler-cpp-dev \
	poppler-data
	

RUN R -e "install.packages(c('textshaping','ragg'), repos = 'https://packagemanager.rstudio.com/cran/latest')"

RUN R -e "install.packages(c('jsonlite','tidyverse', 'here', 'lubridate', 'pdftools', 'tesseract', 'bslib', 'toastui', 'shinyjs'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')" 

RUN chown -R shiny:shiny /srv/shiny-server/Syllabus_Scheduler

USER shiny

WORKDIR /srv/shiny-server/Syllabus_Scheduler

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
