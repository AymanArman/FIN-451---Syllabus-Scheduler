library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(tidyr)

course_data <- jsonlite::fromJSON(here("web_scraper","courses.json")) %>%
  dplyr::filter(purrr::map_lgl(course_ids, ~ length(.x) > 0))  %>%
  unnest(cols = c(course_section, course_ids, course_dates, course_times)) %>%
  dplyr::mutate(course_section_choices = paste0(course_section, " ", course_dates, " ", course_times))

# https://apps.ualberta.ca/catalogue/course/fin/201 change course number at the end of this link and loop dynamically to scrape each number from above df ^

# Scrape class IDs (ex (81415)) +> regex to remove bracket, then add each ID to end of syllabus download button  after 1940 (href="/catalogue/syllabus/download/194081415">

