#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(lubridate)
library(pdftools)
library(tesseract)
library(tibble)
library(tidyverse)
library(stringr)

server <- function(input, output, session) {

  calendar_data <- reactiveVal(NULL)

  output$course_number_dropdown <- renderUI({
    req(input$course)
    course_number_choices <- course_data %>%
      dplyr::filter(course_code == input$course) %>%
      pull(course_number)
    selectInput("course_number_pick", "Select Course Number", course_number_choices)
  })

  output$course_id_dropdown <- renderUI({
    req(input$course_number_pick)
    course_id_choices <- course_data %>%
      dplyr::filter(course_code == input$course,
                    course_number == input$course_number_pick) %>%
      pull(course_ids) %>%
      unlist()
    selectInput("course_id", "Select Course ID", choices = course_id_choices)
  })

  output$submit_button <- renderUI({
    req(input$course_id)
    actionButton("submit_button","Submit")
  })

  output$submit_upload_button <- renderUI({
    req(input$file_input)
    actionButton("submit_upload", "Submit PDF")
  })


  # observeEvent(input$course, {
  #   course_number_choices <- course_data %>%
  #     dplyr::filter(course_code == input$course) %>%
  #     pull(course_number)
  #   updateSelectInput("course_number_pick", "Select Course Number", course_number_choices)
  # })
  #
  # observeEvent(input$course_number_pick, {
  #   course_id_choices <- course_data %>%
  #     dplyr::filter(course_code == input$course,
  #                   course_number == input$course_number_pick) %>%
  #     pull(course_ids) %>%
  #     unlist()
  #   updateSelectInput("course_id_pick", "Select Course ID", course_id_choices)
  # })


  ## Checking to see how long it takes to dynamically download and run OCR while user is using the app

  observeEvent(input$submit_button, {
    base_url <- "https://apps.ualberta.ca/catalogue/syllabus/download/"

    download_url <- paste0(base_url, "1940", input$course_id)
    file_name <- paste0(input$course_id, ".pdf")

    # base_dir <- file.path(getwd(), "syllabus_downloads")
    # course_dir <- file.path(base_dir, input$course)
    # course_number_dir <- file.path(course_dir, input$course_number_pick)
    # file_path <- file.path(course_number_dir, file_name)
    #
    # if (!dir.exists(course_dir)) {
    #   dir.create(course_dir)
    # }
    #
    # if (!dir.exists(course_number_dir)) {
    #   dir.create(course_number_dir)
    # }
    #
    # if (!file.exists(file_path)) {
    #   download.file(download_url, file_path)
    # }

    ###########
    #
    print(Sys.time())
    base_dir <- file.path(getwd())
    cache_dir <- file.path(base_dir, "dl_cache")
    file_path <- file.path(cache_dir, file_name)

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir)
    }

    path <- file_path

    download.file(download_url, file_path, mode ="wb")

    # img_file <- pdftools::pdf_convert(file_path)
    # pdf_text <- ocr(img_file)
    #
    # # Delete the pngs and the pdf file
    # unlink(img_file)
    # unlink(file_path)
    #
    # print(pdf_text)
    # print(paste0("DONE: ",Sys.time()))

    ## Downloading and converting to text with OCR after submit
    ## currently takes ~10.2 seconds before any text matching.

    text <- pdf_text(file_path)
    text <- paste(text, collapse = "\n")

    date_lines <- str_extract_all(
      text,
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2},\\s+\\d{4}.*"
    )[[1]]

    training_set <- tibble(raw = date_lines) %>%
      mutate(
        date = mdy(str_extract(raw, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2},\\s+\\d{4}")),
        event_type = case_when(
          str_detect(raw, regex("exam", TRUE)) ~ "Exam",
          str_detect(raw, regex("assignment", TRUE)) ~ "Assignment",
          str_detect(raw, regex("quiz", TRUE)) ~ "Quiz",
          TRUE ~ "Lecture"
        ),
        title = str_trim(str_remove(raw, ".*\\d{4}"))
      ) %>%
      select(event_type, title, start_date = date)

    print(training_set)
    unlink(file_path)
    calendar_data(training_set)
      })

  # Prepare data for calendar





    #########################################

    # Word Bags

    # mths <- c('Jan','Feb',"Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
    #
    # # Trying pdftools to text, could be quicker than ocr
    #
    # t1 <- pdftools::pdf_text(file_path)
    #
    # # combine into one page and separate lines
    #
    # lines <- str_split(paste(t1, collapse = "\n"), "\n")[[1]]
    #
    # # find line where class schedule starts
    #
    # idx <- str_which(
    #   lines,
    #   regex("(class|course|lecture|tentative).*?schedule", ignore_case = TRUE))
    #
    #
    # # If above is 0, then search entire pdf for for 'midterm|assignment|project|exam|quiz|homework'
    #
    # # If non zero, proceed as done below
    #
    # # return class schedule only
    #
    # schedule_text <- lines[(idx + 1):length(lines)]
    #
    # # collapse back into readable text to confirm output
    #
    # schedule_text <- str_trim(schedule_text)
    # schedule_text <- schedule_text[schedule_text != ""]
    #
    # assessment_lines <- schedule_text %>%
    #   purrr::keep(~stringr::str_detect(.x,regex('midterm|assignment|project|exam|quiz|homework',ignore_case = T)))
    #
    # assessments <- tibble::tibble(
    #   raw = assessment_lines
    # ) %>%
    #   dplyr::mutate(
    #     date1 = str_extract(raw, "^[A-Za-z]{3}\\s+\\d{1,2}"),
    #     date2 = str_extract(raw, "(?<=\\b)[A-Za-z]{3}\\s+\\d{1,2}"), # fallback
    #     date = coalesce(date1, date2), # if date 1 is na, use date 2
    #     event = str_trim(str_remove(raw, "^[A-Za-z]{3}\\s+\\d{1,2}"))) %>%
    #   dplyr::mutate(date = lubridate::mdy(paste(date, 2026))) %>%
    #   select(date, event)
    #
    # print(assessments)
    #

    # Delete the pngs and the pdf file
  # Prepare data for calendar
  calendar_ready <- reactive({
    req(calendar_data())

    calendar_data() %>%
      mutate(
        id = row_number(),
        calendarId = event_type,
        category = "time",
        start = format(start_date, "%Y-%m-%d"),
        end = format(start_date, "%Y-%m-%d"),
        backgroundColor = case_when(
          event_type == "Exam" ~ "#FF6B6B",
          event_type == "Assignment" ~ "#4ECDC4",
          event_type == "Quiz" ~ "#FFE66D",
          TRUE ~ "#95E1D3"
        ),
        borderColor = backgroundColor
      )
  })

  # Render calendar
  output$schedule_calendar <- renderCalendar({
    calendar(calendar_ready(), navigation = TRUE)
  })
  output$schedule_table <- renderTable({
    calendar_data
  })
  }


