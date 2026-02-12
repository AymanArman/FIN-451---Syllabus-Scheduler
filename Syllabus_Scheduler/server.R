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

#OCR Processing as a function so that we can use the same logic for both the selected courses and the pdf uplaod
process_pdf <- function(file_path) {

  text <- pdf_text(file_path)
  text <- paste(text, collapse = "\n")

  date_lines <- str_extract_all(
    text,
    "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2},\\s+\\d{4}.*"
  )[[1]]

  training_set <- tibble(raw = date_lines) %>%
    mutate(
      date = mdy(str_extract(raw,
                             "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2},\\s+\\d{4}"
      )),

      # Extract time ranges like 9:00 AM - 10:15 AM
      time_range = str_extract(raw,
                               "\\d{1,2}(:\\d{2})?\\s?(AM|PM)?\\s?[-–to]+\\s?\\d{1,2}(:\\d{2})?\\s?(AM|PM)?"
      ),

      # Extract single time if no range
      single_time = str_extract(raw,
                                "\\d{1,2}(:\\d{2})?\\s?(AM|PM)"
      ),

      event_type = case_when(
        str_detect(raw, regex("exam", TRUE)) ~ "Exam",
        str_detect(raw, regex("assignment", TRUE)) ~ "Assignment",
        str_detect(raw, regex("quiz", TRUE)) ~ "Quiz",
        TRUE ~ "Lecture"
      ),

      title = str_trim(str_remove(raw, ".*\\d{4}[:\\-\\s]*"))
    )

  training_set <- training_set %>%
    rowwise() %>%
    mutate(
      start_date = {

        if (!is.na(time_range)) {

          times <- str_split(time_range, "[-–to]+")[[1]]
          parsed_time <- parse_date_time(times[1], orders = c("I:M p", "I p"))
          as.POSIXct(date, tz = "America/Edmonton") + hours(hour(parsed_time)) + minutes(minute(parsed_time))

        } else if (!is.na(single_time)) {

          parsed_time <- parse_date_time(single_time, orders = c("I:M p", "I p"))
          as.POSIXct(date, tz = "America/Edmonton") + hours(hour(parsed_time)) + minutes(minute(parsed_time))

        } else {

          # Default if no time found
          as.POSIXct(date, tz = "America/Edmonton") + hours(12)
        }
      },

      end_date = {

        if (!is.na(time_range)) {

          times <- str_split(time_range, "[-–to]+")[[1]]
          parsed_time <- parse_date_time(times[2], orders = c("I:M p", "I p"))
          as.POSIXct(date, tz = "America/Edmonton") + hours(hour(parsed_time)) + minutes(minute(parsed_time))

        } else {

          start_date + hours(1)
        }
      }
    ) %>%
    ungroup()
  return(training_set)
}


server <- function(input, output, session) {

  calendar_data <- reactiveVal(NULL)

  observe({
    print(paste("Current page:", input$navbar))
  })

  output$course_number_dropdown <- renderUI({
    req(input$course)
    course_number_choices <- course_data %>%
      dplyr::filter(course_code == input$course) %>%
      pull(course_number)
    selectInput("course_number_pick", "Select Course Number", course_number_choices)
  })

  output$course_id_dropdown <- renderUI({
    req(input$course_number_pick)
    course_section_choices <- course_data %>%
      dplyr::filter(course_code == input$course,
                    course_number == input$course_number_pick) %>%
      pull(course_section_choices)
    selectInput("course_section", "Select Course Section", choices = course_section_choices)
  })

  output$submit_button <- renderUI({
    req(input$course_section)
    actionButton("submit_button","Submit")
  })

  output$submit_upload_button <- renderUI({
    req(input$file_input)
    actionButton("submit_upload", "Submit PDF")
  })

  output$back_button <- renderUI({
    actionButton("back_button", "Back to Upload")
  })

  output$back_button1 <- renderUI({
    actionButton("back_button1", "Back to Upload")
  })
  output$table_button <- renderUI({
    actionButton("table_button", "See Table")
  })
  output$calendar_button <- renderUI({
    actionButton("calendar_button", "See Calendar")
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

    course_id <- course_data %>%
      dplyr::filter(course_code == input$course,
                    course_number == input$course_number_pick,
                    course_section_choices == input$course_section) %>%
      pull(course_ids)

    download_url <- paste0(base_url, "1940", course_id)
    file_name <- paste0(course_id, ".pdf")

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

    training_set <- process_pdf(file_path)

    print(training_set)
    calendar_data(training_set)
    unlink(file_path)
    nav_select("navbar", selected = "Results - Calendar")
  })

#Pdf Upload Workflow
  observeEvent(input$submit_upload, {

    req(input$file_input)

    file_path <- input$file_input$datapath

    training_set <- process_pdf(file_path)

    calendar_data(training_set)

    unlink(file_path)

    nav_select("navbar", selected = "Results - Calendar")
  })

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
  calendar_ready <- reactive({
    req(calendar_data())

    calendar_data() %>%
      mutate(
        id = row_number(),
        calendarId = event_type,
        category = "time",

        start = format(start_date, "%Y-%m-%dT%H:%M:%S"),
        end   = format(end_date, "%Y-%m-%dT%H:%M:%S"),

        backgroundColor = case_when(
          event_type == "Exam" ~ "#FF6B6B",
          event_type == "Assignment" ~ "#4ECDC4",
          event_type == "Quiz" ~ "#FFE66D",
          event_type == "Lecture" ~"#A020F0",
          TRUE ~ "#95E1D3"
        ),

        borderColor = backgroundColor
      )
  })


  output$schedule_table <- renderTable({
    req(calendar_data())

    calendar_data() %>%
      arrange(start_date) %>%
      mutate(
        start_date = format(start_date, "%Y-%m-%d")
      ) %>%
      select(Type = event_type, Title = title, `Start Date` = start_date, `End Date` = end_date)

  })
  output$schedule_calendar <- renderCalendar({
    calendar(calendar_ready(), navigation = TRUE)
  })

    observeEvent(input$back_button, {
      nav_select("navbar", selected = "Upload")
    })
    observeEvent(input$back_button1, {
      nav_select("navbar", selected = "Upload")
    })
    observeEvent(input$table_button, {
      nav_select("navbar", selected = "Results - Table")
    })
    observeEvent(input$calendar_button, {
      nav_select("navbar", selected = "Results - Calendar")
    })
}





