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
library(tibble)
library(tidyverse)
library(stringr)
library(shinyjs)


process_pdf <- function(path) {
  # Trying pdftools to text, could be quicker than ocr

  t1 <- pdftools::pdf_text(path)

  # combine into one page and separate lines

  lines <- stringr::str_split(paste(t1, collapse = "\n"), "\n")[[1]]

  # find line where class schedule starts

  idx <- stringr::str_which(
    lines,
    regex("(class|course|lecture|tentative).*?(schedule|outline|grading)", ignore_case = TRUE))


  # If above is 0, then search entire pdf for for 'midterm|assignment|project|exam|quiz|homework'

  # If non zero, proceed as done below

  # return class schedule only

  if (length(idx) == 0) {
    schedule_text <- lines
  } else {
    idx <- idx[1]
    schedule_text <- lines[(idx + 1):length(lines)]
  }

  # collapse back into readable text to confirm output

  schedule_text <- stringr::str_trim(schedule_text)
  schedule_text <- schedule_text[schedule_text != ""]


  assessment_lines <- schedule_text %>%
    purrr::keep(~stringr::str_detect(.x,regex('midterm|assignment|project|exam|quiz|homework',ignore_case = T)))

  assessments <- tibble::tibble(
    raw = assessment_lines
  ) %>%
    dplyr::mutate(
      date1 = stringr::str_extract(raw,
                                   "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2}"),
      date2 = stringr::str_extract(raw,
                                   "(?<=\\b)(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2}"), # fallback
      start_date = dplyr::coalesce(date1, date2), # if date 1 is na, use date 2
      event = stringr::str_trim(str_remove(raw, "^[A-Za-z]{3}\\s+\\d{1,2}")),
      event_type = case_when(
                str_detect(raw, regex("exam", TRUE)) ~ "Exam",
                str_detect(raw, regex("assignment", TRUE)) ~ "Assignment",
                str_detect(raw, regex("quiz", TRUE)) ~ "Quiz",
                TRUE ~ "Lecture")) %>%
    dplyr::mutate(
      start_date = lubridate::mdy(paste(start_date, 2026))
    ) %>%
    select(start_date, event, event_type) %>%
    tidyr::drop_na()

  return(assessments)
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

  ## Checking to see how long it takes to dynamically download and run OCR while user is using the app

  observeEvent(input$submit_button, {
    shinyjs::disable("submit_button")
    Sys.sleep(3)
    base_url <- "https://apps.ualberta.ca/catalogue/syllabus/download/"

    course_id <- course_data %>%
      dplyr::filter(course_code == input$course,
                    course_number == input$course_number_pick,
                    course_section_choices == input$course_section) %>%
      pull(course_ids)

    download_url <- paste0(base_url, "1940", course_id)
    file_name <- paste0(course_id, ".pdf")

    print(Sys.time())
    base_dir <- file.path(getwd())
    cache_dir <- file.path(base_dir, "dl_cache")
    file_path <- file.path(cache_dir, file_name)

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir)
    }

    path <- file_path

    download.file(download_url, file_path, mode ="wb")

    training_set <- process_pdf(file_path)

    print(training_set)
    calendar_data(training_set)
    unlink(file_path)
    nav_select("navbar", selected = "Results - Calendar")
    shinyjs::enable("submit_button")
  })

#Pdf Upload Workflow
  observeEvent(input$submit_upload, {
    showModal(
      modalDialog(
        title = "Oops!",
        tagList(
          p("This is still under construction."),
          tags$img(
            src = "bob.png",
            width = "250px",
            style = "display:block; margin:auto;"
          )
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      )
    )
  })

    # Delete the pngs and the pdf file
  calendar_ready <- reactive({
    req(calendar_data())

    calendar_data() %>%
      mutate(
        id = row_number(),
        calendarId = event_type,
        title = event,
        category = "allday",
        start = format(start_date, "%Y-%m-%d"),
        end = format(start_date, "%Y-%m-%d"),
        backgroundColor = case_when(
          event_type == "Exam" ~ "#FF6B6B",
          event_type == "Assignment" ~ "#4ECDC4",
          event_type == "Quiz" ~ "#FFE66D",
          event_type == "Lecture" ~ "#A020F0",
          TRUE ~ "#95E1D3"
        ),
        borderColor = backgroundColor
      )
  })

  # Combine class schedule with syllabus events
  combined_calendar <- reactive({
    req(calendar_ready())

    # Get the selected semester dates
    start_semester <- as.Date(input$semester_dates[1])
    end_semester <- as.Date(input$semester_dates[2])

    # Parse the course times and dates from the scraped data
    class_info <- course_data %>%
      filter(
        course_code == input$course,
        course_number == input$course_number_pick,
        course_section_choices == input$course_section
      ) %>%
      select(course_dates, course_times) %>%
      slice(1)

    # Extract days of week from format like "2026-01-05 - 2026-04-10 (TR)"
    days_str <- class_info$course_dates
    days_match <- str_extract(days_str, "\\(([A-Z]+)\\)")
    days_only <- str_replace_all(days_match, "[\\(\\)]", "")

    day_map <- c(M = 2, T = 3, W = 4, R = 5, F = 6, S = 7, U = 1)
    class_days <- strsplit(days_only, "")[[1]]
    weekdays_nums <- day_map[class_days]
    weekdays_nums <- weekdays_nums[!is.na(weekdays_nums)]

    # Parse time range (handles both 12-hour and 24-hour formats)
    time_str <- class_info$course_times
    times <- strsplit(time_str, " - ")[[1]]

    start_time <- suppressWarnings(parse_date_time(times[1], orders = c("H:M", "I:M p", "I p")))
    end_time <- suppressWarnings(parse_date_time(times[2], orders = c("H:M", "I:M p", "I p")))

    # Generate all dates in the semester range
    all_dates <- seq.Date(start_semester, end_semester, by = "day")
    class_dates <- all_dates[wday(all_dates) %in% weekdays_nums]

    # Create recurring class events
    max_id <- ifelse(nrow(calendar_ready()) > 0, max(calendar_ready()$id), 0)

    class_events <- tibble(
      id = (max_id + 1):(max_id + length(class_dates)),
      calendarId = "Class",
      title = paste(input$course, input$course_number_pick, "Lecture"),
      category = "time",
      start = format(
        as.POSIXct(paste(class_dates, "00:00:00"), tz = "America/Edmonton") +
          hours(hour(start_time)) + minutes(minute(start_time)),
        "%Y-%m-%dT%H:%M:%S"
      ),
      end = format(
        as.POSIXct(paste(class_dates, "00:00:00"), tz = "America/Edmonton") +
          hours(hour(end_time)) + minutes(minute(end_time)),
        "%Y-%m-%dT%H:%M:%S"
      ),
      backgroundColor = "#95A5A6",
      borderColor = "#95A5A6"
    )

    # Combine with syllabus events
    bind_rows(calendar_ready(), class_events)
  })


  combined_table <- reactive({
    req(calendar_ready())

    # Get the selected semester dates
    start_semester <- as.Date(input$semester_dates[1])
    end_semester <- as.Date(input$semester_dates[2])

    # Parse the course times and dates from the scraped data
    class_info <- course_data %>%
      filter(
        course_code == input$course,
        course_number == input$course_number_pick,
        course_section_choices == input$course_section
      ) %>%
      select(course_dates, course_times) %>%
      slice(1)

    # Extract days of week from format like "2026-01-05 - 2026-04-10 (TR)"
    days_str <- class_info$course_dates
    days_match <- str_extract(days_str, "\\(([A-Z]+)\\)")
    days_only <- str_replace_all(days_match, "[\\(\\)]", "")

    day_map <- c(M = 2, T = 3, W = 4, R = 5, F = 6, S = 7, U = 1)
    class_days <- strsplit(days_only, "")[[1]]
    weekdays_nums <- day_map[class_days]
    weekdays_nums <- weekdays_nums[!is.na(weekdays_nums)]

    # Parse time range (handles both 12-hour and 24-hour formats)
    time_str <- class_info$course_times
    times <- strsplit(time_str, " - ")[[1]]

    start_time <- suppressWarnings(parse_date_time(times[1], orders = c("H:M", "I:M p", "I p")))
    end_time <- suppressWarnings(parse_date_time(times[2], orders = c("H:M", "I:M p", "I p")))

    # Generate all dates in the semester range
    all_dates <- seq.Date(start_semester, end_semester, by = "day")
    class_dates <- all_dates[wday(all_dates) %in% weekdays_nums]

    # Create recurring class events
    max_id <- ifelse(nrow(calendar_ready()) > 0, max(calendar_ready()$id), 0)

    lecture_events <- tibble(
      start_date = class_dates,
      event = paste(input$course, input$course_number_pick, "Lecture"),
      event_type = "Lecture"
    )

    bind_rows(calendar_data(), lecture_events)
  })

  output$schedule_table <- renderTable({
    req(calendar_data())

    combined_table() %>%
      arrange(start_date) %>%
      mutate(
        start_date = format(start_date, "%Y-%m-%d")
      ) %>%
      select(Type = event_type, Title = event, `Start Date` = start_date)

  })
  output$schedule_calendar <- renderCalendar({
    calendar(combined_calendar(), navigation = TRUE)
  })
  # Combine class schedule with syllabus events

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





