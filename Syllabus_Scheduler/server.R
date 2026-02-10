library(shiny)
library(lubridate)
library(pdftools)
library(tesseract)
library(tibble)

server <- function(input, output, session) {

  # Page switching state
  current_page <- reactiveVal("upload")

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

  ## Catalog syllabus download and OCR
  observeEvent(input$submit_button, {
    base_url <- "https://apps.ualberta.ca/catalogue/syllabus/download/"
    download_url <- paste0(base_url, "1940", input$course_id)
    file_name <- paste0(input$course_id, ".pdf")

    print(Sys.time())
    base_dir <- file.path(getwd())
    cache_dir <- file.path(base_dir, "dl_cache")
    file_path <- file.path(cache_dir, file_name)

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir)
    }

    download.file(download_url, file_path, mode ="wb")

    img_file <- pdftools::pdf_convert(file_path)
    pdf_text <- ocr(img_file)

    # Delete the pngs and the pdf file
    unlink(img_file)
    unlink(file_path)

    print(pdf_text)
    print(paste0("DONE: ",Sys.time()))

    # Switch to results page
    current_page("results")
  })

  # Page switch observer for back button
  observeEvent(input$back_btn, {
    current_page("upload")
  })

  # Dynamic page output
  output$dynamic_page <- renderUI({
    if (current_page() == "upload") {
      fluidRow(
        column(width = 8,
               wellPanel(
                 h3("Class Selection"),
                 selectInput("course","Select Course Code", course_data$course_code),
                 uiOutput("course_number_dropdown"),
                 uiOutput("course_id_dropdown"),
                 uiOutput("submit_button")
               )
        ),
        column(width = 8,
               wellPanel(
                 h3("Upload PDF"),
                 fileInput("file_input","Upload a PDF", accept = ".pdf")
               )
        )
      )
    } else {
      fluidRow(
        column(width = 12,
               wellPanel(
                 h3("Results"),
                 actionButton("back_btn", "Go Back")
               )
        )
      )
    }
  })
}
