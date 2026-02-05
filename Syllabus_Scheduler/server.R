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

server <- function(input, output, session) {

  current_page <- reactiveVal("upload")

  default_start <- Sys.Date()
  default_end <- default_start %m+% months(4)

  output$dynamic_page <- renderUI({
    if (current_page() == "upload") {
      tagList(
        mainPanel(
        titlePanel("Syllabus Upload and Semester Dates"),
        fileInput("Syllabus_Upload", "Upload Your Syllabus Here"),
        dateRangeInput("semester_dates",
                       "Select Your Semester Start and End Dates",
                       start = default_start,
                       end = default_end),
        actionButton("submit_btn", "Submit")
        )
      )
    } else {
      tagList(
        mainPanel(
        titlePanel("Calender"),
        actionButton("back_btn", "Go Back"),
        verbatimTextOutput("extracted_text")
      )
      )
    }
  })

  calendar_init <- eventReactive(input$semester_dates, {

    start_date <- as.Date(input$semester_dates[1])
    end_date <- as.Date(input$semester_dates[2])

    tibble::tibble(date = format(seq.Date(from = start_date,
                                               to = end_date,
                                               by = 1), "%Y-%m-%d"),
                   activity = "")

  })


  syllabus_data <- eventReactive(input$submit_btn, {
    req(input$Syllabus_Upload)
    file_path <- input$Syllabus_Upload$datapath
    text <- pdf_text(file_path)
  })



  output$extracted_text <- renderText({
    syllabus_data()
  })

  output$calendar_tab <- renderTable({
    calendar_init()
  })


  observeEvent(input$submit_btn, {
    current_page("results")
  })

  observeEvent(input$back_btn, {
    current_page("upload")
  })
}
