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

  syllabus_data <- eventReactive(input$submit_btn, {
    req(input$Syllabus_Upload)
    file_path <- input$Syllabus_Upload$datapath
    text <- pdf_text(file_path)
  })
  output$extracted_text <- renderText({
    syllabus_data()
  })


  observeEvent(input$submit_btn, {
    current_page("results")
  })

  observeEvent(input$back_btn, {
    current_page("upload")
  })
}
