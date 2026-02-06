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
        selectInput(
          inputId = "Fac_selection",
          label = "Select Your Faculty Here",
          choices = c("FIN", "ACCTG")
          ),
        selectInput(
          inputId = "Course_selection",
          label = "Select Your Course Number Here",
          choices = NULL
        ),
        dateRangeInput("semester_dates",
                       "Select Your Semester Start and End Dates",
                       start = default_start,
                       end = default_end),
        actionButton("submit_btn", "Submit")
        )
      )
    } else {
      tagList(
        sidebarLayout(
          sidebarPanel(
              tableOutput("calendar_tab")
            ),
            mainPanel(
              titlePanel("Calender"),
              verbatimTextOutput("extracted_text"),
              actionButton("back_btn", "Go Back")
            )
          )
        )
    }
  })
  observeEvent(input$Fac_selection, {
    if (input$Fac_selection == "FIN") {
      updateSelectInput(session, "Course_selection",
                        choices = c("201", "312", "322", "413", "414", "415", "416", "418", "430", "434", "436A", "436B", "440", "442", "445", "449", "450", "451", "452", "455", "460", "473", "480", "488", "495", "496", "497"))
    } else if (input$Fac_selection == "ACCTG") {
      updateSelectInput(session, "Course_selection",
                        choices = c("200", "211", "222", "312", "314", "315", "324", "416", "418", "426", "432", "437", "456", "463", "467", "468", "480", "481", "488", "495", "496", "497"))
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
