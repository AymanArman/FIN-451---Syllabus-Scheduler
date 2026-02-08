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

#######################################################################3

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
        numericInput("table_page",
                     "Enter Page Containing Course Schedule",
                     value = 1,
                     min = 1,
                     max = 20),
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




  syllabus_data <- eventReactive(input$submit_btn, {
    req(input$Syllabus_Upload)
    req(input$table_page)
    file_path <- input$Syllabus_Upload$datapath
    table <- tabulapdf::extract_tables(file_path,
                                       pages = table_page,
                                       method = "stream")
  })


  calendar_init <- eventReactive(input$semester_dates, {

    start_date <- as.Date(input$semester_dates[1])
    end_date <- as.Date(input$semester_dates[2])

    tibble::tibble(date = format(seq.Date(from = start_date,
                                               to = end_date,
                                               by = 1), "%Y-%m-%d"),
                   activity = "")

  })



  nlp_process <- reactive({


    #can do nlp process in here

    calendar <- calendar_init()

    print(calendar)





  })




  output$extracted_text <- renderText({
    syllabus_data()
  })

  output$calendar_tab <- renderTable({
    req(current_page() == "results")
    nlp_process()
  })


  observeEvent(input$submit_btn, {
    current_page("results")
  })

  observeEvent(input$back_btn, {
    current_page("upload")
  })
}
