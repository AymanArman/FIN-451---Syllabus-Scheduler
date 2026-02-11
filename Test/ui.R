library(shiny)
library(lubridate)
library(bslib)
library(toastui)

page_sidebar(
    title = "Syllabus Schedulizer",
    sidebar = list(
      selectInput("course", "Select Course", course_data$course_code),
      uiOutput("course_number_dropdown"),
      uiOutput("course_id_dropdown"),
      uiOutput("submit_button"),
      fileInput("file_input", "Upload a PDF", accept = ".pdf"),
      uiOutput("submit_upload_button")
    ),
    card(card_header("Schedule"),
         calendarOutput("schedule_calendar", height = "600px")
         ),
    card(
      card_header("Event List"),
      tableOutput("schedule_table")
      )
)


