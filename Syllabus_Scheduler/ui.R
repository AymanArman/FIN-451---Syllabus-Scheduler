#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(lubridate)
library(shiny)
library(shinyjs)
library(bslib)
library(toastui)

#Used bslib for the ui. Formatting is much more intuitive and easier. Also looks way better.
#toastui for the calendar is the cleanest output but may be difficult. Also have table as an option
default_start <- Sys.Date()
default_end <- default_start %m+% months(4)


page_navbar(
  useShinyjs(),
  title = "Syllabus Scheduler",
  id = "navbar",
  theme = bs_theme(bootswatch = "flatly"),
  selected = "Upload",
  sidebar = sidebar(
    dateRangeInput("semester_dates",
                   "Select Your Semester Start and End Dates",
                   start = default_start,
                   end = default_end)
  ),
  # First page - Upload
  nav_panel(
    title = "Upload",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Class Selection"),
        selectInput("course", "Select Course Code", course_data$course_code),
        uiOutput("course_number_dropdown"),
        uiOutput("course_id_dropdown"),
        uiOutput("submit_button")
      ),
      card(
        card_header("Upload PDF"),
        fileInput("file_input", "Upload a PDF", accept = ".pdf"),
        uiOutput("submit_upload_button")
      )
    )
  ),

  # Second page - Results
  nav_panel(
    title = "Results - Calendar",
    card(
      card_header("Schedule"),
      calendarOutput("schedule_calendar", height = "600px")
    ),
    card(
      uiOutput("back_button1"),
      uiOutput("table_button")
    )
  ),
  #Third Page - Results Table
  nav_panel(
    title = "Results - Table",
    card(
        card_header("Schedule Table"),
        tableOutput("schedule_table")
      ),
      card(
        uiOutput("back_button"),
        uiOutput("calendar_button")
      ),
    row_heights = c(4,1)
    )
  )
