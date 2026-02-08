#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(lubridate)
library(bslib)

# page_fillable(
#   titlePanel("Syllabus Event Creator"),
#   layout_columns(
#     card(
#       card_header("Select Course Code"),
#       selectInput("course","Select Course Code", course_data$course_code),
#       uiOutput("course_number_dropdown"),
#       uiOutput("course_id_dropdown"),
#       uiOutput("submit_button")
#       ),
#     card(
#       card_header("Upload PDF"),
#       fileInput("file_input","Upload a PDF", accept = ".pdf")
#     )
#   )
# )

###########################################

fluidPage(
  fluidRow(
    column(width = 6,
           wellPanel(
             h3("Class Selection"),
             #p("If your class isn't shown, upload a pdf of your syllabus"),
             selectInput("course","Select Course Code", course_data$course_code),
             uiOutput("course_number_dropdown"),
             uiOutput("course_id_dropdown"),
             uiOutput("submit_button")
           )
    ),
    column(width = 6,
           wellPanel(
             h3("Upload PDF"),
             #p("Insert your own syllabus and have us analyze it for you."),
             fileInput("file_input","Upload a PDF", accept = ".pdf")
           )
    )
  )
)


##########################################3
#
# fluidPage(
#   mainPanel(
#     uiOutput("dynamic_page")
#   )
# )

