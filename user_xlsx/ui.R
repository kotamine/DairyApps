library(shiny)
library(shinyjs)
library(shinyBS)
library(xlsx)
library(XLConnect)

shinyUI(fluidPage(
 # bootstrapPage(
    shinyjs::useShinyjs(),
  titlePanel('Download/Upload Excel Workbook Demo'),
  sidebarLayout(
    sidebarPanel(
      textInput("user_name","Group Member Name",value="Awesome Person"), 
      radioButtons("feeling", "How is the general helth of this person?", 
                   choices = c("good", "okay", "not bad","not so good")),
      numericInput("hours_sleep","How many hours does the person sleep regularly?",
                   value= 7, step=.5, min=0, max=24), 
      actionButton("add_1","Add to Table Sleep"), 
      hr(), 
      
      uiOutput("get_name"), 
      br(), 
      selectInput("exercise_day", "Which day of the week does the person exercise? 
                  (multiple entries to the table if applicable)",
                  choices = c("Sun", "Mon", "Tue", "Wed","Thu","Fri","Sat")),
      numericInput("hours_exercise", "How many hours does that exercise take?",
                   value= 1, step=.25, min=0, max=24), 
      actionButton("add_2","Add to Table Exercise"), 
      # hidden input field tracking the timestamp of the submission
      shinyjs::hidden(textInput("timestamp", "")), 
      br(), br(),
      actionButton("next_user","Move to next member"),
      hr(), 
      
      downloadButton("download","Download"),
      actionButton("clear", "Clear"), 
      br(), br(), 
      # fileInput() is passessed from the server
      uiOutput('resettableInput'),
      bsAlert("upload_alert"), 
      br(), 
      actionButton("remove", "Remove File")
    ),
     mainPanel(
       bsCollapse(id = "collapse_tables", open = "Sleep",
                  bsCollapsePanel("Sleep",
          tableOutput('sheet1')),
          bsCollapsePanel("Exercise",
          tableOutput('sheet2'))
      )
    )
  )
)) 


