library(shiny)
library(shinyBS)
library(rmarkdown)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(XLConnect))

source("global_parameters.R")
default_data_1 <- read.xlsx("www/default_user_input_data.xlsx", sheetIndex = 1) 
default_data_2 <- read.xlsx("www/default_user_input_data.xlsx", sheetIndex = 2) 


shinyServer(function(input, output, session) { 
#browser()
 
  
   output$herd_increase_pr4 <- renderUI({ 

     browser()
    h5(input$herd_increase_pr4) 
    }) 
  
   observeEvent(input$dashboard,{
     updateTabsetPanel(session,"prMilk",input$dashboard)
     updateTabsetPanel(session,"prLabor",input$dashboard)
     updateTabsetPanel(session,"prCapital",input$dashboard)
   })
   
   observeEvent(input$prCapital,{
     updateTabsetPanel(session,"prLabor",input$prCapital)
     updateTabsetPanel(session,"prMilk",input$prCapital)
     updateTabsetPanel(session,"dashboard",input$prCapital)
   })
   
   observeEvent(input$prLabor,{
     updateTabsetPanel(session,"prCapital",input$prLabor)
     updateTabsetPanel(session,"prMilk",input$prlabor)
     updateTabsetPanel(session,"dashboard",input$prLabor)
   })
   
   observeEvent(input$prMilk,{
     updateTabsetPanel(session,"prCapital",input$prMilk)
     updateTabsetPanel(session,"prLabor",input$prMilk)
     updateTabsetPanel(session,"dashboard",input$prMilk)
   })
   
   ans <- reactiveValues()
   
   observe({
     profile <- input$dashboard
     herd_increase <- input[[paste0("herd_increase",profile)]]
     ans$herd_size2 <- input$herd_size + herd_increase
   })
   
   output$herd_size2 <- renderUI({ h5(ans$herd_size2) })
   
  ### Does rv have to be reactiveValue?? 
  # Create a list of reactive values 
  rv <- reactiveValues(recalculate=0)
  
  # Create a list of reactive values for robustness checks
  rb <- reactiveValues(c_val=20)
  
  # Create a list of reactive values for robots vs parlors
  rp <- reactiveValues()
  
  # data tables to be downloaded
  df <- reactiveValues()
  
#   # ----------- Functions: some of them depend on local variables  -----------
#   source("helper.R",local=TRUE)
#   
#   # ----------- Miselleneous tasks -----------
#   source(file.path("session_files","session_misc.R"), local=TRUE)
#   
#   # ----------- Main Calculations for Partial Budget and Cash Flow Analyses -----------
#   source(file.path("session_files","session_calculations_base.R"), local=TRUE)
#   
#   # ----------- Main Rendering -----------
#   source(file.path("session_files","session_render_base.R"), local=TRUE)
#   
#   # ----------- Robots vs Parlors Analysis -----------
#   source(file.path("session_files","session_robot_parlor.R"), local=TRUE)
#   
#   
#   # ----------- Sensitivity Analysis -----------
#   source(file.path("session_files","session_sensitivity.R"), local=TRUE)
#   
#   # ----------- Scenario Analysis -----------
#   source(file.path("session_files","session_scenarios.R"), local=TRUE)  
#   
#   
  
#   # --- User Data Storage ---
#   # There is no offical method to remove an uploaded file. 
#   # The following provides a "fix" by initializing fileInput(). 
#   # http://stackoverflow.com/questions/17352086/how-can-i-update-a-shiny-fileinput-object
#   output$resettableInput <- renderUI({
#     input$remove
#     closeAlert(session, "ref_upload_alert")
#     
#     fileInput("data_upload", "Upload Input-Data File",
#               accept=c(".xlsx", "application/vnd.ms-excel"))
#   })
  
}) 


