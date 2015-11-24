library(shiny)
library(shinyBS)
library(markdown)
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
  
  # Create a list of reactive values 
  rv <- reactiveValues(recalculate=0)
  
  # Create a list of reactive values for robustness checks
  rb <- reactiveValues(c_val=20)
  
  # Create a list of reactive values for robots vs parlors
  rp <- reactiveValues()
  
  # data tables to be downloaded
  df <- reactiveValues()
  
  # ----------- Functions: some of the depend on local variables  -----------
  source("helper.R",local=TRUE)
  
  # ----------- Miselleneous tasks -----------
  source("session_misc.R", local=TRUE)
  
  # ----------- Main Calculations for Partial Budget and Cash Flow Analyses -----------
  source("session_calculations_base.R", local=TRUE)
  
  # ----------- Main Rendering -----------
  source("session_render_base.R", local=TRUE)
  
  # ----------- Robots vs Parlors Analysis -----------
  source("session_robot_parlor.R", local=TRUE)
  
  
  # ----------- Sensitivity Analysis -----------
  source("session_sensitivity.R", local=TRUE)
  
  # ----------- Scenario Analysis -----------
  source("session_scenarios.R", local=TRUE)  
  
  
  
  # --- User Data Storage ---
  # There is no offical method to remove an uploaded file. 
  # The following provides a "fix" by initializing fileInput(). 
  # http://stackoverflow.com/questions/17352086/how-can-i-update-a-shiny-fileinput-object
  output$resettableInput <- renderUI({
    input$remove
    closeAlert(session, "ref_upload_alert")
    
    fileInput("data_upload", "Upload Input-Data File",
              accept=c(".xlsx", "application/vnd.ms-excel"))
  })
  
})


