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
source(file.path("ui_files", "ui_dashboard.R"), local=TRUE)  # Contains functions

default_data_1 <- read.xlsx("www/default_user_input_data.xlsx", sheetIndex = 1) 
default_data_2 <- read.xlsx("www/default_user_input_data.xlsx", sheetIndex = 2) 

## Load the default setting values for investment profiles 
# INSERT CODES HERE


base_profiles <- c("Robots","Retrofit","New")
base_profiles_se <- c(outer(base_profiles, paste0("_se",c(1:5)),FUN=paste,sep="")) # update the number 5
# combo_profiles <- c("RetrofitRobots","RetrofitNew")

refProfileName <-  function(x) {
  if (grepl("_se", x)) {  # TRUE/FALSE for sensitivity analysis
    x <- gsub("_se\\d+","",x)
    } 
    switch(x, 
           "Robots"="Robots",
           "Retrofit"="Retrofit Parlors",
           "New"="New Parlors")
}

shinyServer(function(input, output, session) { 
   
  # All calculations results are stored in "ans" as lists for various profiles  
  # This helps reactive rending of many variables
  
  ans <- reactiveValues()  # profile specific answers/calculation results 
  sum <- reactiveValues()  # summery of results across profiles
  user_data <- reactiveValues() # uploaded (or default) user data
    
  calc_type <- "full"  # Set default calculation type
  browser()


  
  # ----------- Functions: some of them depend on local variables  -----------
  source("helper.R",local=TRUE)
  
  # ----------- Miselleneous tasks -----------
  source(file.path("session_files","session_misc.R"), local=TRUE)
  
  # ----------- Calculations for Partial Budget and Cash Flow Analyses -----------
  source(file.path("session_files","session_calculations_main.R"), local=TRUE)

  source(file.path("session_files","session_partial_budget.R"), local=TRUE)

  source(file.path("session_files","session_summary.R"), local=TRUE)
  
  source(file.path("session_files","session_render_base.R"), local=TRUE)
  
  # ----------- Sensitivity Analysis -----------
   source(file.path("session_files","session_sensitivity.R"), local=TRUE)
  
#   
#   # ----------- Scenario Analysis -----------
#   source(file.path("session_files","session_scenarios.R"), local=TRUE)  
#   
#   
  
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


