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


shinyServer(function(input, output, session) {
  
  # Create a list of reactive values 
  rv <- reactiveValues(recalculate=0)

  # Create a list of reactive values for robustness checks
  rb <- reactiveValues(c_val=20)

  # Create a list of reactive values for robots vs parlors
  rp <- reactiveValues()
  
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
  ## --- WORK IN PROGRESS --- 
  
  
  
#   rb$colnames <- c("input_id","variable", "% change","value","new value",
#                    "net impact w/o housing","change: impact w/o housing", 
#                    "net impact w/ housing","change: impact w/ housing",
#                    "net impact w/ salvage", "change: impact w/ salvage",
#                    "IFOC gain cow/year", "change: IOFC gain cow",
#                    "IFOC gain cwt", "change: IOFC gain cwt",
#                    "milk - feed", "change: milk - feed",
#                    "labor + repair", "change: labor + repair",
#                    "capital cost", "change: capital cost",
#                    "the rest", "change: the rest")
#   rb$order1 <- c("input_id","variable", "% change","value","new value",
#                  "net impact w/ salvage", "change: impact w/ salvage")
#   
#   rb$table_sensitivity <- data.frame(Column1 = numeric(0)) # creating an emptry table
#   
#   rv$colnames <- c("input_id", "milk_cow_day","milk_change")
  # rv$table_input <- data.frame(Column1 = numeric(0))
  
  
#   observe({
#     if (is.null(rv$new_input) | is.null(rb$new_row)) {
#       return()
#     } else if (rv$new_input | rb$new_row)  {
#       updateButton(session, "c_store", disabled = FALSE, style = "primary", icon = "")
#     } 
#     else {
#       updateButton(session, "c_store", disabled = TRUE, style = "default", icon = "")
#     }    
#   }) 
  
#   observeEvent(input$c_store,{
#     if (rv$new_input) {
#       rv$input_id <- rv$input_id + 1
#       tmp2 <- matrix(c(rv$input_id, input$milk_cow_day,input$milk_change),nrow=1)
#       colnames(tmp2) <- rv$colnames
#       # tmp2 <- apply(tmp2,2,round, 2)
#       rv$table_input <- rbind(rv$table_input, tmp2)
#     } 
#     else {
#     }
#     
#     if (rb$new_row) {  
#       tmp <- matrix(c(rv$input_id, input$c_val, rb$value, rb$new_value,  
#                       rb$impact_without_housing, rb$impact_without_housing - rv$impact_without_housing, 
#                       rb$impact_with_housing, rb$impact_with_housing - rv$impact_with_housing,
#                       rb$impact_with_robot_salvage, rb$impact_with_robot_salvage - rv$impact_with_robot_salvage,
#                       rb$IOFC2 - rb$IOFC,  rb$IOFC2-rb$IOFC - (rv$IOFC2 - rv$IOFC),
#                       rb$IOFC2_cwt - rb$IOFC_cwt,  
#                       rb$IOFC2_cwt - rb$IOFC_cwt - (IOFC2_cwt() - IOFC_cwt()),           
#                       rb$milk_feed, rb$milk_feed - rv$milk_feed, 
#                       rb$labor_repair, rb$labor_repair - rv$labor_repair, 
#                       rb$capital_cost, rb$capital_cost - rv$capital_cost, 
#                       rb$misc, rb$misc - rv$misc), nrow=1)  
#       tmp <- apply(tmp,2,round,0)
#       tmp <- matrix(c(tmp[1],rb$var,tmp[2:length(tmp)]),nrow=1)
#       colnames(tmp) <- rb$colnames
#       
#       rb$table_sensitivity <- rbind(rb$table_sensitivity,tmp)
#     } 
#     else {
#     }
#     rb$new_row <- FALSE
#     rv$new_input <- FALSE
#   })
#   
  
  
#   output$table_sensitivity <- DT::renderDataTable({
#     if (dim(rb$table_sensitivity)[1]>0) {
#       # 
#       tbl <- rb$table_sensitivity[, c_order1] 
#       DT::datatable(tbl,
#                     rownames = FALSE,
#                     extensions = 'ColVis',
#                     # extensions = 'ColReorder',
#                     options = list(
#                       dom = 'C<"clear">lfrtip',
#                       scrollX = TRUE,
#                       scrollCollapse = TRUE,
#                       scrollY = 500,
#                       scrollCollapse = TRUE,
#                       # colVis = list(exclude = c(0, 1,1,0),
#                       showNone=TRUE, 
#                       activate = 'mouseover'))
#     } 
#     else {
#       return()
#     }
#   })
  
#   output$table_robust <- DT::renderDataTable({
#     if (input$robust=="Sensitivity") {
#       if (dim(rb$table_sensitivity)[1]==0) return()
#         tbl <- rb$table_sensitivity[, c_order1] 
#     }
#     else if (input$robust=="Scenarios") {
#       if (dim(rb$table_scenario)[1]==0) return()
#       tbl <- rb$table_scenario[, s_order1] 
#     } else {
#       return()
#     }
#     
#       DT::datatable(tbl,
#                     rownames = FALSE,
#                     extensions = 'ColVis',
#                     # extensions = 'ColReorder',
#                     options = list(
#                       dom = 'C<"clear">lfrtip',
#                       scrollX = TRUE,
#                       scrollCollapse = TRUE,
#                       scrollY = 500,
#                       scrollCollapse = TRUE,
#                       # colVis = list(exclude = c(0, 1,1,0),
#                       showNone=TRUE, 
#                       activate = 'mouseover'))
# 
#   })
#   
#   
#   output$table_input <-  DT::renderDataTable({
#     if (dim(rv$table_input)[1]>0) {
#       DT::datatable(rv$table_input,
#                     rownames = FALSE,
#                     extensions = 'ColReorder', options = list(dom = 'Rlfrtip'))
#     } 
#     else {
#       return()
#     }
#   })
#   
#   rv$test_data <- matrix(c(1:6),nrow=2)
#   
#   output$c_download <- downloadHandler(
#     # browser() 
#     filename = "test.xlsx", 
#     
#     content = function(file) { 
#       wb <- XLConnect::loadWorkbook(file, create = TRUE)
#       XLConnect::createSheet(wb, name = "Sheet1")
#       XLConnect::createSheet(wb, name = "Sheet2")
#       XLConnect::writeWorksheet(wb, c(1:3), sheet = "Sheet1") # writes numbers 1:3 in file
#       XLConnect::writeWorksheet(wb, rv$test_data, sheet = "Sheet2") # writes numbers 1:3 in file
#       XLConnect::saveWorkbook(wb)
#     } 
#     
#   ) 
#   
#   
#   
#   output$sheet1 <- renderTable({
#     
#     inFile <- input$user_data
#     if (is.null(inFile))
#     { return(NULL)}
#     
#     browser()
#     wb <- loadWorkbook(inFile$datapath)
#     sheets <- getSheets(wb)
# 
#     read.xlsx(inFile$datapath, sheetIndex = 2)
#   })
#   
  # There is no offical method to remove an uploaded file. 
  # The following provides a "fix" by initializing fileInput(). 
  # http://stackoverflow.com/questions/17352086/how-can-i-update-a-shiny-fileinput-object
  output$resettableInput <- renderUI({
    input$remove
    
    fileInput("file1", "Upload Data File",
              accept=c(".xlsx", "application/vnd.ms-excel"))
  })
  
})


