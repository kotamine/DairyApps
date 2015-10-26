library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(xlsx)
library(XLConnect)

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d-%H:%M:%OS")
}


shinyServer(function(input, output, session) {
  
  updateTextInput(session, "timestamp", value = get_time_human())
  
  # Define session specific variables, read only once in the beginning 
  rv <- reactiveValues()
  rv$colnames_1 <- c("Name", "Feeling","Hours Sleep","Time Stamp")
  rv$colnames_2 <- c("Name", "Day Exercise","Hours Exercise","Time Stamp")
  rv$table_1 <- data.frame(Column1 = numeric(0))
  rv$table_2 <- data.frame(Column1 = numeric(0))  
  shinyjs::toggleState("add_2", FALSE)

  
  # Add a row to table 1 
  observeEvent(input$add_1,{
    updateTextInput(session, "timestamp", value = get_time_human())
    
    new_row <- matrix(c(input$user_name, input$feeling, 
                        input$hours_sleep, input$timestamp),nrow=1)
    colnames(new_row) <- rv$colnames_1
    rv$table_1 <- rbind(rv$table_1, new_row)
  })
  
  # Add a row to table 2
  observeEvent(input$add_2,{
    updateTextInput(session, "timestamp", value = get_time_human())
    
    new_row <- matrix(c(input$user_name, input$exercise_day, 
                        input$hours_exercise, input$timestamp),nrow=1)
    colnames(new_row) <- rv$colnames_2
    rv$table_2 <- rbind(rv$table_2, new_row)
  })
  
  # Render table 1 
  output$sheet1 <- renderTable({
    if (dim(rv$table_1)[1]>0) { 
      rv$table_1 
    } 
    else {
      NULL
    }
  })
  
  # Render table 2
  output$sheet2 <- renderTable({
    if (dim(rv$table_2)[1]>0) { 
      rv$table_2 
    } 
    else {
      NULL
    }
  })
  
  # Subtract a row from table 2
  observeEvent(input$subtract, {
    if (dim(rv$table_2)[1]==0) return() 
    rv$table_2 <-rv$table_2[-dim(rv$table_2)[1],]
  })
  
  # Clear tables 
  observeEvent(input$clear, {
    rv$table_1 <- data.frame(Column1 = numeric(0))
    rv$table_2 <- data.frame(Column1 = numeric(0))
  })
  
  
  # Download the tables as an Excel file   
  output$download <- downloadHandler(
    filename = "my_test_file.xlsx", 
    content = function(file) { 
      wb <- XLConnect::loadWorkbook(file, create = TRUE)
      XLConnect::createSheet(wb, name = "sleep")
      XLConnect::createSheet(wb, name = "exercise")
      XLConnect::writeWorksheet(wb, rv$table_1, sheet = "sleep") 
      XLConnect::writeWorksheet(wb, rv$table_2, sheet = "exercise") 
      XLConnect::saveWorkbook(wb)
    } 
  ) 
  
  
  # Upload an Excel file, validate it, and then update tables
  observe({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) 
    }
    
    closeAlert(session, "ref_upload_alert")
    
    # Check file type: This is automatically handled when launched to the web
    if (strsplit(inFile$name, "\\.")[[1]][2] !="xlsx") {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Not a Excel workbook: 
                  please upload a data from this app.", 
                  append = TRUE)
      return()
    }
      
    wb <- XLConnect::loadWorkbook(inFile$datapath) 
    sheets <- XLConnect::getSheets(wb)
    
    # Check the number of sheets
    if (length(sheets)!=2) {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Wrong number of sheets: 
                  please upload a file from this app.",
                  append = TRUE)
      return()
    }
      
    user_data_1 <- read.xlsx(inFile$datapath, sheetIndex = 1) 
    user_data_2 <- read.xlsx(inFile$datapath, sheetIndex = 2) 
    # Replace "." symbole with space " "
    colnames(user_data_1) <- gsub("\\."," ",colnames(user_data_1))
    colnames(user_data_2) <- gsub("\\."," ",colnames(user_data_2))
    
    # Check colname names
    if (!(all(colnames(user_data_1) %in% rv$colnames_1) &
        all(rv$colnames_1 %in% colnames(user_data_1)) &
        all(colnames(user_data_2) %in% rv$colnames_2) &
        all(rv$colnames_2 %in% colnames(user_data_2)))) {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Wrong column names: 
                    please upload a file from this app.",
                  append = TRUE)
      return()
    } 
    
    closeAlert(session, "ref_upload_alert")
     
    rv$table_1 <- user_data_1
    rv$table_2 <- user_data_2
  })
  
  
  # There is no offical method to remove an uploaded file. 
  # The following provides a "fix". 
  # http://stackoverflow.com/questions/17352086/how-can-i-update-a-shiny-fileinput-object
  output$resettableInput <- renderUI({
    input$remove
    
    fileInput("file1", "Upload File",
              accept=c(".xlsx", "application/vnd.ms-excel"))
  })
  
  observe({
  if (dim(rv$table_1)[1]==0) {
    shinyjs::disable("download")
  } 
  else {
    shinyjs::enable("download")
  }
    
  })
  
  ## ------------ Supplemental Features ------------
  # Disable/Enable add_1 button until next_user is triggered etc. 
  observeEvent(input$add_1, {
    shinyjs::toggleState("add_1", FALSE)
    shinyjs::toggleState("add_2", TRUE)
    shinyjs::disable("user_name")
    shinyjs::disable("feeling")
    shinyjs::disable("hours_sleep")
    shinyjs::enable("exercise_day")
    shinyjs::enable("hours_exercise")
    updateCollapse(session, "collapse_tables", open = "Sleep")
  })
  
  output$get_name <- renderUI({
    shinyjs::disable("exercise_day")
    shinyjs::disable("hours_exercise")
    shinyjs::toggleState("add_2", FALSE)
    shinyjs::toggleState("next_user", FALSE)
    
    div(h5("Please enter the following for"),
        h4(strong(input$user_name))
    )
  })
  
  observeEvent(input$add_2, {
    shinyjs::toggleState("next_user", TRUE)
    updateCollapse(session, "collapse_tables", open = "Exercise")
  })
  
  observeEvent(input$next_user, {
    shinyjs::toggleState("add_1", TRUE)
    shinyjs::enable("user_name")
    shinyjs::enable("feeling")
    shinyjs::enable("hours_sleep")
    updateTextInput(session,"user_name","Group Member Name",value="")
  })
  
  observe({
    if (input$user_name=="")  {
      shinyjs::toggleState("add_1", FALSE) 
    }
    else {  shinyjs::toggleState("add_1", TRUE)
    }
  })
})

