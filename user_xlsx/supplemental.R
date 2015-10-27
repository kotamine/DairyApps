
## ------------ Supplemental Features ------------

# Initial time stamp
updateTextInput(session, "timestamp", value = get_time_human())

# Initially  add_2 is disabled
shinyjs::toggleState("add_2", FALSE)

observe({
  if (dim(rv$table_1)[1]==0) {
    shinyjs::disable("download")
  } 
  else {
    shinyjs::enable("download")
  }
  
})

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


