
# There are three ways the sensitivity analysis can be triggered. 
# 1. change of variable to consider the sensitivity 
# 2. change of value for that variable 
# 3. triger of "calculate" button after a change of any input value 

shinyjs::onclick("sensitivity_show",
                { 
                   shinyjs::toggle(id="sensitivity_control", anim = TRUE) 
                   shinyjs::toggle(id="scenario_control", anim = TRUE)
                   shinyjs::toggle(id="dashboard_robust", anim = TRUE) 
                   createAlert(session, "c_toggle", "ref_c_toggle", 
                               content = "Change sensitivity items to refresh the results.",
                               append = FALSE) 
                 }
)


# ----------- Sensitivity Analysis -----------
# This is not used as a reactive object but as a vector updated by a child function  
rb$c_change_val <- c(20, 20, 50, 50, -50, -50, 50) 

# creating an emptry table that reactively renders
rb$table_sensitivity <- c_empty_table


c_n <- reactive({
  as.integer(gsub("c","", input$c_choice))
}) 

observeEvent(input$c_choice, {
  c_val <- rb$c_change_val[c_n()]
  updateNumericInput(session,"c_val",NULL, value=c_val, step=10)
})


# Update table_sensitivity when input$c_val or input$choice is changed as well as initial set up
observe({
  input$c_val
  input$c_choice
  closeAlert(session, "ref_c_toggle")
  
  isolate({ 
  n <- c_n()

  # update the value for % change: reactive object is used as a storage 
  rb$c_change_val[n] <- input$c_val  
  c_choice <-  input$c_choice
  
  c_val <- rb$c_change_val[n]
  
  base_val <- input[[c_varnames[n]]]
  new_val <- (base_val * (1 + c_val/100))
  label <- c_labels[n]
  
  robust <- "Sensitivity" 
  source("calculation_robustness.R", local=TRUE)  # Calculates new_row
  
  rb$table_sensitivity[n,] <- new_row
  })
})


# Recalculate all rows of table_sensitivity 
observeEvent(input$sensitivity_calculate, {

  rb$table_sensitivity <- c_empty_table # Returning to an emptry table
  closeAlert(session, "ref_c_input_change")
  robust <- "Sensitivity" 
  
  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(1:7), 
         function(x) { 

             c_choice <-  paste0("c",x) 
             c_val <- rb$c_change_val[x]
             
             base_val <- input[[c_varnames[x]]]
             new_val <- (base_val * (1 + c_val/100))
             label <- c_labels[x]
             
             source("calculation_robustness.R", local=TRUE)  # Calculates new_row
             rb$table_sensitivity[x,] <- new_row
             
         }
  )
})


# input$c_choice (or input$c_val via rb$c_change_val) triggers this
output$c_text <- renderUI({
  n <- c_n()
 
  isolate( c_choice <-  input$c_choice )
  c_val <- rb$c_change_val[n]
  
  base_val <- input[[c_varnames[n]]]
  new_val <- (base_val * (1 + c_val/100))
  
  if (c_choice=="c1") {
    val0 <-  base_val %>% formatdollar()
    val1 <-  new_val %>% formatdollar()
    unit <- ""
  } else if (c_choice=="c2") { 
    val0 <-  base_val %>% formatdollar()
    val1 <-  new_val %>% formatdollar()
    unit <- ""
  } else if (c_choice=="c3") {
    val0 <-  base_val %>% formatdollar()
    val1 <-  new_val %>% formatdollar()
    unit <- ""
  } else if (c_choice=="c4") {
    val0 <-  base_val  %>% round(2)
    val1 <-  new_val  %>% round(2)
    unit <- "years"
  } else if (c_choice=="c5") {
    val0 <-  base_val %>% formatdollar()
    val1 <-  new_val %>% formatdollar()
    unit <- ""
  } else if (c_choice=="c6") {
    val0 <-  base_val %>% round(2)
    val1 <-  new_val  %>% round(2)
    unit <- "hrs/day"
  } else if (c_choice=="c7") {
    val0 <-  base_val %>% round(2)
    val1 <-  new_val  %>% round(2)
    unit <- "lb/cow/day"
  }
  
  paste("from", val0," to ", val1, unit) %>% h5()
})


