

# There are three ways the scenario analysis can be triggered. 
# 1. change of scenario to be considered  
# 2. change of value of a variable 
# 3. triger of "calculate" button after a change of any input value 


shinyjs::onclick("scenario_show",
                 shinyjs::toggle(id="scenario_control", anim = TRUE)
)

# ----------- Scenario Analysis -----------
# This is not used as a reactive object but as a matrix updated by a child function  
rb$s_change_val <-matrix(c(25, 0, 0,
                           -95, -99, -25,
                           0, -50, 60,
                           0, 0, 200,
                           0, -50, 0), ncol=3, byrow=TRUE)
    

# creating an emptry table that reactively renders
rb$table_scenario <- s_empty_table


s_n <- reactive({
  as.integer(gsub("s","", input$s_choice))
}) 

observeEvent(input$s_choice, {
  s_val <- rb$s_change_val[,s_n()]
  
    updateNumericInput(session,"s_cost_robot",NULL, value=s_val[1], step=10)
    updateNumericInput(session,"s_cost_housing_cow",NULL, value=s_val[2], step=10)
    updateNumericInput(session,"s_milk_change",NULL, value=s_val[3], step=10)
    updateNumericInput(session,"s_scc_change",NULL, value=s_val[4], step=10)
    updateNumericInput(session,"s_pellets",NULL, value=s_val[5], step=10)
  
})


# Update table_scenario when applicable input$s_XXX is changed 

lapply(c(1:5), 
       function(x) {
  observeEvent(input[[paste0("s_",s_varnames[x])]], { 
    n <- s_n()
    
    # Update the value for value change: reactive object is used as a storage 
    rb$s_change_val[1,n] <- input[[paste0("s_",s_varnames[x])]]
  }) 
}
) 

  
observeEvent(rb$s_change_val, {
  
  n <- s_n()
  s_choice <-  input$s_choice
  s_val <- rb$s_change_val[,n]
  base_val <- lappy(s_varnames, 
        function(x) {
             input[[x]]
        }) %>% unlist() 
  new_val <- (base_val * (1 + s_val/100))
  label <- s_labels[n]
  
  source("calculation_robustness.R", local=TRUE)  # Calculates new_row
  
  rb$table_scenario[n,] <- new_row
})


# Recalculate all rows of table_sensitivity 
observeEvent(input$sensitivity_calculate, {
  
  rb$table_scenario <- c_empty_table # Returning to an emptry table
  closeAlert(session, "ref_s_input_change")
  
  base_val <- lappy(s_varnames, 
                    function(x) {
                      input[[x]]
                    }) %>% unlist() 
  
  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(1:3), 
         function(s) { 
           
           s_choice <-  paste0("s",s) 
           s_val <- rb$s_change_val[,s]
           
           s_choice <-  input$s_choice
           s_val <- rb$s_change_val[,n]
           new_val <- (base_val * (1 + s_val/100))
           label <- s_labels[n]
           
           source("calculation_robustness.R", local=TRUE)  # Calculates new_row
           rb$table_scenario[x,] <- new_row
           
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



