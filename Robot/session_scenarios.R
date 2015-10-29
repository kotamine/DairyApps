
# There are three ways the scenario analysis can be triggered. 
# 1. change of scenario to be considered  
# 2. change of value of a variable 
# 3. triger of "calculate" button after a change of any input value 


shinyjs::onclick("scenario_show",
                 {
                   shinyjs::toggle(id="sensitivity_control", anim = TRUE) 
                   shinyjs::toggle(id="scenario_control", anim = TRUE)
                   shinyjs::toggle(id="dashboard_robust", anim = TRUE) 
                   createAlert(session, "s_toggle", "ref_s_toggle", 
                               content = "Change scenarios to refresh the results.",
                               append = FALSE) 
                 }
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
           
           # update the value for % change: reactive object is used as a storage 
           rb$s_change_val[x,n] <- input[[paste0("s_",s_varnames[x])]]
         }) 
       }
) 


observe({
  rb$s_change_val
  input$s_choice
  closeAlert(session, "ref_s_toggle")
  
  isolate({
    
    n <- s_n()
    s_choice <-  input$s_choice
    s_val <- rb$s_change_val[,n]
    base_val <- lapply(s_varnames, 
                       function(x) {
                         input[[x]]
                       }) %>% unlist() 
    new_val <- (base_val * (1 + s_val/100))
    label <- s_labels[n]
    robust <- "Scenarios"
    
    source("calculation_robustness.R", local=TRUE)  # Calculates new_row
    
    rb$table_scenario[n,] <- new_row
    
  })
})


# Recalculate all rows of table_sensitivity 
observeEvent(input$scenario_calculate, {
  
  rb$table_scenario <- s_empty_table # Returning to an emptry table
  closeAlert(session, "ref_s_input_change")
  
  base_val <- lapply(s_varnames, 
                     function(x) {
                       input[[x]]
                     }) %>% unlist() 
  robust <- "Scenarios"
  
  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(1:3), 
         function(s) { 
           
           s_choice <-  paste0("s",s) 
           s_val <- rb$s_change_val[,s]
           
           new_val <- (base_val * (1 + s_val/100))
           label <- s_labels[s]
           
           source("calculation_robustness.R", local=TRUE)  # Calculates new_row
           rb$table_scenario[s,] <- new_row
           
         }
  )
  
})


# input$s_choice (or rb$s_change_val) triggers this
observe({ 
  input$s_choice
  rb$s_change_val
  
  isolate({
    n <- s_n()
    s_val <- rb$s_change_val[,n]
    base_val <- lapply(s_varnames, 
                       function(x) {
                         input[[x]]
                       }) %>% unlist() 
    new_val <- (base_val * (1 + s_val/100))
    
    unit <- c("dollar","dollar", "lb/cow/day", "percent","lb/day") 
    
    val0 <-c(); val1<-c()

    d_round <- c(0,0,1,1,1)
    for (x in 1:5) {
        rb$s_txt[[x]] <- list()
        rb$s_txt[[x]][[1]] <- base_val[x] %>% round(d_round[x])
        if (base_val[x]!=new_val[x]) {
        rb$s_txt[[x]][[2]] <- new_val[x] %>% round(d_round[x])
        }
        rb$s_txt[[x]][[3]] <- unit[x] 
        }
  })
})

lapply(c(1:5), 
       function(x) {
         output[[paste0("s_txt_",s_varnames[x])]] <- renderUI({
           if (length(rb$s_txt)>0) { 
             fluidRow(column(4, rb$s_txt[[x]][[1]]),
                      column(4, rb$s_txt[[x]][[2]]),
                      column(4, rb$s_txt[[x]][[3]])) 
           }
         })
       }
)


