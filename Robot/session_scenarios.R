
# There are three ways the scenario analysis can be triggered. 
# 1. change of scenario to be considered  
# 2. change of value of a variable 
# 3. triger of "calculate" button after a change of any input value 


shinyjs::onclick("scenario_show",
                 {
                   shinyjs::toggle(id="sensitivity_control", anim = TRUE) 
                   shinyjs::toggle(id="scenario_control", anim = TRUE)
                   shinyjs::toggle(id="dashboard_robust", anim = TRUE) 
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
rb$table_scenario_before_tax <- s_empty_table
rb$table_scenario_after_tax <- s_empty_table


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
  if (input$robust=="Off") { return() }
  
  rb$s_change_val
  input$s_choice
  closeAlert(session, "ref_s_toggle")
  
  if (!(input$robot_parlor=="OFF" | input$profile_choice=="Robots")) { return() } 
  # Currently, Scenario is enabled only for Robots
    
  isolate({
    
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
      varnames <- s_varnames
      labels <- s_labels
    } else {
      varnames <- s_varnames_parlor
      labels <- s_labels_parlor
    }
    
    n <- s_n()
    s_choice <-  input$s_choice
    s_val <- rb$s_change_val[,n]
    base_val <- lapply(varnames, 
                       function(x) {
                         input[[x]]
                       }) %>% unlist() 
    new_val <- (base_val * (1 + s_val/100))
    label <- labels[n]
    robust <- "Scenarios"
    
#     source("session_calculation_robustness.R", local=TRUE)  # Calculates new_row
#     
#     rb$table_scenario[n,] <- new_row
#     
    
    source("session_calculations_robustness.R", local=TRUE)
    
    rb$NAI_spec <- input$NAI
    
    source("session_dashboard_robustness.R", local=TRUE)
    
    if (input$NAI=="before tax") {
      # --- add a row of results to the table_sensitivity_before_tax ---
      new_row <- c(s_val, new_val,  
                   rb$NAI, rb$diff_NAI,
                   rb$milk_feed, rb$diff_milk_feed, 
                   rb$labor_repair, rb$diff_labor_repair, 
                   rb$capital, rb$diff_capital, 
                   rb$misc, rb$diff_misc,
                   rb$inflation, rb$diff_inflation,
                   rb$IOFC2 - rb$IOFC,  rb$diff_IOFC2 -rb$diff_IOFC,
                   rb$IOFC2_cwt - rb$IOFC_cwt,  rb$diff_IOFC2_cwt -rb$diff_IOFC_cwt,
                   rb$bw_wage_before_tax, rb$bw_wage_before_tax-rv$bw_wage_before_tax,  
                   rb$bw_wage_inflation_before_tax,
                   rb$bw_wage_inflation_before_tax - rv$bw_wage_inflation_before_tax
      ) %>% round(c(rep(0,24),rep(2,4),rep(3,2)))
      

      new_row <- matrix(c(label,new_row),nrow=1)
      colnames(new_row) <- s_colnames
      
      rb$table_scenario_before_tax[n,] <- new_row
    } else {
      rb$NAI_spec <- "after tax"
      source("session_dashboard_robustness.R", local=TRUE)
      
      new_row <- c(s_val, new_val,  
                   rb$NAI, rb$diff_NAI,
                   rb$milk_feed, rb$diff_milk_feed, 
                   rb$labor_repair, rb$diff_labor_repair, 
                   rb$capital, rb$diff_capital, 
                   rb$misc, rb$diff_misc,
                   rb$inflation, rb$diff_inflation,
                   rb$IOFC2 - rb$IOFC,  rb$diff_IOFC2 -rb$diff_IOFC,
                   rb$IOFC2_cwt - rb$IOFC_cwt,  rb$diff_IOFC2_cwt -rb$diff_IOFC_cwt,
                   rb$bw_wage_after_tax, rb$bw_wage_after_tax-rv$bw_wage_after_tax,  
                   rb$bw_wage_inflation_after_tax,
                   rb$bw_wage_inflation_after_tax - rv$bw_wage_inflation_after_tax
      ) %>% round(c(rep(0,24),rep(2,4),rep(3,2)))
      
      new_row <- matrix(c(label,new_row),nrow=1)
      colnames(new_row) <- s_colnames
      
      rb$table_scenario_after_tax[n,] <- new_row
    } 
    
  })  
}) 


# Recalculate all rows of table_scenario
observeEvent(input$scenario_calculate, {
  
  if (!(input$robot_parlor=="OFF" | input$profile_choice=="Robots")) { return() } 
  # Currently, Scenario is enabled only for Robots
  
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
    varnames <- s_varnames
    labels <- s_labels
  } else {
    varnames <- s_varnames_parlor
    labels <- s_labels_parlor
  }
  
  rb$table_scenario <- s_empty_table # Returning to an emptry table
  closeAlert(session, "ref_s_input_change")
  closeAlert(session, "ref_s_toggle")
  
  base_val <- lapply(varnames, 
                     function(x) {
                       input[[x]]
                     }) %>% unlist() 
  robust <- "Scenarios"
  
  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(3:1), 
         function(s) { 
           
           s_choice <-  paste0("s",s) 
           s_val <- rb$s_change_val[,s]
           
           new_val <- (base_val * (1 + s_val/100))
           label <- s_labels[s]
           
#            source("session_calculation_robustness.R", local=TRUE)  # Calculates new_row
#            rb$table_scenario[s,] <- new_row
           
           
           source("session_calculations_robustness.R", local=TRUE)
           
           rb$NAI_spec <- "after tax"
           source("session_dashboard_robustness.R", local=TRUE)
           
           new_row <-  c(s_val, new_val,  
                         rb$NAI, rb$diff_NAI,
                         rb$milk_feed, rb$diff_milk_feed, 
                         rb$labor_repair, rb$diff_labor_repair, 
                         rb$capital, rb$diff_capital, 
                         rb$misc, rb$diff_misc,
                         rb$inflation, rb$diff_inflation,
                         rb$IOFC2 - rb$IOFC,  rb$diff_IOFC2 -rb$diff_IOFC,
                         rb$IOFC2_cwt - rb$IOFC_cwt,  rb$diff_IOFC2_cwt -rb$diff_IOFC_cwt,
                         rb$bw_wage_after_tax, rb$bw_wage_after_tax-rv$bw_wage_after_tax,  
                         rb$bw_wage_inflation_after_tax,
                         rb$bw_wage_inflation_after_tax - rv$bw_wage_inflation_after_tax
           ) %>% round(c(rep(0,24),rep(2,4),rep(3,2)))
           
           new_row <- matrix(c(label,new_row),nrow=1)
           colnames(new_row) <- s_colnames
           
           rb$table_scenario_after_tax[s,] <- new_row
           
           rb$NAI_spec <- "before tax"
           
           source("session_dashboard_robustness.R", local=TRUE)
           
           # --- add a row of results to the table_sensitivity_before_tax ---
           new_row <- c(s_val, new_val,  
                        rb$NAI, rb$diff_NAI,
                        rb$milk_feed, rb$diff_milk_feed, 
                        rb$labor_repair, rb$diff_labor_repair, 
                        rb$capital, rb$diff_capital, 
                        rb$misc, rb$diff_misc,
                        rb$inflation, rb$diff_inflation,
                        rb$IOFC2 - rb$IOFC,  rb$diff_IOFC2 -rb$diff_IOFC,
                        rb$IOFC2_cwt - rb$IOFC_cwt,  rb$diff_IOFC2_cwt -rb$diff_IOFC_cwt,
                        rb$bw_wage_before_tax, rb$bw_wage_before_tax-rv$bw_wage_before_tax,  
                        rb$bw_wage_inflation_before_tax,
                        rb$bw_wage_inflation_before_tax - rv$bw_wage_inflation_before_tax
           ) %>% round(c(rep(0,24),rep(2,4),rep(3,2)))
           
           new_row <- matrix(c(label,new_row),nrow=1)
           colnames(new_row) <- s_colnames
           
           rb$table_scenario_before_tax[s,] <- new_row
           
         }
  )
  updateRadioButtons(session,"NAI",NULL, choices=c("before tax","after tax"),
                     selected="before tax") 
  updateSelectInput(session, "s_choice","Scenario", selected="s1",
              choices=c("Increased investment"="s1",
                        "Use less pellets"="s2",
                        "New barn ($120k/stall)"="s3"
              ))
})


# input$s_choice (or rb$s_change_val) triggers this
observe({ 
  input$s_choice
  rb$s_change_val
  
  if (!(input$robot_parlor=="OFF" | input$profile_choice=="Robots")) { return() } 
  # Currently, Scenario is enabled only for Robots
  
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
    varnames <- s_varnames
    labels <- s_labels
  } else {
    varnames <- s_varnames_parlor
    labels <- s_labels_parlor
  }
  
  
  isolate({
    n <- s_n()
    s_val <- rb$s_change_val[,n]
    base_val <- lapply(varnames, 
                       function(x) {
                         input[[x]]
                       }) %>% unlist() 
    new_val <- (base_val * (1 + s_val/100))
    
    unit <- c("dollar","dollar", "lb/cow/day", "percent","lb/day") 
    
    val0 <-c(); val1<-c()

    d_round <- c(0,0,1,1,1)
    for (x in 1:5) {
        rb$s_txt[[x]] <- list()
        rb$s_txt[[x]][[1]] <- base_val[x] %>% round(d_round[x]) %>% helpText()
        if (base_val[x]!=new_val[x]) {
        rb$s_txt[[x]][[2]] <- new_val[x] %>% round(d_round[x])  %>% helpText()
        }
        rb$s_txt[[x]][[3]] <- unit[x]  %>% helpText()
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


