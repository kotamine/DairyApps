
# There are three ways the sensitivity analysis can be triggered. 
# 1. change of variable to consider the sensitivity 
# 2. change of value for that variable 
# 3. triger of "calculate" button after a change of any input value 

shinyjs::onclick("sensitivity_show",
                 { 
                   shinyjs::toggle(id="sensitivity_control", anim = TRUE) 
                   shinyjs::toggle(id="scenario_control", anim = TRUE)
                   shinyjs::toggle(id="dashboard_robust", anim = TRUE) 
 
                 }
)


# ----------- Sensitivity Analysis -----------
# This is not used as a reactive object but as a vector updated by a child function  
rb$c_change_val <- c(20, 20, 50, 50, -50, -50, 50) 

# creating emptry tables that reactively renders
rb$table_sensitivity_before_tax <- c_empty_table
rb$table_sensitivity_after_tax <- c_empty_table


c_n <- reactive({
  as.integer(gsub("c","", input$c_choice))
}) 

observeEvent(input$c_choice, {
  c_val <- rb$c_change_val[c_n()]
  updateNumericInput(session,"c_val",NULL, value=c_val, step=10)
})


# Update table_sensitivity when input$c_val or input$choice is changed as well as initial set up
observe({

  if (input$robust=="Off") { return() }

  input$c_val
  input$c_choice
  closeAlert(session, "ref_c_toggle")
  
  isolate({ 
    n <- c_n()
    
    # update the value for % change: reactive object is used as a storage 
    rb$c_change_val[n] <- input$c_val  
    c_choice <-  input$c_choice
    
    c_val <- rb$c_change_val[n]
    
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
        varnames <- c_varnames
        labels <- c_labels
      } else {
        varnames <- c_varnames_parlor
        labels <- c_labels_parlor
      }
    
    base_val <- input[[varnames[n]]]
    new_val <- (base_val * (1 + c_val/100))
    label <- labels[n]
    robust <- "Sensitivity" 
    
    source("session_calculations_robustness.R", local=TRUE)
    
    rb$NAI_spec <- input$NAI
    
    source("session_dashboard_robustness.R", local=TRUE)
    
    if (input$NAI=="before tax") {
      # --- add a row of results to the table_sensitivity_before_tax ---
      new_row <- c(c_val, base_val, new_val,  
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
      ) %>% round(c(rep(0,17),rep(2,4),rep(3,2)))

      new_row <- matrix(c(label,new_row),nrow=1)
      colnames(new_row) <- c_colnames
      
      rb$table_sensitivity_before_tax[n,] <- new_row
    } else {
      rb$NAI_spec <- "after tax"
      source("session_dashboard_robustness.R", local=TRUE)
      
      new_row <- c(c_val, base_val, new_val,  
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
      ) %>% round(c(rep(0,17),rep(2,4),rep(3,2)))

      new_row <- matrix(c(label,new_row),nrow=1)
      colnames(new_row) <- c_colnames
      
      rb$table_sensitivity_after_tax[n,] <- new_row
    }
  })
})

# uodate sensitivity choices
observe({
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
    updateSelectInput(session, "c_choice",NULL, selected="c1",
                      choices=c("Estimated cost per robot"="c1",
                                "Related housing changes needed per cow"="c2",
                                "Estimated annual change in milking system repair"="c3",
                                "Robots: years of useful life"="c4",
                                "Value of the robots after useful life"="c5",
                                "Anticipated savings in milking & chore labor"="c6",
                                "Projected change in milk production"="c7"
                      ))
    } else {
      updateSelectInput(session, "c_choice",NULL, selected="c1",
                        choices=c("Estimated cost of parlors"="c1",
                                  "Related housing changes needed per cow"="c2",
                                  "Estimated annual change in milking system repair"="c3",
                                  "Parlors: years of useful life"="c4",
                                  "Salvage value of parlors after useful life"="c5",
                                  "Anticipated savings in milking & chore labor"="c6",
                                  "Projected change in milk production"="c7"
                        ))
  }
})


# Recalculate all rows of table_sensitivity 
observeEvent(input$sensitivity_calculate, {
  
  rb$table_sensitivity <- c_empty_table # Returning to an emptry table
  closeAlert(session, "ref_c_input_change")
  closeAlert(session, "ref_c_toggle")
  
  robust <- "Sensitivity" 
 

  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(7:1), 
         function(x) { 
           
           c_choice <-  paste0("c",x) 
           c_val <- rb$c_change_val[x]
           
           if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
             varnames <- c_varnames
             labels <- c_labels
           } else {
             varnames <- c_varnames_parlor
             labels <- c_labels_parlor
           }
           
           base_val <- input[[varnames[x]]]
           new_val <- (base_val * (1 + c_val/100))
           label <- labels[x]
           
           #              source("session_calculation_robustness.R", local=TRUE)  # Calculates new_row
           #              rb$table_sensitivity[x,] <- new_row
           #              
           #              
           
           source("session_calculations_robustness.R", local=TRUE)
           
           rb$NAI_spec <- "after tax"
           source("session_dashboard_robustness.R", local=TRUE)
           
           new_row <- c(c_val, base_val, new_val,  
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
           ) %>% round(c(rep(0,17),rep(2,4),rep(3,2)))
           
           new_row <- matrix(c(label,new_row),nrow=1)
           colnames(new_row) <- c_colnames
           
           rb$table_sensitivity_after_tax[x,] <- new_row
           
           rb$NAI_spec <- "before tax"
           
           source("session_dashboard_robustness.R", local=TRUE)
           
           # --- add a row of results to the table_sensitivity_before_tax ---
           new_row <- c(c_val, base_val, new_val,  
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
           ) %>% round(c(rep(0,17),rep(2,4),rep(3,2)))
           
           new_row <- matrix(c(label,new_row),nrow=1)
           colnames(new_row) <- c_colnames

           rb$table_sensitivity_before_tax[x,] <- new_row
           

         }
  )
  updateRadioButtons(session,"NAI",NULL, choices=c("before tax","after tax"),
                                                 selected="before tax") 
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
    updateSelectInput(session, "c_choice",NULL, selected="c1",
                      choices=c("Estimated cost per robot"="c1",
                                "Related housing changes needed per cow"="c2",
                                "Estimated annual change in milking system repair"="c3",
                                "Robots: years of useful life"="c4",
                                "Value of the robots after useful life"="c5",
                                "Anticipated savings in milking & chore labor"="c6",
                                "Projected change in milk production"="c7"
                      ))
  } else {
    updateSelectInput(session, "c_choice",NULL, selected="c1",
                      choices=c("Estimated cost of parlors"="c1",
                                "Related housing changes needed per cow"="c2",
                                "Estimated annual change in milking system repair"="c3",
                                "Parlors: years of useful life"="c4",
                                "Salvage value of parlors after useful life"="c5",
                                "Anticipated savings in milking & chore labor"="c6",
                                "Projected change in milk production"="c7"
                      ))
  }
})


c_val_list <- c(c(-5:-1)*10,c(1:5)*10)
rb$calculation_plot <- FALSE
# Plot Calculation at various values of percentage change 
observe({
  input$c_choice  # Triggered by input$c_choice
  
  isolate({
  robust <- "Sensitivity"
  rb$calculation_plot <- TRUE
  rv$table_plot_robust <- nulls(length(c_val_list),3)
  c_choice <-  input$c_choice 
  
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
      varnames <- c_varnames
      labels <- c_labels
    } else {
      varnames <- c_varnames_parlor
      labels <- c_labels_parlor
    }
  
    for (n in 1:length(c_val_list)) {
      
    c_val <- c_val_list[n]
    base_val <- input[[varnames[n]]]
    new_val <- (base_val * (1 + c_val/100))
    label <- labels[n]
    robust <- "Sensitivity" 
    
    source("session_calculations_robustness.R", local=TRUE)
    
    rv$table_plot_robust[n,1] <- c_val_list[n]
    rv$table_plot_robust[n,2] <- rb$net_annual_impact_before_tax
    rv$table_plot_robust[n,3] <- rb$net_annual_impact_after_tax
    }
    colnames(rv$table_plot_robust) <- c("change","net_annual_impact_before_tax","net_annual_impact_after_tax")
    rv$table_plot_robust <- rv$table_plot_robust %>% data.frame()
  rb$calculation_plot <- FALSE 
  })
})


output$plot_robust <- renderGvis({
  if (length(rv[["table_plot_robust"]])==0) return()
  tbl <- round(rv[["table_plot_robust"]])
  tbl$Impact_Before_Tax <- tbl$net_annual_impact_before_tax
  tbl$Impact_After_Tax <- tbl$net_annual_impact_after_tax
  
  gvisLineChart(tbl, xvar="change", 
                yvar=c("Impact_Before_Tax","Impact_After_Tax"), 
                options=list(title=paste("Sensitivity Plot: ", c_labels_parlor[c_n()]),
                             vAxis=paste("{title:'Net Annual Impact under",  robot_or_parlor()," ($)'}"),
                             hAxis="{title:'% Change'}", 
                             legend="bottom",
                             width=800, height=400
                ))
}) 




# input$c_choice (or input$c_val via rb$c_change_val) triggers this
output$c_text <- renderUI({
  n <- c_n()
  
  isolate( c_choice <-  input$c_choice )
  c_val <- rb$c_change_val[n]
  
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { 
    varnames <- c_varnames
  } else {
    varnames <- c_varnames_parlor
  }
  
  base_val <- input[[varnames[n]]]
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
  
  paste("from", val0," to ", val1, unit) %>% helpText()
})



# --- Dashboard features for Robustness Analysis ---

output$c_NAI <- renderUI({
  dash_NAI(rb$NAI,cutoff=0, difference=rb$diff_NAI)
}) 

output$c_IOFC <- renderUI({
  isolate(IOFC_unit <- input$IOFC)
  if (IOFC_unit=="per cow") {
    dash_IOFC(rb$IOFC, rb$IOFC2, basis=IOFC_unit, 
              difference= rb$diff_IOFC2-rb$diff_IOFC)
  } else {
    dash_IOFC(rb$IOFC_cwt, rb$IOFC2_cwt, basis=IOFC_unit, 
              difference= rb$diff_IOFC2_cwt -rb$diff_IOFC_cwt)
  }
})   


output$c_milk_feed <- renderUI({ 
  validate( 
    need(!is.na(rb$diff_milk_feed), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rb$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      rb$diff_milk_feed %>% formatdollar2b() %>% strong() %>% h5())
})   


output$c_labor_repair <- renderUI({
  validate( 
    need(!is.na(rb$diff_labor_repair), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rb$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      rb$diff_labor_repair %>% formatdollar2b() %>% strong() %>% h5())
})  

output$c_captial_cost <- renderUI({
  validate( 
    need(!is.na(rb$diff_capital), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rb$capital %>% formatdollar2() %>% strong() %>% h4(),
      rb$diff_capital %>% formatdollar2b() %>% strong() %>% h5())
})  

output$c_misc <- renderUI({
  validate( 
    need(!is.na(rb$diff_misc), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rb$misc %>% formatdollar2() %>% strong %>% h4(), 
      rb$diff_misc %>% formatdollar2b() %>% strong() %>% h5())
})  


output$c_inflation <- renderUI({
  validate(
    need(!is.na(rb$diff_inflation ),"NA")
  )
  div(class="well well-sm", style= "background-color: #7A5DC7; color:white;", 
      rb$inflation %>% formatdollar2() %>% strong %>% h4(), 
      rb$diff_inflation %>% formatdollar2b() %>% strong() %>% h5())
})  


output$c_plot1 <- renderPlot({ 
  dash_plot1(rb$feed_current,rb$feed_robot,rb$milk_current,rb$milk_robot)
})

output$c_plot2 <- renderPlot({
  dash_plot2(rb$inc_exp_repair,rb$labor_current,rb$labor_robot) 
})

output$c_plot3 <- renderPlot({  
  dash_plot3(rb$capital_recovery_robot2,rb$capital_recovery_housing2,
             rb$cost_downpayment, rb$robot_end_PV)  
})


output$c_cashflow <- renderUI({
  validate(
    need(!is.na(rb$table_cash_flow ),"NA")
  )
  min <- min(rb$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  avg <- mean(rb$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  max <- max(rb$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  sd <- sd(rb$table_cash_flow$after_tax_cash_flow) %>% formatdollar()
  pos <- paste0(round(sum(rb$table_cash_flow$after_tax_cash_flow>0)/input$horizon*100),"%")
  
  div(class="well well-sm", style= "background-color: 	#778899; color:white;", 
      h4("Cash Flow", align="center"),
      h5("Min:", min), 
      h5("Avg:", avg),
      h5("Max:", max),
      h5("S.D.:", sd),
      h5(pos, "stays positive")
  ) 
}) 


output$c_cashflow2 <- renderGvis({
  if (length(rb[["table_cash_flow"]])==0) return()
  tbl <- round(rb[["table_cash_flow"]])
  tbl$Year <- tbl$year
  tbl$Cashflow <- tbl$after_tax_cash_flow 
  gvisAreaChart(tbl, xvar="Year", 
                yvar=c("Cashflow"),
                options=list(
                  title="After-tax Cash Flow", 
                  vAxis= paste("{title:'Impact under", robot_or_parlor(),"($)'}"),
                  hAxis="{title:'Year'}",
                  legend="none"
                ))
})


output$c_breakeven2 <- renderGvis({
  validate(
    need(!is.na(rb$bw_wage_before_tax),"NA")
  )
  if (input$NAI=="before tax") { 
    labor_rate <- rb$bw_wage_before_tax
    inflation <- rb$bw_wage_inflation_before_tax
    tax <- "(Before Tax)"
  } else {
    labor_rate <- rb$bw_wage_after_tax 
    inflation <- rb$bw_wage_inflation_after_tax
    tax <- "(After Tax)"
  }
  
  df <- data.frame(Year=c(1:input$horizon))
  df$Base <- lapply(c(1:input$horizon), function(t) { 
    input$labor_rate * (1 + input$inflation_labor/100)^(t-1)
  }) %>% unlist() %>% round(2)
  df$Wage <- lapply(c(1:input$horizon), function(t) { 
    labor_rate * (1 + input$inflation_labor/100)^(t-1)
  }) %>% unlist() %>% round(2)
  df$Wage_Inflation <- lapply(c(1:input$horizon), function(t) { 
    input$labor_rate * (1 + inflation)^(t-1)
  }) %>% unlist() %>% round(2)
  
  gvisLineChart(df, xvar="Year", 
                yvar=c("Base", "Wage","Wage_Inflation"),
                options=list(
                  title=paste("Breakeven Wage for",robot_or_parlor(),tax), 
                  vAxis="{title:'Wage Trajectory ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom"
                ))
})

output$c_breakeven <- renderUI({ 
  validate(
    need(!is.na(rb$bw_wage_before_tax),"NA")
  )
  if (input$breakeven_option=="wage") {
    option <- "Wage:"
    if (input$NAI=="before tax") { 
      labor_rate <- rb$bw_wage_before_tax
    } else {
      labor_rate <- rb$bw_wage_after_tax
    }
    be_val <- paste0("$", round(labor_rate, 2))
    inflation <-  input$inflation_labor/100
  } else {
    option <- "Inflation:"
    if (input$NAI=="before tax") { 
      inflation <- rb$bw_wage_inflation_before_tax
    } else {
      inflation <- rb$bw_wage_inflation_after_tax
    }
    be_val <- paste0(round(inflation*100,3),"%") 
    labor_rate <- input$labor_rate
  }
  
  yr_one <- paste("Year", round(input$horizon/3), ": ")
  yr_two <- paste("Year", round(input$horizon*(2/3)),": ")
  yr_three <- paste("Year", round(input$horizon),": ")
  wage_zero <- labor_rate  %>% formatdollar(2)
  wage_one <- (labor_rate * (1 + inflation)^(round(input$horizon/3)-1))  %>% formatdollar(2)
  wage_two <- (labor_rate * (1 + inflation)^(round(input$horizon*2/3)-1))  %>% formatdollar(2)
  wage_three <- (labor_rate * (1 + inflation)^(round(input$horizon)-1))  %>% formatdollar(2)
  
  div(class="well well-sm", style= "background-color:	#778899; color:white;", 
      h4("Breakeven", option, be_val, align="center"),
      h5("Year 1: ", wage_zero),
      h5(yr_one, wage_one),
      h5(yr_two, wage_two),
      h5(yr_three, wage_three)
  )
}) 





