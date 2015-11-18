

varnames_production_change <- c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
                                "anticipated_hours_heat","increase_rc_mgt",
                                "decrease_lab_mgt", "milk_change","scc_change","software", 
                                "pellets","cost_pellets","change_turnover","change_electricity",
                                "change_water", "change_chemical",
                                "down_housing", "down_milking1", "down_milking2",
                                "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
                                "salvage_housing", "salvage_milking1", 
                                "milking_years")

mins_production_change <- c(rep(0,12), -10, rep(0,11), 1)
steps_production_change <- c(10, 500, 0.1, 0.2, 
                             0.05, 0.1,
                            0.1, 2, 0.25, 5, 
                            2, 2, 0.25, 0.25,
                            0.25, 0.25,
                            rep(20000,3), rep(1,3),rep(5000,2),1)


update_profile_vars  <- function(varnames, mins, steps, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  updateNumericInput(session, paste0(x),NULL, 
                     value=input[[paste0(x,varname_ref)]], 
                     min=mins[i],
                     step=steps[i])
  }
}

update_profile_vars2  <- function(varnames, mins, steps, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  updateNumericInput(session, paste0(x,varname_ref),NULL, 
                     value=input[[paste0(x)]], 
                     min=mins[i],
                     step=steps[i])
  }
}


use_profile_vars  <- function(varnames, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  {   x <- varnames[i]
      rp[[paste0(x)]] <- input[[paste0(x,varname_ref)]]
  }
}


# update variables in Data Entry when their counterparts change in Robot vs Parlors
observe({
  if (input$robot_parlor=="OFF") { return() }
  if (input$profile_choice=="Barn Only") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr1")
    updateNumericInput(session, "cost_parlors", NULL, value=0,min=0,step=10000)
    updateNumericInput(session, "milking_years", NULL, value=input$milking_years_pr1,min=0,step=1)
    
  }
  else if (input$profile_choice=="Retrofit Parlors") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr2")
    updateNumericInput(session, "cost_parlors", NULL, value=input$cost_parlors_pr2,min=0,step=10000)
    updateNumericInput(session, "milking_years", NULL, value=input$milking_years_pr1,min=0,step=1)

  } else if (input$profile_choice=="New Parlors") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr3")
    updateNumericInput(session, "cost_parlors", NULL, value=input$cost_parlors_pr3,min=0,step=10000)
    updateNumericInput(session, "milking_years", NULL, value=input$milking_years_pr3,min=0,step=1)
    
  } else {
    update_profile_vars(varnames_production_change,
                         mins_production_change, 
                         steps_production_change, 
                         varname_ref="_pr4")
    updateNumericInput(session, "n_robot",NULL,value=input$n_robot_pr4,min=0,step=1)
    updateNumericInput(session, "cost_robot",NULL,value=input$cost_robot_pr4,min=50000,step=10000)
    updateNumericInput(session, "robot_years", NULL, value=input$robot_years_pr4,min=0,step=1)
  }
})


# update variables in Robot vs Parlors when their counterparts change in Data Entry 
observe({
  isolate(profile <- input$profile_choice)
  if (profile=="Barn Only") {
    update_profile_vars2(varnames_production_change,
                         mins_production_change, 
                         steps_production_change, 
                         varname_ref="_pr1")
    updateNumericInput(session, "milking_years_pr1", NULL, value=input$milking_years,min=0,step=1)
    
  }
  else if (profile=="Retrofit Parlors") {
    update_profile_vars2(varnames_production_change,
                         mins_production_change, 
                         steps_production_change, 
                         varname_ref="_pr2")
    updateNumericInput(session, "cost_parlors_pr2", NULL, value=input$cost_parlors,min=0,step=10000)
    updateNumericInput(session, "milking_years_pr2", NULL, value=input$milking_years,min=0,step=1)
    
  } else if (profile=="New Parlors") {
    update_profile_vars2(varnames_production_change,
                         mins_production_change, 
                         steps_production_change, 
                         varname_ref="_pr3")
    updateNumericInput(session, "cost_parlors_pr3", NULL, value=input$cost_parlors,min=0,step=10000)
    updateNumericInput(session, "milking_years_pr3", NULL, value=input$milking_years,min=0,step=1)
    
  } else {

    update_profile_vars2(varnames_production_change,
                         mins_production_change, 
                         steps_production_change, 
                         varname_ref="_pr4")
    updateNumericInput(session, "n_robot_pr4",NULL,value=input$n_robot,min=0,step=1)
    updateNumericInput(session, "cost_robot_pr4",NULL,value=input$cost_robot,min=50000,step=10000)
    updateNumericInput(session, "robot_years_pr4", NULL, value=input$robot_years,min=0,step=1)
  }
})


# Update section B: Financing Schedule by Profile
observe({
  if (input$robot_parlor=="OFF") { return() }
  isolate(profile <- input$profile_choice)
  if (profile=="Barn Only") {
    rv$copy_cost_housing_pr1 <- input$herd_size + input$herd_increase_pr1
    rv$loan_housing_pr1 <- rv$copy_cost_housing_pr1 - input$down_housing_pr1
    rv$copy_r_housing_pr1 <- input$interest
  } else if (profile=="Retrofit Parlors") {
    rv$housing_years_pr2 <- input$milking_years_pr2
    rv$copy_cost_housing_pr2 <- input$herd_size + input$herd_increase_pr2
    rv$copy_cost_milking1_pr2 <- input$cost_parlors_pr2
    rv$loan_housing_pr2 <- rv$copy_cost_housing_pr2 - input$down_housing_pr2
    rv$loan_milking1_pr2 <-rv$copy_cost_milking1_pr2 - input$down_milking1_pr2
    rv$copy_r_housing_pr2 <- input$interest
    rv$copy_r_milking1_pr2 <- input$interest
  } else if (profile=="New Parlors") {
    rv$housing_years_pr3 <- input$milking_years_pr3
    rv$copy_cost_housing_pr3 <-  input$herd_size + input$herd_increase_pr3
    rv$copy_cost_milking1_pr3 <- input$cost_parlors_pr3
    rv$loan_housing_pr3 <-  rv$copy_cost_housing_pr3 - input$down_housing_pr3
    rv$loan_milking1_pr3 <- rv$copy_cost_milking1_pr3 - input$down_milking1_pr3
    rv$copy_r_housing_pr3 <- input$interest
    rv$copy_r_milking1_pr3 <- input$interest
  } else {
    rv$housing_years_pr4 <- input$n_robot_life * input$robot_years
    rv$copy_cost_housing_pr4 <-  input$herd_size + input$herd_increase_pr3
    rv$copy_cost_milking1_pr4 <- input$n_robot * input$cost_robot
    rv$copy_cost_milking2_pr4 <-  rv$copy_cost_milking1_pr4*(1+input$inflation_robot/100)^input$robot_years
    rv$loan_housing_pr4 <- rv$copy_cost_housing_pr4 - input$down_housing_pr4
    rv$loan_milking1_pr4 <- rv$copy_cost_milking1_pr4 - input$down_milking1_pr4
    rv$loan_milking2_pr4 <- rv$copy_cost_milking2_pr4 - input$down_milking2_pr4
    rv$copy_r_housing_pr4 <- input$interest
    rv$copy_r_milking1_pr4 <- input$interest
    rv$copy_r_milking2_pr4 <- input$interest
    rv$copy_robot_years_pr4 <- input$robot_years
    rv$copy_n_robot_pr4 <- input$n_robot 
    rv$copy_cost_robot_pr4 <- input$cost_robot*(1+input$inflation_robot/100)^input$robot_years
    rv$salvage_milking2_pr4 <-  input$salvage_milking1_pr4*(1+input$inflation_robot/100)^input$robot_years
    rv$yr_invest_milking2_pr4 <- input$robot_years
  }
})



# creating an emptry table that reactively renders
rp$table_before_tax <- matrix(NA) %>% data.frame()
rp$table_after_tax  <- matrix(NA) %>% data.frame()
rp$table_operating_income  <-  matrix(NA) %>% data.frame()
rp$table_after_tax_cash_flow  <-  matrix(NA) %>% data.frame()

# Calculate rp$table_profiles
observeEvent(input$robot_parlor_calculate, {
  
  n_years <- max(input$milking_years_pr1,
                 input$milking_years_pr2,
                 input$milking_years_pr3,
                 input$robot_years_pr4*input$n_robot_life) + 1
  
  varnames <- c("Net Annual Impact", "Milk Income minus Feed Cost","Labor and Repair Cost", 
                "Cost of Capital", "Others","Inflation Adjustments", 
                "IOFC per cow","IOFC per cwt",  "Breakeven Wage", "Breakeeven Wage Inflation")

  rp$table_before_tax <- cbind(rep(NA,length(varnames)))
  rp$table_after_tax  <-  cbind(rep(NA,length(varnames)))
  rp$table_operating_income <- cbind(Year=c(1:n_years)-1) 
  rp$table_after_tax_cash_flow  <- cbind(Year=c(1:n_years)-1)
  
  closeAlert(session, "ref_p_input_change")
  choices <- c("Barn Only","Retrofit Parlors","New Parlors","Robots")
  profile_ref <- c("_pr1", "_pr2", "_pr3", "_pr4")
  for (p in 1:4) {
    
    profile <- choices[p]
    
    use_profile_vars(varnames_production_change, profile_ref[p])
    
    source("session_calculations_robot_parlor.R", local=TRUE)
    
    if (n_years > length(rp$table_cash_flow$operating_income)) {
      tmp_zero <- rep(0, n_years - length(rp$table_cash_flow$operating_income))
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         c(rp$table_cash_flow$operating_income,tmp_zero))
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                         profile = c(rp$table_cash_flow$after_tax_cash_flow,tmp_zero))
    } else {
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         profile  = rp$table_cash_flow$operating_income)
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                            profile  = rp$table_cash_flow$after_tax_cash_flow)      
    } 
    
    rp$NAI_spec <- "before tax"
    source("session_dashboard_robot_parlor.R", local=TRUE)
    
    rp$table_before_tax <-  cbind(rp$table_before_tax, 
                                  profile =c(rp$NAI, rp$milk_feed,  rp$labor_repair, rp$capital, 
                                               rp$misc,  rp$inflation, 
                                               rp$IOFC2 - rp$IOFC, 
                                               rp$IOFC2_cwt - rp$IOFC_cwt,
                                               rp$bw_wage_before_tax,
                                               rp$bw_wage_inflation_before_tax)) 
      
    rp$NAI_spec <- "after tax"
    source("session_dashboard_robot_parlor.R", local=TRUE)
    
    rp$table_after_tax <-   cbind(rp$table_after_tax, 
                                  profile =c(rp$NAI, rp$milk_feed,  rp$labor_repair, rp$capital, 
                                               rp$misc,  rp$inflation, 
                                               rp$IOFC2 - rp$IOFC, 
                                               rp$IOFC2_cwt - rp$IOFC_cwt,
                                               rp$bw_wage_after_tax,
                                              rp$bw_wage_inflation_after_tax)) 
  }

  rownames(rp$table_before_tax) <- varnames
  rownames(rp$table_after_tax)  <- varnames
  colnames(rp$table_before_tax) <- c("Year",choices)
  colnames(rp$table_after_tax) <- c("Year",choices)
  colnames(rp$table_operating_income) <- c("Year",choices)
  colnames(rp$table_after_tax_cash_flow) <- c("Year",choices)
  rp$table_before_tax <- rp$table_before_tax[,-1] %>% data.frame() 
  rp$table_after_tax <- rp$table_after_tax[,-1] %>% data.frame() 
  rp$table_operating_income <- rp$table_operating_income %>% data.frame() 
  rp$table_after_tax_cash_flow <- rp$table_after_tax_cash_flow %>% data.frame() 
  
})



output$table_before_tax <- DT::renderDataTable({
  if (length(rp[["table_before_tax"]])==0) return()
  tbl <- rp[["table_before_tax"]]
  tbl[7:9,] <- tbl[7:9,]*100
  tbl[10,] <- tbl[10,]*1000
  tbl <- round(tbl)
  tbl[7:9,] <- tbl[7:9,]/100
  tbl[10,] <- tbl[10,]/1000
  
  DT::datatable(tbl, 
                options = list(
                  scrollX = TRUE,
                  pageLength = 10,
                  bLengthChange =FALSE,
                  searching = FALSE
                  )) %>% 
    formatCurrency(colnames(tbl[-10]))
})


output$table_after_tax <- DT::renderDataTable({
  if (length(rp[["table_after_tax"]])==0) return()
  tbl <- rp[["table_after_tax"]]
  tbl[7:9,] <- tbl[7:9,]*100
  tbl[10,] <- tbl[10,]*1000
  tbl <- round(tbl)
  tbl[7:9,] <- tbl[7:9,]/100
  tbl[10,] <- tbl[10,]/1000
  
  DT::datatable(tbl, 
                options = list(
                  scrollX = TRUE,
                  pageLength = 10,
                  bLengthChange =FALSE,
                  searching = FALSE
                )) %>% 
    formatCurrency(colnames(tbl[-10]))
})


output$table_operating_income  <- DT::renderDataTable({
  if (length(rp[["table_operating_income"]])==0) return()
  tbl <- round(rp[["table_operating_income"]])
  L <- length(tbl$Year)
  DT::datatable(tbl, 
                rownames = FALSE,
                options = list(
                  scrollX = TRUE,
                  pageLength = L,
                  bLengthChange =FALSE,
                  searching = FALSE)) %>% 
    formatCurrency(colnames(tbl)[-1])
})


output$table_after_tax_cash_flow  <- DT::renderDataTable({
  if (length(rp[["table_after_tax_cash_flow"]])==0) return()
  tbl <- round(rp[["table_after_tax_cash_flow"]])
  L <- length(tbl$Year)
  DT::datatable(tbl, 
                rownames = FALSE,
                options = list(
                  scrollX = TRUE,
                  pageLength = L,
                  bLengthChange =FALSE,
                  searching = FALSE)) %>% 
    formatCurrency(colnames(tbl)[-1])
})



output$profile_cashflow_chart <- renderGvis({
  if (length(rp[["table_after_tax_cash_flow"]])==0) return()
  tbl <- round(rp[["table_after_tax_cash_flow"]]) 
  varnames <- colnames(tbl)[-1]  
  gvisLineChart(tbl, xvar="Year", 
                yvar= varnames,
                options=list(
                             title="After-tax Cash Flow", 
                             vAxis="{title:'Net Annual Impact ($)'}",
                             hAxis="{title:'Year'}",
                             legend="bottom",
                             width=800, height=400
                ))
})

output$profile_operating_income_chart <- renderGvis({
  if (length(rp[["table_operating_income"]])==0) return()
  tbl <- round(rp[["table_operating_income"]]) 
  varnames <- colnames(tbl)[-1]  
  gvisLineChart(tbl, xvar="Year", 
                yvar= varnames,
                options=list(
                  title="Before-tax Operating Income", 
                  vAxis="{title:'Net Annual Impact ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom",
                  width=800, height=400
                ))
})

output$profile_impacts <- renderGvis({ 
  if (length(rp[["table_after_tax"]])==0) return()

  tbl <- data.frame(
      profile=c("Barn Only","Retrofit Parlors","New Parlors","Robots"),
      before.tax=as.numeric(round(rp$table_before_tax[1,])),
      after.tax =as.numeric(round(rp$table_after_tax[1,]))
    ) 
  
  gvisColumnChart(tbl, xvar="profile",
                  yvar=c("before.tax","after.tax"),
                  options=list(title="Net Annual Impact ($)",
                               titleTextStyle="{fontSize:16}",
                               legend="top")
  )
})


