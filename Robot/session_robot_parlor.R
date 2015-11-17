

varnames_production_change <- c("herd_increase", "repair","insurance","hr_sv_milking", 
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


update_profile_vars  <- function(varnames, mins, steps, profile_choice, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  updateNumericInput(session, paste0(x),NULL, 
                     value=input[[paste0(x,varname_ref)]], 
                     min=mins[i],
                     step=steps[i])
  }
}

update_profile_vars2  <- function(varnames, mins, steps, profile_choice, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  updateNumericInput(session, paste0(x,varname_ref),NULL, 
                     value=input[[paste0(x)]], 
                     min=mins[i],
                     step=steps[i])
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


# creating an emptry table that reactively renders
rp$table_before_tax <- matrix(NA) %>% data.frame()
rp$table_after_tax  <- matrix(NA) %>% data.frame()
rp$table_operating_income  <-  matrix(NA) %>% data.frame()
rp$table_after_tax_cash_flow  <-  matrix(NA) %>% data.frame()

# Calculate rp$table_profiles
observeEvent(input$robot_parlor_calculate, {
  
  browser()

  rp$table_before_tax <- matrix(NA)
  rp$table_after_tax  <- matrix(NA) 
  rp$table_operating_income <- matrix(NA)
  rp$table_after_tax_cash_flow  <- matrix(NA)
  
  n_years <- max(input$milking_years_pr1,
      input$milking_years_pr2,
      input$milking_years_pr3,
      input$robot_years_pr4*input$n_robot_life) + 1
    
  
#   closeAlert(session, "ref_c_input_change")
  choices <- c("Barn Only","Retrofit Parlors","New Parlors","Robots")
  for (p in 1:4) {
    
    browser()

    updateSelectInput(session, "profile_choice", NULL, selected=choices[p],
                      choices=choices)
    
    if (n_years > length(rv$table_cash_flow$operating_income)) {
      tmp_zero <- rep(0, n_years - length(rv$table_cash_flow$operating_income))
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         choices[i] = c(rv$table_cash_flow$operating_income,tmp_zero))
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                            choices[i] = c(rv$table_cash_flow$after_tax_cash_flow,tmp_zero))
    } else {
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         choices[i] = rv$table_cash_flow$operating_income)
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                            choices[i] = rv$table_cash_flow$after_tax_cash_flow)      
    } 
    
    updateRadioButtons(session, "NAI",NULL, choices=c("before tax", "after tax"),
                       selected="before tax")
    
    rp$table_before_tax <-  cbind(rp$table_before_tax, 
                                  choices[p]=c(rv$milk_feed,  rv$labor_repair, rv$capital, 
                                               rv$misc,  rv$inflation, rv$NAI, 
                                               rv$IOFC2 - rv$IOFC, 
                                               rv$IOFC2_cwt - rv$IOFC_cwt)) 
      
    updateRadioButtons(session, "NAI",NULL, choices=c("before tax", "after tax"),
                       selected="after tax")
    
    rp$table_after_tax <-   cbind(rp$table_after_tax, 
                                  choices[p]=c(rv$milk_feed,  rv$labor_repair, rv$capital, 
                                               rv$misc,  rv$inflation, rv$NAI, 
                                               rv$IOFC2 - rv$IOFC, 
                                               rv$IOFC2_cwt - rv$IOFC_cwt)) 
  }
  rp$table_operating_income <- cbind(rv$table_cash_flow$year,rp$table_operating_income) 
  rp$table_after_tax_cash_flow <- cbind(rv$table_cash_flow$year,rp$table_after_tax_cash_flow) 
  varmanes <- c("Milk - Feed","Labor + Repair", "Cost of Capital", 
    "Misc","Inflation Adj.", "Net Impact","IOFC per cow","IOFC per cwt")
  rp$table_before_tax <- cbind(Variable=varmanes,rp$table_before_tax ) %>% data.frame()
  rp$table_after_tax <- cbind(Variable=varmanes,rp$table_after_tax )  %>% data.frame()
  
})



output$table_before_tax <- DT::renderDataTable({
  if (length(rp[["table_before_tax"]])==0) return()
  tbl <- rp[["table_before_tax"]]
#   if (robot_parlor=="OFF" | input.profile_choice=="Robots") { milk_sys <- 'Robot' }
#   else { milk_sys <- 'Parlor'}
#   colnames(tbl) <- c('Year', milk_sys, 'Housing', 'Total')
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = dim(tbl)[2],
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(colnames(tbl)[-1])
})


output$profile_cashflow_chart <- renderGvis({
  if (length(rp[["table_after_tax_cash_flow"]])==0) return()
  tbl <- round(rp[["table_after_tax_cash_flow"]])
  browser()
  
  tbl$Year <- tbl$year
#   gvisAreaChart(tbl, xvar="Year", 
#                 yvar=c("Cashflow","Operating_Income"),
#                 options=list(isStacked=TRUE,
#                              title="Before-tax Operating Income & After-tax Cash Flow", 
#                              vAxis="{title:'Net Annual Impact under Robot ($)'}",
#                              hAxis="{title:'Year'}",
#                              legend="bottom",
#                              width=800, height=400
#                 ))
})

