


varnames_production_change <- c("herd_increase", "repair","insurance","anticipated_hours_heat","increase_rc_mgt",
  "decrease_lab_mgt", "milk_change","scc_change","software", 
  "pellets","cost_pellets","change_turnover","change_electricity",
  "change_water", "change_chemical",
  "down_housing", "down_milking1", "down_milking2",
  "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
  "salvage_housing", "salvage_milking1", 
  "milking_years")

mins_production_change <- c(rep(0,12), -100,rep(0,11),1)
steps_production_change <- c(10, 500, 0.1, 0.2, 0.05, 0.1,
                            0.1, 2, 0.25, 5, 2, 
                            2, 0.25, 0.25, 0.25, 0.25,
                            rep(20000,3), rep(1,3),rep(5000,2),15)

update_profile_vars  <- function(varnames, mins, steps, varname_ref=NULL) {
  lapply (varnames, function(x) {
    n <- which(varnames==paste0(x))
    updateNumericInput(session, paste0(x),NULL, 
                       value=input[[paste0(x,varname_ref)]],
                       min=mins[n],
                       step=steps[n])
  }) 
}

update_profile_vars2  <- function(varnames, mins, steps, varname_ref=NULL) {
  lapply (varnames, function(x) {
    n <- which(varnames==paste0(x))
    updateNumericInput(session, paste0(x,varname_ref),NULL, 
                       value=input[[paste0(x)]],
                       min=mins[n],
                       step=steps[n])
  }) 
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
  if (input$robot_parlor=="OFF") { return() }
  if (input$profile_choice=="Barn Only") {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr1")
    updateNumericInput(session, "milking_years_pr1", NULL, value=input$milking_years,min=0,step=1)
    
  }
  else if (input$profile_choice=="Retrofit Parlors") {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr2")
    updateNumericInput(session, "cost_parlors_pr2", NULL, value=input$cost_parlors,min=0,step=10000)
    updateNumericInput(session, "milking_years_pr2", NULL, value=input$milking_years,min=0,step=1)
    
  } else if (input$profile_choice=="New Parlors") {
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




