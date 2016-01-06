


lapply(base_profiles, function(x) {
  
  observe(priority=100, {
    
    # browser()
    
    # Calculations given a profile x
  
    # Make this reactive to all data inputs; 
    lapply(list_inputs_shared, function(z) input[[paste(z)]])
    lapply(list_inputs_profile, function(z) input[[paste0(z,x)]])
    if (x=="Robots") lapply(list_inputs_feed, function(z) input[[paste(z)]])

    isolate({
      X <- x  # Capital X != x will be used for sensitivity analysis etc.
      
      ans[[X]]$milk_change  <- input[[paste0("milk_change",x)]] 
      ans[[X]]$labor_rate  <- input$labor_rate 
      ans[[X]]$cost_housing_cow <- input[[paste0("cost_housing_cow",x)]] 
      ans[[X]]$cost_robot <- input[[paste0("cost_robot",x)]]
      ans[[X]]$cost_parlors <- input[[paste0("cost_parlors",x)]] 
      ans[[X]]$useful_years <- input[[paste0("useful_years",x)]] 
      ans[[X]]$r_housing <- input[[paste0("r_housing",x)]] 
      ans[[X]]$r_milking1 <- input[[paste0("r_milking1",x)]]
      ans[[X]]$inflation_robot  <- input$inflation_robot
      ans[[X]]$inflation_margin <- input$inflation_margin 
      ans[[X]]$inflation_labor  <- input$inflation_labor 
      if (ans[[X]]$r_housing < 1e-8) ans[[X]]$r_housing <- 1e-8
      if (ans[[X]]$r_milking1 < 1e-8) ans[[X]]$r_milking1 <- 1e-8
      
      source(file.path("session_files", "session_calculation_steps.R"), local=TRUE)  # Calculates main results
      
    }) 
    
#     # This is used for alerting the base-value change in sensitivity and scenario analysis  
#     createAlert(session, "c_input_change", "ref_c_input_change", 
#                 content = "New data inputs. 
#                 Press ``Calculate'' to updated the results.",
#                 append = FALSE) 

    
  })  
})


#  ------ Dashboard  -----------
# Triggered by any change in ans[[x]]$net_annual_impact_before_tax 

lapply(base_profiles, function(x) {
  
  X <- x
  source(file.path("session_files", "session_dashboard.R"), local=TRUE)  # Calculates dashboard items
  
})

