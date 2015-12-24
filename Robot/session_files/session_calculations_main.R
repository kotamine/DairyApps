


lapply(base_profiles, function(x) {
  
  observe(priority=100, {
    
    # Calculations given a profile x
  
    # Make this reactive to all data inputs; 
    lapply(list_inputs_shared, function(z) input[[paste(z)]])
    lapply(list_inputs_profile, function(z) input[[paste0(z,x)]])
    if (x=="Robots") lapply(list_inputs_feed, function(z) input[[paste(z)]])

    isolate({
      X <- x  # Capital X != x will be used for sensitivity analysis etc.
      
      ans[[X]]$milk_change  <- input[[paste0("milk_change",x)]] 
      ans[[X]]$labor_rate  <- input$labor_rate 
      ans[[X]]$inflation_robot  <- input$inflation_robot
      ans[[X]]$inflation_margin <- input$inflation_margin 
      ans[[X]]$inflation_labor  <- input$inflation_labor 
      
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

