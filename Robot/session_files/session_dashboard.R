

# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increaseRobots, input$herd_increaseRetrofit, input$herd_increaseNew
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "cost_housing_cow",
#     "down_housing", "down_milking1", "down_milking2",
#     "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
#     "salvage_housing", "salvage_milking1", 
#     "milking_years", "cost_parlors", "cost_robot", "robot_years", "n_robot")
#     
#       and input$NAI

#  ------ Dashboard Features -----------

isolate({
  
  browser()
  
  ans[[x]]$tax_factor <- (1-(ans[[x]]$NAI_spec=="after tax")*input$tax_rate/100)
  
  if (ans[[x]]$NAI_spec=="before tax") {
    ans[[x]]$NAI <- ans[[x]]$net_annual_impact_before_tax
  } else {
    ans[[x]]$NAI <- ans[[x]]$net_annual_impact_after_tax
  }
  
  ans[[x]]$IOFC <- (input$milk_cow_day * input$price_milk/100 - ans[[x]]$DMI_day * input$cost_DM )*330 * ans[[x]]$tax_factor
  
  ans[[x]]$IOFC2 <- (ans[[x]]$milk_day_cow_alt * input$price_milk/100 + 
                       - ans[[x]]$DMI_projected * input$cost_DM - ans[[x]]$pellets * ans[[x]]$cost_pellets/2000)*330 *
    ans[[x]]$tax_factor  
  
  ans[[x]]$IOFC_cwt <- ans[[x]]$IOFC /365 /input$milk_cow_day * 330 * ans[[x]]$tax_factor
  
  ans[[x]]$IOFC2_cwt <- ans[[x]]$IOFC2 /365 /ans[[x]]$milk_day_cow_alt * 330 * ans[[x]]$tax_factor
  
  ans[[x]]$milk_current <- 
    input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                    +  input$scc_premium/100 * input$scc_average/1000) *
    ans[[x]]$tax_factor
  
  ans[[x]]$milk_robot <- (ans[[x]]$herd_size2 * 330 * ans[[x]]$milk_day_cow_alt *
                            (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-ans[[x]]$scc_change/100)/1000)) *
    ans[[x]]$tax_factor 
  
  ans[[x]]$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365 *
    ans[[x]]$tax_factor 
  
  ans[[x]]$labor_robot <- ((ans[[x]]$anticipated_hours_heat + ans[[x]]$anticipated_hours_milking) * input$labor_rate *365 + 
                             + (ans[[x]]$increase_rc_mgt - ans[[x]]$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
                             + input$additional_labor * ans[[x]]$herd_increase) * ans[[x]]$tax_factor 
  
  ans[[x]]$feed_current <-  ans[[x]]$DMI_day * input$cost_DM * 330 * input$herd_size * ans[[x]]$tax_factor
  
  ans[[x]]$feed_robot <- (ans[[x]]$DMI_projected * input$cost_DM + ans[[x]]$pellets *
                            ans[[x]]$cost_pellets/2000) * 330 * ans[[x]]$herd_size2 * ans[[x]]$tax_factor 
  
  ans[[x]]$milk_feed <-  (-(ans[[x]]$feed_robot - ans[[x]]$feed_current) + ans[[x]]$milk_robot -  ans[[x]]$milk_current )
  
  ans[[x]]$labor_repair <- -(ans[[x]]$labor_robot - ans[[x]]$labor_current + ans[[x]]$inc_exp_repair) 
  
  ans[[x]]$inflation <- - pmt(input$interest/100, ans[[x]]$planning_horizon, 
                              npv(input$interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
    - (ans[[x]]$inc_rev_total + ans[[x]]$dec_exp_total - ans[[x]]$inc_exp_total) 
  
  ans[[x]]$capital <- -ans[[x]]$capital_cost_total + 
    +(ans[[x]]$NAI_spec=="after tax")*(ans[[x]]$tax_interest + ans[[x]]$tax_depreciation)
  
  ans[[x]]$misc <- ans[[x]]$NAI - (ans[[x]]$milk_feed + ans[[x]]$labor_repair + ans[[x]]$capital + ans[[x]]$inflation) 
  
  ans[[x]]$capital_recovery_robot2 <- ans[[x]]$capital_recovery_robot +
    - (ans[[x]]$NAI_spec=="after tax")*ans[[x]]$tax_deduction_robot
  ans[[x]]$capital_recovery_housing2 <- ans[[x]]$capital_recovery_housing +
    - (ans[[x]]$NAI_spec=="after tax")*ans[[x]]$tax_deduction_housing
  
})

