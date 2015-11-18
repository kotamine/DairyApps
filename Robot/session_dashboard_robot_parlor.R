
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increase replaced by  rp$herd_increase
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "down_housing", "down_milking1", "down_milking2",
#     "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
#     "salvage_housing", "salvage_milking1", 
#     "milking_years") and input$NAI

#  ------ Dashboard portion of  Robot vs Parlor analysis -----------

isolate({
  
  rp$tax_factor <- (1-(rp$NAI_spec=="after tax")*input$tax_rate/100)
  
  if (rp$NAI_spec=="before tax") {
    rp$NAI <- rp$net_annual_impact_before_tax
  } else {
    rp$NAI <- rp$net_annual_impact_after_tax
  }
  
  rp$IOFC <- (input$milk_cow_day * input$price_milk/100 - rp$DMI_day * input$cost_DM )*330 * rp$tax_factor
  
  rp$IOFC2 <- (rp$milk_day_cow_alt * input$price_milk/100 + 
                 - rp$DMI_projected * input$cost_DM - rp$pellets * rp$cost_pellets/2000)*330 *
    rp$tax_factor  
  
  rp$IOFC_cwt <- rp$IOFC /365 /input$milk_cow_day * 330 * rp$tax_factor
  
  rp$IOFC2_cwt <- rp$IOFC2 /365 /rp$milk_day_cow_alt * 330 * rp$tax_factor
  
  rp$milk_current <- 
    input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                    +  input$scc_premium/100 * input$scc_average/1000) *
    rp$tax_factor
  
  rp$milk_robot <- (rp$herd_size2 * 330 * rp$milk_day_cow_alt *
                      (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-rp$scc_change/100)/1000)) *
    rp$tax_factor 
  
  rp$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365 *
    rp$tax_factor 
  
  rp$labor_robot <- ((rp$anticipated_hours_heat + rp$anticipated_hours_milking) * input$labor_rate *365 + 
                       + (rp$increase_rc_mgt - rp$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
                       + input$additional_labor * rp$herd_increase) * rp$tax_factor 
  
  rp$feed_current <-  rp$DMI_day * input$cost_DM * 330 * input$herd_size * rp$tax_factor
  
  rp$feed_robot <- (rp$DMI_projected * input$cost_DM + rp$pellets *
                      rp$cost_pellets/2000) * 330 * rp$herd_size2 * rp$tax_factor 
  
  rp$milk_feed <-  (-(rp$feed_robot - rp$feed_current) + rp$milk_robot -  rp$milk_current )
  
  rp$labor_repair <- -(rp$labor_robot - rp$labor_current + rp$inc_exp_repair) 
  
  rp$inflation <- - pmt(input$interest/100, rp$housing_years, 
                        npv(input$interest/100, rp$table_cash_flow$revenue_minus_expense[-1])) +
    - (rp$inc_rev_total + rp$dec_exp_total - rp$inc_exp_total) 
  
  rp$capital <- -rp$capital_cost_total + 
    +(rp$NAI_spec=="after tax")*(rp$tax_interest + rp$tax_depreciation)
  
  rp$misc <- rp$NAI - (rp$milk_feed + rp$labor_repair + rp$capital + rp$inflation) 
  
  rp$capital_recovery_robot2 <- rp$capital_recovery_robot +
    - (rp$NAI_spec=="after tax")*rp$tax_deduction_robot
  rp$capital_recovery_housing2 <- rp$capital_recovery_housing +
    - (rp$NAI_spec=="after tax")*rp$tax_deduction_housing
  
})


