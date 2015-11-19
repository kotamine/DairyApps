
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increase replaced by  rb$herd_increase
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "down_housing", "down_milking1", "down_milking2",
#     "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
#     "salvage_housing", "salvage_milking1", 
#     "milking_years") and input$NAI

#  ------ Dashboard portion of  Robustness analysis -----------
  
isolate({
  
  rb$tax_factor <- (1-(rb$NAI_spec=="after tax")*input$tax_rate/100)
  
  if (rb$NAI_spec=="before tax") {
    rb$NAI <- rb$net_annual_impact_before_tax
  } else {
    rb$NAI <- rb$net_annual_impact_after_tax
  }
  
  rb$IOFC <- (input$milk_cow_day * input$price_milk/100 - rb$DMI_day * input$cost_DM )*330 * rb$tax_factor
  
  rb$IOFC2 <- (rb$milk_day_cow_alt * input$price_milk/100 + 
                 - rb$DMI_projected * input$cost_DM - rb$pellets * rb$cost_pellets/2000)*330 *
    rb$tax_factor  
  
  rb$IOFC_cwt <- rb$IOFC /365 /input$milk_cow_day * 330 * rb$tax_factor
  
  rb$IOFC2_cwt <- rb$IOFC2 /365 /rb$milk_day_cow_alt * 330 * rb$tax_factor
  
  rb$milk_current <- 
    input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                    +  input$scc_premium/100 * input$scc_average/1000) *
    rb$tax_factor
  
  rb$milk_robot <- (rb$herd_size2 * 330 * rb$milk_day_cow_alt *
                      (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-rb$scc_change/100)/1000)) *
    rb$tax_factor 
  
  rb$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365 *
    rb$tax_factor 
  
  rb$labor_robot <- ((rb$anticipated_hours_heat + rb$anticipated_hours_milking) * input$labor_rate *365 + 
                       + (rb$increase_rc_mgt - rb$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
                       + input$additional_labor * rb$herd_increase) * rb$tax_factor 
  
  rb$feed_current <-  rb$DMI_day * input$cost_DM * 330 * input$herd_size * rb$tax_factor
  
  rb$feed_robot <- (rb$DMI_projected * input$cost_DM + rb$pellets *
                      rb$cost_pellets/2000) * 330 * rb$herd_size2 * rb$tax_factor 
  
  rb$milk_feed <-  (-(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current )
  
  rb$labor_repair <- -(rb$labor_robot - rb$labor_current + rb$inc_exp_repair) 
  
  rb$inflation <- - pmt(input$interest/100, rb$housing_years, 
                        npv(input$interest/100, rb$table_cash_flow$revenue_minus_expense[-1])) +
    - (rb$inc_rev_total + rb$dec_exp_total - rb$inc_exp_total) 
  
  rb$capital <- -rb$capital_cost_total + 
    +(rb$NAI_spec=="after tax")*(rb$tax_interest + rb$tax_depreciation)
  
  rb$misc <- rb$NAI - (rb$milk_feed + rb$labor_repair + rb$capital + rb$inflation) 
  
  rb$capital_recovery_robot2 <- rb$capital_recovery_robot +
    - (rb$NAI_spec=="after tax")*rb$tax_deduction_robot
  rb$capital_recovery_housing2 <- rb$capital_recovery_housing +
    - (rb$NAI_spec=="after tax")*rb$tax_deduction_housing
  
  
  # calculate differences from the base model 
  if (rb$NAI_spec=="before tax") {
    NAI <- net_annual_impact_before_tax()
  } else {
    NAI <- net_annual_impact_after_tax()
  }
  
  if (input$NAI==rb$NAI_spec) {
    tax_coeff <- 1
  } else if (input$NAI=="before tax") {
    tax_coeff <- (1-input$tax_rate/100)
  } else {
    tax_coeff <- 1/(1-input$tax_rate/100)
  }
  
  rb$diff_NAI <- rb$NAI - NAI
  
  rb$diff_IOFC <- rb$IOFC - rv$IOFC * tax_coeff
  
  rb$diff_IOFC2 <- rb$IOFC2 - rv$IOFC2 * tax_coeff
  
  rb$diff_milk_feed <- rb$milk_feed - rv$milk_feed * tax_coeff
  
  rb$diff_labor_repair <- rb$labor_repair - rv$labor_repair * tax_coeff
  
  rb$diff_inflation <- rb$inflation - rv$inflation
  
  rb$diff_capital <- rb$capital - 
    + (-rv$capital_cost_total +(rb$NAI_spec=="after tax")*(tax_interest() + tax_depreciation()))
  
  rb$diff_misc <- rb$diff_NAI - (rb$diff_milk_feed + rb$diff_labor_repair + rb$diff_capital + rb$diff_inflation)
  
})






