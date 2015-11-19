
## --- For robustoness analysis we will calculate almost everything  all over again ---
## - Technically we don't need to store all varibles under "rb" 
## - but it makes it easier to retrieve them later. 

isolate({ # isolate the robustness calculation  
  
 # Initialize changed values 
  rb$cost_robot <- input$cost_robot
  rb$cost_housing_cow <- input$cost_housing_cow
  rb$repair <- input$repair
  rb$robot_years <- input$robot_years
  rb$salvage_milking1 <- input$salvage_milking1
  rb$hr_sv_milking <- input$hr_sv_milking
  rb$milk_change <- input$milk_change
  rb$scc_change <- input$scc_change
  rb$pellets <- input$pellets 
  rb$cost_parlors <-  input$cost_parlors
  rb$milking_years <- input$milking_years
  
  
  if (robust=="Sensitivity") {
    
    # Change the selected value by c_val (value)
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
    rb$cost_robot <- input$cost_robot*(1+ (c_choice=="c1")*c_val/100)
    rb$robot_years <- rb$robot_years*(1+ (c_choice=="c4")*c_val/100)
    } else {
    rb$cost_parlors <-  rb$cost_parlors*(1+ (c_choice=="c1")*c_val/100)
    rb$milking_years <- rb$milking_years*(1+ (c_choice=="c4")*c_val/100)
    }
    rb$cost_housing_cow <- input$cost_housing_cow*(1+ (c_choice=="c2")*c_val/100)
    rb$repair <- rb$repair*(1+ (c_choice=="c3")*c_val/100)
    rb$salvage_milking1 <- rb$salvage_milking1*(1+ (c_choice=="c5")*c_val/100)
    rb$hr_sv_milking <- rb$hr_sv_milking*(1+ (c_choice=="c6")*c_val/100)
    rb$milk_change <- rb$milk_change*(1+ (c_choice=="c7")*c_val/100)
    
  }
  
  if (robust=="Scenarios") {
    
    # Change the selected value by s_val (array)
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
      rb$cost_robot <- new_val[1]
    } else {
      rb$cost_parlors <- new_val[1]
    }
    rb$cost_housing_cow <- new_val[2]
    rb$milk_change <- new_val[3]
    rb$scc_change < - new_val[4]
    rb$pellets <- new_val[5] 
    
  }
  
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
    rb$cost_milking <- input$n_robot * rb$cost_robot
    rb$cost_milking2 <- rb$cost_milking*(1+input$inflation_robot/100)^rb$robot_years
    rb$housing_years <- input$n_robot_life * rb$robot_years
    rb$salvage_milking_fv1 <- rb$salvage_milking1*(1+input$inflation_robot/100)^rb$robot_years
    rb$salvage_milking_fv2 <- rb$salvage_milking1*(1+input$inflation_robot/100)^(rb$robot_years*2)*(input$n_robot_life>=2)
    rb$repair_total <- rb$repair * input$n_robot
  } else {
    rb$cost_milking <- rb$cost_parlors
    rb$cost_milking2 <- 0
    rb$housing_years <- rb$milking_years
    rb$salvage_milking_fv1 <- rb$salvage_milking1*(1+input$inflation_robot/100)^rb$milking_years
    rb$salvage_milking_fv2 <- 0
    rb$repair_total <- rb$repair 
  }
  
  

# Data Entry Level Calculations
  rb$herd_size2 <- input$herd_size + rb$herd_increase
  
  rb$cost_housing <- rb$cost_housing_cow  * rb$herd_size2
  
  rb$total_investment_cow <-  rb$cost_housing_cow  + rb$cost_milking/rb$herd_size2
  
  rb$total_investment <- rb$total_investment_cow  * rb$herd_size2
  
  rb$increased_insurance <- rb$total_investment
  
  rb$anticipated_hours_milking <- input$hours_milking - rb$hr_sv_milking
  
  rb$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
    + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
  
  rb$milk_day_cow_alt <- input$milk_cow_day + rb$milk_change
  
  rb$milk_lb_alt_day <- rb$milk_day_cow_alt * rb$herd_size2/input$n_robot 
  
  rb$adj_milk_cow_day2 <- rb$milk_day_cow_alt * input$milk_cow_coeff + 
    + rb$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 
  
  rb$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
  
  rb$DMI_day <-  rb$stage_lactation * (rb$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                         +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rb$DMI_projected <-  rb$stage_lactation * (rb$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                                               +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rb$DMI_change <- rb$DMI_projected - rb$DMI_day
  
  rb$adj_milk_cow_day2 <- rb$milk_day_cow_alt * input$milk_cow_coeff + 
    + rb$milk_day_cow_alt  * input$milk_fat/100 * input$milk_fat_coeff 
  
  
  # Cash Flow items to render in Data Entry
  rb$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero
  
  rb$loan_housing <- rb$cost_housing - rb$down_housing
  rb$loan_milking1 <- rb$cost_milking - rb$down_milking1
  rb$loan_milking2 <- rb$cost_milking2 - rb$down_milking2
  
  rb$yr_robot2 <- rb$robot_years 
  rb$copy_salvage_milking1 <- rb$salvage_milking1
  rb$copy_salvage_milking2 <- rb$salvage_milking_fv1
  
  rb$copy_cost_housing <- rb$cost_housing
  
  rb$copy_cost_milking1 <- rb$cost_milking
  rb$copy_cost_milking2 <- rb$cost_milking2
  

  # Positive Impacts (year 1)
  rb$inc_rev_herd_size <- rb$milk_day_cow_alt * 330 *
    (input$price_milk/100) * rb$herd_increase
  
  rb$inc_rev_per_cow <- rb$milk_change * 330 * (input$price_milk/100) * input$herd_size
  
  rb$inc_rev_milk_premium  <- rb$milk_day_cow_alt *330 * input$scc_premium/100*
    (input$scc_average*(-rb$scc_change)/100)/1000 * rb$herd_size2
  
  rb$inc_rev_cull_sale   <- rb$herd_size2 * rb$change_turnover/100 * input$cull_price
  
  rb$inc_rev_software  <- rb$software * rb$herd_size2
  
  rb$inc_rev_total <- rb$inc_rev_herd_size + rb$inc_rev_per_cow + rb$inc_rev_milk_premium +
    + rb$inc_rev_cull_sale + rb$inc_rev_software
  
  rb$dec_exp_heat_detection <- (input$hr_heat_detection - rb$anticipated_hours_heat )*input$labor_rate *365
  
  rb$dec_exp_labor <- rb$hr_sv_milking * input$labor_rate *365 
  
  rb$dec_exp_labor_management <- rb$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
  
  rb$dec_exp_total <- rb$dec_exp_heat_detection  + rb$dec_exp_labor + rb$dec_exp_labor_management
  
  rb$positive_total <- rb$inc_rev_total +  rb$dec_exp_total
  
  
  # Negative Impacts (year 1)
  rb$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*rb$herd_increase
  
  
  rb$inc_exp_repair <-rb$repair_total + rb$insurance_rate/100 * rb$increased_insurance
  
  
  rb$inc_exp_feed <-  rb$DMI_change * input$cost_DM * 330 * rb$herd_size2
  
  rb$inc_exp_pellet <- rb$cost_pellets * 330 * rb$herd_size2 * rb$pellets/2000
  
  rb$inc_exp_replacement <- input$cost_heifer * rb$change_turnover/100 * rb$herd_size2
  
  rb$inc_exp_utilities <- (rb$change_electricity + rb$change_water + rb$change_chemical) * rb$herd_size2
  
  rb$inc_exp_record_management <- rb$increase_rc_mgt * input$labor_rate_rc_mgt * 365
  
  rb$inc_exp_total <- rb$inc_exp_herd_increase + rb$inc_exp_repair + rb$inc_exp_feed + rb$inc_exp_pellet +
    + rb$inc_exp_replacement +  rb$inc_exp_utilities + rb$inc_exp_record_management 
  
  
  rb$WACC <- ((rb$down_housing + rb$down_milking1) * input$hurdle_rate +
                + (rb$loan_housing * input$r_housing + rb$loan_milking1 * input$r_milking1)*
                (1-input$tax_rate/100))/(rb$cost_housing + rb$cost_milking)
  
  source("session_cash_flow_robustness.R", local=TRUE)  # Calculates cash flow tables
  
  rb$capital_recovery_robot <-  -pmt(input$interest/100, rb$housing_years, 
                                     npv(input$interest/100, 
                                         rb$table_debt$robot_interest+rb$table_debt$robot_principal)) 
  
  rb$capital_recovery_housing  <- -pmt(input$interest/100, rb$housing_years, 
                                       npv(input$interest/100, 
                                           rb$table_debt$barn_interest+rb$table_debt$barn_principal))
  
  rb$robot_end_PV <-   pmt(input$interest/100, rb$housing_years,  # This will be shown as negative cost
                           npv(input$interest/100, 
                               rb$table_cash_flow$salvage[-1]))
  
  rb$cost_downpayment <-  pmt(input$hurdle_rate/100, rb$housing_years, 
                              npv(input$hurdle_rate/100, 
                                  rb$table_cash_flow$downpayment[-1])+rb$table_cash_flow$downpayment[1])
  
  rb$capital_cost_total <- rb$capital_recovery_robot + rb$capital_recovery_housing +
    + rb$cost_downpayment + rb$robot_end_PV
  
  
  ## ------------ Breakeven Calculations ------------
  
  n_years <- rb$housing_years
  
  table_breakeven <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
  
  colnames(table_breakeven) <- c("year","increased_expense","capital_cost_minus_downpayment", "cost_downpayment",
                                 "increased_revenue","reduced_labor_management","reduced_heat_detection",
                                 "reduced_labor","cost_capital_WACC","tax_deduction")
  
  table_breakeven$increased_expense <- lapply(c(1:n_years), function(t) {
    rb$inc_exp_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$increased_revenue <- lapply(c(1:n_years), function(t) {
    rb$inc_rev_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$capital_cost_minus_downpayment <- rep((rb$capital_cost_total - rb$cost_downpayment),n_years)
  
  table_breakeven$cost_downpayment <- rep((rb$cost_downpayment),n_years)
  
  table_breakeven$reduced_labor_management <- lapply(c(1:n_years), function(t) {
    rb$dec_exp_labor_management *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$reduced_heat_detection <- lapply(c(1:n_years), function(t) {
    rb$dec_exp_heat_detection *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$reduced_labor <- lapply(c(1:n_years), function(t) {
    rb$dec_exp_labor *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven <- rbind(0, table_breakeven)
  
  table_breakeven$cost_capital_WACC <- -(rb$table_cash_flow$downpayment + rb$table_cash_flow$salvage +
                                           + rb$table_cash_flow$interest + rb$table_cash_flow$principal)
  
  table_breakeven$tax_deduction <- -input$tax_rate/100 * (rb$table_cash_flow$depreciation + rb$table_cash_flow$interest) 
  
  rb$npv_interest <- list()
  rb$annuity_interest <- list()
  rb$npv_WACC <- list()
  rb$annuity_WACC <- list()
  rb$npv_hurdle <- list()
  rb$annuity_hurdle <- list()
  
  cnames1 <- colnames(table_breakeven)
  lapply(cnames1, function(x) {
    
    rb$npv_interest[[paste0(x)]] <- npv(input$interest/100, table_breakeven[[paste0(x)]][-1]) +
      + table_breakeven[[paste0(x)]][1]
    
    rb$npv_WACC[[paste0(x)]] <- npv(rb$WACC/100, table_breakeven[[paste0(x)]][-1]) +
      + table_breakeven[[paste0(x)]][1]
    
    rb$annuity_interest[[paste0(x)]] <- -pmt(input$interest/100, n_years,rb$npv_interest[[paste0(x)]])
    rb$annuity_WACC[[paste0(x)]] <- -pmt(rb$WACC/100, n_years,rb$npv_WACC[[paste0(x)]])
  })
  
  rb$npv_hurdle[["cost_downpayment"]] <- npv(input$hurdle_rate/100, table_breakeven[["cost_downpayment"]][-1]) +
    + table_breakeven[["cost_downpayment"]][1]
  rb$annuity_hurdle[["cost_downpayment"]] <- -pmt(rb$WACC/100, n_years,rb$npv_hurdle[["cost_downpayment"]])
  
  
  rb$bw_wage_before_tax <- (rb$annuity_interest$increased_expense + rb$annuity_interest$capital_cost_minus_downpayment +
                              + rb$annuity_hurdle$cost_downpayment - rb$annuity_interest$increased_revenue + 
                              - rb$annuity_interest$reduced_labor_management)/
    ((rb$annuity_interest$reduced_heat_detection + rb$annuity_interest$reduced_labor)/input$labor_rate)
  
  rb$bw_wage_after_tax <-  ((rb$annuity_WACC$increased_expense - rb$annuity_WACC$increased_revenue + 
                               - rb$annuity_WACC$reduced_labor_management)*(1-input$tax_rate/100)  + 
                              rb$annuity_WACC$cost_capital_WACC - rb$annuity_WACC$tax_deduction)/
    ((rb$annuity_WACC$reduced_heat_detection + rb$annuity_WACC$reduced_labor)*(1-input$tax_rate/100)/input$labor_rate)
  
  payment1 <- -rb$dec_exp_total/(1 + input$interest/100)
  payment2 <- -rb$dec_exp_total/(1 + rb$WACC/100)*(1-input$tax_rate/100)
  
  npv1 <- rb$npv_interest$increased_expense + rb$npv_interest$capital_cost_minus_downpayment +
    + rb$npv_hurdle$cost_downpayment - rb$npv_interest$increased_revenue + payment1
  
  npv2 <- (rb$npv_WACC$increased_expense  - rb$npv_WACC$increased_revenue) *(1-input$tax_rate/100) + 
    + rb$npv_WACC$cost_capital_WACC - rb$npv_WACC$tax_deduction + payment2 
  
  rb$bw_wage_inflation_before_tax <- (1 + input$interest/100)/(1 + rate(n_years-1, payment1, npv1)) - 1
  
  rb$bw_wage_inflation_after_tax <-  (1 + rb$WACC/100)/(1 + rate(n_years-1, payment2, npv2)) - 1
  
  
  # --- Partial Budget Analysis-Specific items:  They need to respond to input$budget_year ---
  rb$ positive_total <- 
    rb$inc_rev_total * (1+input$inflation_margin/100)^(input$budget_year-1) +
    + rb$dec_exp_total *  (1+input$inflation_labor/100)^(input$budget_year-1)
  
  rb$negative_total <- 
    rb$inc_exp_total * (1+input$inflation_margin/100)^(input$budget_year-1) + rb$capital_cost_total 
  
  rb$inflation_adjustment <-
    - pmt(input$interest/100, rb$housing_years, npv(input$interest/100, rb$table_cash_flow$revenue_minus_expense[-1])) +
    - (rb$positive_total - rb$negative_total + rb$capital_cost_total) 
  
  rb$positive_minus_negative <- 
    rb$positive_total - rb$negative_total 
  
  rb$revenue_minus_expense <-  
    rb$positive_total -(rb$negative_total-rb$capital_cost_total) + rb$inflation_adjustment  
  
  rb$net_annual_impact_before_tax <-  
    rb$positive_minus_negative + rb$inflation_adjustment  
  
  rb$tax_revenue_minus_expense <-
    -input$tax_rate/100 * rb$revenue_minus_expense   
  
  
  rb$tax_interest <-
    input$tax_rate/100 * pmt(input$interest/100, rb$housing_years, 
                             npv(input$interest/100, rb$table_cash_flow$interest[-1]))   
  
  rb$tax_depreciation <- 
    input$tax_rate/100 * pmt(input$interest/100, rb$housing_years, 
                             npv(input$interest/100, rb$table_cash_flow$depreciation[-1]))   
  
  rb$tax_deduction_robot <-
    -input$tax_rate/100 *(pmt(input$interest/100, rb$housing_years, 
                              npv(input$interest/100, rb$table_depreciation$depreciation_robot))
                          +  pmt(input$interest/100, rb$housing_years, 
                                 npv(input$interest/100, rb$table_debt$robot_interest))) 
  
  rb$tax_deduction_housing <-  
    -input$tax_rate/100 *(pmt(input$interest/100, rb$housing_years, 
                              npv(input$interest/100, rb$table_depreciation$depreciation_housing))
                          +  pmt(input$interest/100, rb$housing_years, 
                                 npv(input$interest/100, rb$table_debt$barn_interest))) 
  
  depr <- -pmt(rb$WACC/100, rb$housing_years, 
               npv(rb$WACC/100, rb$table_cash_flow$depreciation[-1])) +
    + pmt(input$interest/100, rb$housing_years, 
          npv(input$interest/100, rb$table_cash_flow$depreciation[-1])) 
  
  salvage <- -pmt(rb$WACC/100, rb$housing_years, 
                  npv(rb$WACC/100, rb$table_cash_flow$salvage[-1])) +
    + pmt(input$interest/100, rb$housing_years, 
          npv(input$interest/100, rb$table_cash_flow$salvage[-1])) 
  
  interest <- -pmt(rb$WACC/100, rb$housing_years, 
                   npv(rb$WACC/100, rb$table_cash_flow$interest[-1])) +
    + pmt(input$interest/100, rb$housing_years, 
          npv(input$interest/100, rb$table_cash_flow$interest[-1]))
  
  principal <- -pmt(rb$WACC/100, rb$housing_years, 
                    npv(rb$WACC/100, rb$table_cash_flow$principal[-1])) +
    + pmt(input$interest/100, rb$housing_years, 
          npv(input$interest/100, rb$table_cash_flow$principal[-1])) 
  
  revenue_minus_expense <- -pmt(rb$WACC/100, rb$housing_years, 
                                npv(rb$WACC/100, rb$table_cash_flow$revenue_minus_expense[-1])) +
    + pmt(input$interest/100, rb$housing_years, 
          npv(input$interest/100, rb$table_cash_flow$revenue_minus_expense[-1])) 
  
  rb$adj_WACC_interest <- (revenue_minus_expense + interest)*(1-input$tax_rate/100) + 
    - depr*input$tax_rate/100 + principal + salvage
  
  rb$adj_WACC_hurdle <-  -pmt(rb$WACC/100, rb$housing_years, 
                              npv(rb$WACC/100,  rb$table_cash_flow$downpayment[-1])+rb$table_cash_flow$downpayment[1]) +
    + pmt(input$hurdle_rate/100, rb$housing_years, 
          npv(input$hurdle_rate/100, rb$table_cash_flow$downpayment[-1])+rb$table_cash_flow$downpayment[1])
  
  
  rb$net_annual_impact_after_tax <-
    rb$net_annual_impact_before_tax + rb$tax_revenue_minus_expense + rb$tax_interest +
    + rb$tax_depreciation + rb$adj_WACC_interest + rb$adj_WACC_hurdle 
  
  rb$be_wage_positive_minus_negative <-
    (rb$negative_total - rb$inc_rev_total - rb$dec_exp_labor_management)/ 
    ((rb$dec_exp_heat_detection + rb$dec_exp_labor )/input$labor_rate)
  
  

  
if (robust=="Sensitivity") {
  
# --- add a row of results to the table_sensitivity ---
new_row <- c(c_val, base_val, new_val,  
                  rb$impact_without_housing, rb$impact_without_housing - rv$impact_without_housing, 
                  rb$impact_with_housing, rb$impact_with_housing - rv$impact_with_housing,
                  rb$impact_with_robot_salvage, rb$impact_with_robot_salvage - rv$impact_with_robot_salvage,
                  rb$IOFC2 - rb$IOFC,  rb$IOFC2-rb$IOFC - (rv$IOFC2 - rv$IOFC),
                  rb$IOFC2_cwt - rb$IOFC_cwt,  
                  rb$IOFC2_cwt - rb$IOFC_cwt - (rv$IOFC2_cwt - rv$IOFC_cwt),           
                  rb$milk_feed, rb$milk_feed - rv$milk_feed, 
                  rb$labor_repair, rb$labor_repair - rv$labor_repair, 
                  rb$capital_cost, rb$capital_cost - rv$capital_cost, 
                  rb$misc, rb$misc - rv$misc)

new_row <- matrix(c(label,round(new_row)),nrow=1)

colnames(new_row) <- c_colnames

}


if (robust=="Scenarios") {

  # --- add a row of results to the table_scenario ---
  new_row <- c(s_val, new_val,  
               rb$impact_without_housing, rb$impact_without_housing - rv$impact_without_housing, 
               rb$impact_with_housing, rb$impact_with_housing - rv$impact_with_housing,
               rb$impact_with_robot_salvage, rb$impact_with_robot_salvage - rv$impact_with_robot_salvage,
               rb$IOFC2 - rb$IOFC,  rb$IOFC2-rb$IOFC - (rv$IOFC2 - rv$IOFC),
               rb$IOFC2_cwt - rb$IOFC_cwt,  
               rb$IOFC2_cwt - rb$IOFC_cwt - (rv$IOFC2_cwt - rv$IOFC_cwt),           
               rb$milk_feed, rb$milk_feed - rv$milk_feed, 
               rb$labor_repair, rb$labor_repair - rv$labor_repair, 
               rb$capital_cost, rb$capital_cost - rv$capital_cost, 
               rb$misc, rb$misc - rv$misc)
  
  new_row <- matrix(c(label,round(new_row)), nrow=1)
  
  colnames(new_row) <- s_colnames
  
}
  

})





