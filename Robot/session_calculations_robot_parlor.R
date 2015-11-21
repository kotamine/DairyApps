
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increase replaced by  rp$herd_increase
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


## --- This file deals with most of the calculations for the Robot vs Parlor analysis 
## ----------- Main Calculations adopted for obtaining results for reactive values rp -----------
isolate({ 
  
  # p is profile reference number passed through p=1("Barn Only"),2("Retrofit Parlors"),3(New Parlors"),4("Robots")
  
  if (p==4) {
    rp$cost_milking <- rp$n_robot * rp$cost_robot
    rp$cost_milking2 <-  rp$cost_milking*(1+input$inflation_robot/100)^rp$robot_years
    rp$housing_years <- input$n_robot_life * rp$robot_years
    rp$salvage_milking_fv1 <- rp$salvage_milking1*(1+input$inflation_robot/100)^rp$robot_years
    rp$salvage_milking_fv2 <- rp$salvage_milking1*(1+input$inflation_robot/100)^(rp$robot_years*2)*(input$n_robot_life>=2)
    rp$repair_total <- rp$repair * rp$n_robot
  } else {
    rp$cost_milking <- rp$cost_parlors
    rp$cost_milking2 <- 0
    rp$housing_years <- rp$milking_years
    rp$salvage_milking_fv1 <- rp$salvage_milking1*(1+input$inflation_robot/100)^rp$milking_years
    rp$salvage_milking_fv2 <- 0
    rp$repair_total <- rp$repair 
  }
  
  
  # Data Entry Level Calculations
  rp$herd_size2 <- input$herd_size + rp$herd_increase
  
  rp$cost_housing <- rp$cost_housing_cow * rp$herd_size2
  
  rp$total_investment_cow <-  rp$cost_housing_cow + rp$cost_milking/rp$herd_size2
  
  rp$total_investment <- rp$total_investment_cow  * rp$herd_size2
  
  rp$increased_insurance <- rp$total_investment
  
  rp$anticipated_hours_milking <- input$hours_milking - rp$hr_sv_milking
  
  rp$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
    + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
  
  rp$milk_day_cow_alt <- input$milk_cow_day + rp$milk_change
  
  rp$milk_lb_alt_day <- rp$milk_day_cow_alt * rp$herd_size2/rp$n_robot 
  
  rp$adj_milk_cow_day2 <- rp$milk_day_cow_alt * input$milk_cow_coeff + 
    + rp$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 
  
  rp$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
  
  rp$DMI_day <-  rp$stage_lactation * (rp$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                         +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rp$DMI_projected <-  rp$stage_lactation * (rp$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                                               +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rp$DMI_change <- rp$DMI_projected - rp$DMI_day
  
  rp$adj_milk_cow_day2 <- rp$milk_day_cow_alt * input$milk_cow_coeff + 
    + rp$milk_day_cow_alt  * input$milk_fat/100 * input$milk_fat_coeff 
  
  
  # Cash Flow items to render in Data Entry
  rp$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero
  
  rp$loan_housing <- rp$cost_housing - rp$down_housing
  rp$loan_milking1 <- rp$cost_milking - rp$down_milking1
  rp$loan_milking2 <- rp$cost_milking2 - rp$down_milking2
  
  rp$yr_robot2 <- rp$robot_years 
  rp$copy_salvage_milking1 <- rp$salvage_milking1
  rp$copy_salvage_milking2 <- rp$salvage_milking_fv1
  
  rp$copy_cost_housing <- rp$cost_housing
  
  rp$copy_cost_milking1 <- rp$cost_milking
  rp$copy_cost_milking2 <- rp$cost_milking2
  
  # save some rendering-outputs under rv$x
  if (p==1) {
    rv$copy_cost_housing_pr1 <- rp$copy_cost_housing
    rv$loan_housing_pr1 <- rp$loan_housing
    rv$copy_r_housing_pr1 <- input$interest
  } else if (p==2) {
    rv$housing_years_pr2 <- rp$housing_years
    rv$copy_cost_housing_pr2 <- rp$copy_cost_housing
    rv$copy_cost_milking1_pr2 <- rp$copy_cost_milking1
    rv$loan_housing_pr2 <- rp$loan_housing
    rv$loan_milking1_pr2 <- rp$loan_milking1
    rv$copy_r_housing_pr2 <- input$interest
    rv$copy_r_milking1_pr2 <- input$interest
  } else if (p==3) {
    rv$housing_years_pr3 <- rp$housing_years
    rv$copy_cost_housing_pr3 <- rp$copy_cost_housing
    rv$copy_cost_milking1_pr3 <- rp$copy_cost_milking1
    rv$loan_housing_pr3 <- rp$loan_housing
    rv$loan_milking1_pr3 <- rp$loan_milking1
    rv$copy_r_housing_pr3 <- input$interest
    rv$copy_r_milking1_pr3 <- input$interest
  } else {
    rv$housing_years_pr4 <- rp$housing_years
    rv$copy_cost_housing_pr4 <- rp$copy_cost_housing
    rv$copy_cost_milking1_pr4 <- rp$copy_cost_milking1
    rv$copy_cost_milking2_pr4 <- rp$copy_cost_milking2
    rv$loan_housing_pr4 <- rp$loan_housing
    rv$loan_milking1_pr4 <- rp$loan_milking1
    rv$loan_milking2_pr4 <- rp$loan_milking2
    rv$copy_r_housing_pr4 <- input$interest
    rv$copy_r_milking1_pr4 <- input$interest
    rv$copy_r_milking2_pr4 <- input$interest
    rv$copy_robot_years_pr4 <- rp$robot_years
    rv$copy_n_robot_pr4 <- rp$n_robot 
    rv$copy_cost_robot_pr4 <- rp$cost_robot*(1+input$inflation_robot/100)^rp$robot_years
    rv$salvage_milking2_pr4 <-  rp$salvage_milking_fv1
    rv$yr_invest_milking2_pr4 <- rp$robot_years
  }
  
  
  # Positive Impacts (year 1)
  rp$inc_rev_herd_size <- rp$milk_day_cow_alt * 330 *
    (input$price_milk/100) * rp$herd_increase
  
  rp$inc_rev_per_cow <- rp$milk_change * 330 * (input$price_milk/100) * input$herd_size
  
  rp$inc_rev_milk_premium  <- rp$milk_day_cow_alt *330 * input$scc_premium/100*
    (input$scc_average*(-rp$scc_change)/100)/1000 * rp$herd_size2
  
  rp$inc_rev_cull_sale   <- rp$herd_size2 * rp$change_turnover/100 * input$cull_price
  
  rp$inc_rev_software  <- rp$software * rp$herd_size2
  
  rp$inc_rev_total <- rp$inc_rev_herd_size + rp$inc_rev_per_cow + rp$inc_rev_milk_premium +
    + rp$inc_rev_cull_sale + rp$inc_rev_software
  
  rp$dec_exp_heat_detection <- (input$hr_heat_detection - rp$anticipated_hours_heat )*input$labor_rate *365
  
  rp$dec_exp_labor <- rp$hr_sv_milking * input$labor_rate *365 
  
  rp$dec_exp_labor_management <- rp$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
  
  rp$dec_exp_total <- rp$dec_exp_heat_detection  + rp$dec_exp_labor + rp$dec_exp_labor_management
  
  rp$positive_total <- rp$inc_rev_total +  rp$dec_exp_total
  
  
  # Negative Impacts (year 1)
  rp$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*rp$herd_increase
  
  
  rp$inc_exp_repair <-rp$repair_total + rp$insurance_rate/100 * rp$increased_insurance
  
  
  rp$inc_exp_feed <-  rp$DMI_change * input$cost_DM * 330 * rp$herd_size2
  
  rp$inc_exp_pellet <- rp$cost_pellets * 330 * rp$herd_size2 * rp$pellets/2000
  
  rp$inc_exp_replacement <- input$cost_heifer * rp$change_turnover/100 * rp$herd_size2
  
  rp$inc_exp_utilities <- (rp$change_electricity + rp$change_water + rp$change_chemical) * rp$herd_size2
  
  rp$inc_exp_record_management <- rp$increase_rc_mgt * input$labor_rate_rc_mgt * 365
  
  rp$inc_exp_total <- rp$inc_exp_herd_increase + rp$inc_exp_repair + rp$inc_exp_feed + rp$inc_exp_pellet +
    + rp$inc_exp_replacement +  rp$inc_exp_utilities + rp$inc_exp_record_management 
  
  
  rp$WACC <- ((rp$down_housing + rp$down_milking1) * input$hurdle_rate +
                + (rp$loan_housing * input$r_housing + rp$loan_milking1 * input$r_milking1)*
                (1-input$tax_rate/100))/(rp$cost_housing + rp$cost_milking)
  
  source("session_cash_flow_robot_parlor.R", local=TRUE)  # Calculates cash flow tables
  
  rp$capital_recovery_robot <-  -pmt(input$interest/100, rp$housing_years, 
                                     npv(input$interest/100, 
                                         rp$table_debt$robot_interest+rp$table_debt$robot_principal)) 
  
  rp$capital_recovery_housing  <- -pmt(input$interest/100, rp$housing_years, 
                                       npv(input$interest/100, 
                                           rp$table_debt$barn_interest+rp$table_debt$barn_principal))
  
  rp$robot_end_PV <-   pmt(input$interest/100, rp$housing_years,  # This will be shown as negative cost
                           npv(input$interest/100, 
                               rp$table_cash_flow$salvage[-1]))
  
  rp$cost_downpayment <-  pmt(input$hurdle_rate/100, rp$housing_years, 
                              npv(input$hurdle_rate/100, 
                                  rp$table_cash_flow$downpayment[-1])+rp$table_cash_flow$downpayment[1])
  
  rp$capital_cost_total <- rp$capital_recovery_robot + rp$capital_recovery_housing +
    + rp$cost_downpayment + rp$robot_end_PV
  
  
  ## ------------ Breakeven Calculations ------------
  
  n_years <- length(rp$table_cash_flow$depreciation) - 1  
  
  table_breakeven <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
  
  colnames(table_breakeven) <- c("year","increased_expense","capital_cost_minus_downpayment", "cost_downpayment",
                                 "increased_revenue","reduced_labor_management","reduced_heat_detection",
                                 "reduced_labor","cost_capital_WACC","tax_deduction")
  
  table_breakeven$increased_expense <- lapply(c(1:n_years), function(t) {
    rp$inc_exp_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$increased_revenue <- lapply(c(1:n_years), function(t) {
    rp$inc_rev_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$capital_cost_minus_downpayment <- rep((rp$capital_cost_total - rp$cost_downpayment),n_years)
  
  table_breakeven$cost_downpayment <- rep((rp$cost_downpayment),n_years)
  
  table_breakeven$reduced_labor_management <- lapply(c(1:n_years), function(t) {
    rp$dec_exp_labor_management *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$reduced_heat_detection <- lapply(c(1:n_years), function(t) {
    rp$dec_exp_heat_detection *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_breakeven$reduced_labor <- lapply(c(1:n_years), function(t) {
    rp$dec_exp_labor *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  if (rp$housing_years < n_years) {
    for (i in c("increased_expense","increased_revenue","reduced_labor_management",
                "reduced_heat_detection","reduced_labor"))
      table_breakeven[[i]][(rp$housing_years+1):n_years] <- 0 
  }
  
  table_breakeven <- rbind(0, table_breakeven)
  
  table_breakeven$cost_capital_WACC <- -(rp$table_cash_flow$downpayment + rp$table_cash_flow$salvage +
                                           + rp$table_cash_flow$interest + rp$table_cash_flow$principal)
  
  table_breakeven$tax_deduction <- -input$tax_rate/100 * (rp$table_cash_flow$depreciation + rp$table_cash_flow$interest) 
  
  rp$npv_interest <- list()
  rp$annuity_interest <- list()
  rp$npv_WACC <- list()
  rp$annuity_WACC <- list()
  rp$npv_hurdle <- list()
  rp$annuity_hurdle <- list()
  
  cnames1 <- colnames(table_breakeven)
  lapply(cnames1, function(x) {
    
    rp$npv_interest[[paste0(x)]] <- npv(input$interest/100, table_breakeven[[paste0(x)]][-1]) +
      + table_breakeven[[paste0(x)]][1]
    
    rp$npv_WACC[[paste0(x)]] <- npv(rp$WACC/100, table_breakeven[[paste0(x)]][-1]) +
      + table_breakeven[[paste0(x)]][1]
    
    rp$annuity_interest[[paste0(x)]] <- -pmt(input$interest/100, n_years,rp$npv_interest[[paste0(x)]])
    rp$annuity_WACC[[paste0(x)]] <- -pmt(rp$WACC/100, n_years,rp$npv_WACC[[paste0(x)]])
  })
  
  rp$npv_hurdle[["cost_downpayment"]] <- npv(input$hurdle_rate/100, table_breakeven[["cost_downpayment"]][-1]) +
    + table_breakeven[["cost_downpayment"]][1]
  rp$annuity_hurdle[["cost_downpayment"]] <- -pmt(rp$WACC/100, n_years,rp$npv_hurdle[["cost_downpayment"]])
  
  
  rp$bw_wage_before_tax <- (rp$annuity_interest$increased_expense + rp$annuity_interest$capital_cost_minus_downpayment +
                              + rp$annuity_hurdle$cost_downpayment - rp$annuity_interest$increased_revenue + 
                              - rp$annuity_interest$reduced_labor_management)/
    ((rp$annuity_interest$reduced_heat_detection + rp$annuity_interest$reduced_labor)/input$labor_rate)
  
  rp$bw_wage_after_tax <-  ((rp$annuity_WACC$increased_expense - rp$annuity_WACC$increased_revenue + 
                               - rp$annuity_WACC$reduced_labor_management)*(1-input$tax_rate/100)  + 
                              rp$annuity_WACC$cost_capital_WACC - rp$annuity_WACC$tax_deduction)/
    ((rp$annuity_WACC$reduced_heat_detection + rp$annuity_WACC$reduced_labor)*(1-input$tax_rate/100)/input$labor_rate)
  
  payment1 <- -rp$dec_exp_total/(1 + input$interest/100)
  payment2 <- -rp$dec_exp_total/(1 + rp$WACC/100)*(1-input$tax_rate/100)
  
  npv1 <- rp$npv_interest$increased_expense + rp$npv_interest$capital_cost_minus_downpayment +
    + rp$npv_hurdle$cost_downpayment - rp$npv_interest$increased_revenue + payment1
  
  npv2 <- (rp$npv_WACC$increased_expense  - rp$npv_WACC$increased_revenue) *(1-input$tax_rate/100) + 
    + rp$npv_WACC$cost_capital_WACC - rp$npv_WACC$tax_deduction + payment2 
  
  rp$bw_wage_inflation_before_tax <- (1 + input$interest/100)/(1 + rate(n_years-1, payment1, npv1)) - 1
  
  rp$bw_wage_inflation_after_tax <-  (1 + rp$WACC/100)/(1 + rate(n_years-1, payment2, npv2)) - 1
  
  
  # --- Partial Budget Analysis-Specific items:  They need to respond to input$budget_year ---
  rp$ positive_total <- 
    rp$inc_rev_total * (1+input$inflation_margin/100)^(input$budget_year-1) +
    + rp$dec_exp_total *  (1+input$inflation_labor/100)^(input$budget_year-1)
  
  rp$negative_total <- 
    rp$inc_exp_total * (1+input$inflation_margin/100)^(input$budget_year-1) + rp$capital_cost_total 
  
  rp$inflation_adjustment <-
    - pmt(input$interest/100, rp$housing_years, npv(input$interest/100, rp$table_cash_flow$revenue_minus_expense[-1])) +
    - (rp$positive_total - rp$negative_total + rp$capital_cost_total) 
  
  rp$positive_minus_negative <- 
    rp$positive_total - rp$negative_total 
  
  rp$revenue_minus_expense <-  
    rp$positive_total -(rp$negative_total-rp$capital_cost_total) + rp$inflation_adjustment  
  
  rp$net_annual_impact_before_tax <-  
    rp$positive_minus_negative + rp$inflation_adjustment  
  
  rp$tax_revenue_minus_expense <-
    -input$tax_rate/100 * rp$revenue_minus_expense   
  
  
  rp$tax_interest <-
    input$tax_rate/100 * pmt(input$interest/100, rp$housing_years, 
                             npv(input$interest/100, rp$table_cash_flow$interest[-1]))   
  
  rp$tax_depreciation <- 
    input$tax_rate/100 * pmt(input$interest/100, rp$housing_years, 
                             npv(input$interest/100, rp$table_cash_flow$depreciation[-1]))   
  
  rp$tax_deduction_robot <-
    -input$tax_rate/100 *(pmt(input$interest/100, rp$housing_years, 
                              npv(input$interest/100, rp$table_depreciation$depreciation_robot))
                          +  pmt(input$interest/100, rp$housing_years, 
                                 npv(input$interest/100, rp$table_debt$robot_interest))) 
  
  rp$tax_deduction_housing <-  
    -input$tax_rate/100 *(pmt(input$interest/100, rp$housing_years, 
                              npv(input$interest/100, rp$table_depreciation$depreciation_housing))
                          +  pmt(input$interest/100, rp$housing_years, 
                                 npv(input$interest/100, rp$table_debt$barn_interest))) 
  
  depr <- -pmt(rp$WACC/100, rp$housing_years, 
               npv(rp$WACC/100, rp$table_cash_flow$depreciation[-1])) +
    + pmt(input$interest/100, rp$housing_years, 
          npv(input$interest/100, rp$table_cash_flow$depreciation[-1])) 
  
  salvage <- -pmt(rp$WACC/100, rp$housing_years, 
                  npv(rp$WACC/100, rp$table_cash_flow$salvage[-1])) +
    + pmt(input$interest/100, rp$housing_years, 
          npv(input$interest/100, rp$table_cash_flow$salvage[-1])) 
  
  interest <- -pmt(rp$WACC/100, rp$housing_years, 
                   npv(rp$WACC/100, rp$table_cash_flow$interest[-1])) +
    + pmt(input$interest/100, rp$housing_years, 
          npv(input$interest/100, rp$table_cash_flow$interest[-1]))
  
  principal <- -pmt(rp$WACC/100, rp$housing_years, 
                    npv(rp$WACC/100, rp$table_cash_flow$principal[-1])) +
    + pmt(input$interest/100, rp$housing_years, 
          npv(input$interest/100, rp$table_cash_flow$principal[-1])) 
  
  revenue_minus_expense <- -pmt(rp$WACC/100, rp$housing_years, 
                                npv(rp$WACC/100, rp$table_cash_flow$revenue_minus_expense[-1])) +
    + pmt(input$interest/100, rp$housing_years, 
          npv(input$interest/100, rp$table_cash_flow$revenue_minus_expense[-1])) 
  
  rp$adj_WACC_interest <- (revenue_minus_expense + interest)*(1-input$tax_rate/100) + 
    - depr*input$tax_rate/100 + principal + salvage
  
  rp$adj_WACC_hurdle <-  -pmt(rp$WACC/100, rp$housing_years, 
                              npv(rp$WACC/100,  rp$table_cash_flow$downpayment[-1])+rp$table_cash_flow$downpayment[1]) +
    + pmt(input$hurdle_rate/100, rp$housing_years, 
          npv(input$hurdle_rate/100, rp$table_cash_flow$downpayment[-1])+rp$table_cash_flow$downpayment[1])
  
  
  rp$net_annual_impact_after_tax <-
    rp$net_annual_impact_before_tax + rp$tax_revenue_minus_expense + rp$tax_interest +
    + rp$tax_depreciation + rp$adj_WACC_interest + rp$adj_WACC_hurdle 
  
  rp$be_wage_positive_minus_negative <-
    (rp$negative_total - rp$inc_rev_total - rp$dec_exp_labor_management)/ 
    ((rp$dec_exp_heat_detection + rp$dec_exp_labor )/input$labor_rate)
  
})  



