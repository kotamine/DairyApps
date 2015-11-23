
## --- This file deals with most of the calculations for the base analysis 

## ----------- Main Calculations: all restuls are stored under reactive value "rv" -----------
observe({ 
  # rv$recalculate # respond to the change in rv$recalculate
  
  isolate(robot_parlor <- input$robot_parlor)
  if (robot_parlor=="OFF" | input$profile_choice=="Robots") {
    rv$cost_milking <- input$n_robot * input$cost_robot
    rv$cost_milking2 <-  rv$cost_milking*(1+input$inflation_robot/100)^input$robot_years
    rv$housing_years <- input$n_robot_life * input$robot_years
    rv$salvage_milking_fv1 <- input$salvage_milking1*(1+input$inflation_robot/100)^input$robot_years
    rv$salvage_milking_fv2 <- input$salvage_milking1*(1+input$inflation_robot/100)^(input$robot_years*2)*(input$n_robot_life>=2)
    rv$repair_total <- input$repair * input$n_robot
  } else {
    rv$cost_milking <- input$cost_parlors
    rv$cost_milking2 <- 0
    rv$housing_years <- input$milking_years
    rv$salvage_milking_fv1 <- input$salvage_milking1*(1+input$inflation_robot/100)^input$milking_years
    rv$salvage_milking_fv2 <- 0
    rv$repair_total <- input$repair 
  }
  
  
  # Data Entry Level Calculations
  rv$herd_size2 <- input$herd_size + input$herd_increase
  
  rv$cost_housing <- input$cost_housing_cow * rv$herd_size2
  
  rv$total_investment_cow <-  input$cost_housing_cow + rv$cost_milking/rv$herd_size2
  
  rv$total_investment <- rv$total_investment_cow  * rv$herd_size2
  
  rv$increased_insurance <- rv$total_investment
  
  rv$anticipated_hours_milking <- input$hours_milking - input$hr_sv_milking
  
  rv$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
    + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
  
  rv$milk_day_cow_alt <- input$milk_cow_day + input$milk_change
  
  rv$milk_lb_alt_day <- rv$milk_day_cow_alt * rv$herd_size2/input$n_robot 
  
  rv$adj_milk_cow_day2 <- rv$milk_day_cow_alt * input$milk_cow_coeff + 
    + rv$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 
  
  rv$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
  
  rv$DMI_day <-  rv$stage_lactation * (rv$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                         +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rv$DMI_projected <-  rv$stage_lactation * (rv$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                                               +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  
  rv$DMI_change <- rv$DMI_projected - rv$DMI_day
  
  rv$adj_milk_cow_day2 <- rv$milk_day_cow_alt * input$milk_cow_coeff + 
    + rv$milk_day_cow_alt  * input$milk_fat/100 * input$milk_fat_coeff 
  
  
  # Cash Flow items to render in Data Entry
  rv$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero
  
  rv$loan_housing <- rv$cost_housing - input$down_housing
  rv$loan_milking1 <- rv$cost_milking - input$down_milking1
  rv$loan_milking2 <- rv$cost_milking2 - input$down_milking2
  
  rv$yr_robot2 <- input$robot_years 
  rv$copy_salvage_milking1 <- input$salvage_milking1
  rv$copy_salvage_milking2 <- rv$salvage_milking_fv1
  rv$copy_cost_housing <- rv$cost_housing
  rv$copy_cost_milking1 <- rv$cost_milking
  rv$copy_cost_milking2 <- rv$cost_milking2
  
  
  # Positive Impacts (year 1)
  rv$inc_rev_herd_size <- rv$milk_day_cow_alt * 330 *
    (input$price_milk/100) * input$herd_increase
  
  rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * input$herd_size
  
  rv$inc_rev_milk_premium  <- rv$milk_day_cow_alt *330 * input$scc_premium/100*
    (input$scc_average*(-input$scc_change)/100)/1000 * rv$herd_size2
  
  rv$inc_rev_cull_sale   <- rv$herd_size2 * input$change_turnover/100 * input$cull_price
  
  rv$inc_rev_software  <- input$software * rv$herd_size2
  
  rv$inc_rev_total <- rv$inc_rev_herd_size + rv$inc_rev_per_cow + rv$inc_rev_milk_premium +
    + rv$inc_rev_cull_sale + rv$inc_rev_software
  
  rv$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
  
  rv$dec_exp_labor <- input$hr_sv_milking * input$labor_rate *365 
  
  rv$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
  
  rv$dec_exp_total <- rv$dec_exp_heat_detection  + rv$dec_exp_labor + rv$dec_exp_labor_management
  
  rv$positive_total <- rv$inc_rev_total +  rv$dec_exp_total
  
  
  # Negative Impacts (year 1)
  rv$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase
  
  
  rv$inc_exp_repair <-rv$repair_total + input$insurance_rate/100 * rv$increased_insurance
  
  
  rv$inc_exp_feed <-  rv$DMI_change * input$cost_DM * 330 * rv$herd_size2
  
  rv$inc_exp_pellet <- input$cost_pellets * 330 * rv$herd_size2 * input$pellets/2000
  
  rv$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rv$herd_size2
  
  rv$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rv$herd_size2
  
  rv$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
  
  rv$inc_exp_total <- rv$inc_exp_herd_increase + rv$inc_exp_repair + rv$inc_exp_feed + rv$inc_exp_pellet +
    + rv$inc_exp_replacement +  rv$inc_exp_utilities + rv$inc_exp_record_management 
  
  # if (is.na(input$n_robot_life) | is.na(input$interest) | 
  #     is.na(rv$housing_years) | is.na(rv$robot_invest) | is.na(input$inflation_robot) |
  #     is.na(input$robot_years)) {
  #   tmp <- NA
  # } else { 
  #   if (input$n_robot_life > 1) {
  #     tmp <-  - pmt(input$interest/100, rv$housing_years, 
  #                   rv$robot_invest*(1 + input$inflation_robot/100)^input$robot_years/
  #                     (1 + input$interest/100)^(input$robot_years))   
  #   } else {
  #     tmp <- 0
  #   }
  # }
  
  
  
  ## Deflator to convert nominal annuity into real annuity 
  #rv$deflator <- rv$housing_years/sum((1 + input$inflation_general/100)^seq_along(c(1:rv$housing_years))) 
  rv$deflator <- 1
  
  
  # rv$capital_recovery_robot <-  ( - pmt(input$interest/100, rv$housing_years, rv$robot_invest) + 
  #   - (input$n_robot_life > 1)* pmt(input$interest/100, rv$housing_years, 
  #                                    rv$robot_invest*(1 + input$inflation_robot/100)^input$robot_years/
  #                                      (1 + input$interest/100)^(input$robot_years))) * rv$deflator 
  # 
  # rv$capital_recovery_housing  <- - pmt(input$interest/100, rv$housing_years, rv$cost_housing) * rv$deflator 
  # 
  # # FIX THIS 
  # rv$robot_end_PV <- (pmt(input$interest/100, rv$housing_years, 
  #                          input$salvage_robot*(1 + input$inflation_robot/100)^input$robot_years/
  #                            (1 + input$interest/100)^input$robot_years) +
  #                       + pmt(input$interest/100, rv$housing_years, 
  #                             input$salvage_robot*(1 + input$inflation_robot/100)^rv$housing_years/
  #                               (1 + input$interest/100)^rv$housing_years))* rv$deflator  
  
  
  
  
  ## Net Impact (year 1)
  
  # rv$impact_without_salvage <- rv$positive_total - rv$negative_total
  # 
  # # rv$robot_end_PV <- -pmt(input$interest/100, rv$housing_years, 
  # #                         input$salvage_robot*(1 + input$inflation_robot/100)^input$robot_years/
  # #                           (1 + input$interest/100)^rv$housing_years) * rv$deflator 
  # 
  # 
  # rv$impact_with_salvage <- rv$impact_without_salvage + rv$robot_end_PV
  # 
  # rv$impact_with_inflation  <- "Depends on cash flow"
  
  # rv$cash_positive_total <- rv$positive_total
  # rv$cash_negative_total <- rv$negative_total
  # rv$cash_impact_without_salvage <-  rv$impact_without_salvage 
  # rv$cash_impact_with_salvage <-  rv$impact_with_salvage 
  
  source("session_cash_flow.R", local=TRUE)  # Calculates cash flow tables
  
  
  rv$capital_recovery_robot <-  -pmt(input$interest/100, rv$housing_years, 
                                     npv(input$interest/100, 
                                         rv$table_debt$robot_interest+rv$table_debt$robot_principal)) 
  
  rv$capital_recovery_housing  <- -pmt(input$interest/100, rv$housing_years, 
                                       npv(input$interest/100, 
                                           rv$table_debt$barn_interest+rv$table_debt$barn_principal))
  
  rv$robot_end_PV <-   pmt(input$interest/100, rv$housing_years,  # This will be shown as negative cost
                           npv(input$interest/100, 
                               rv$table_cash_flow$salvage[-1]))
  
  rv$cost_downpayment <-  pmt(input$hurdle_rate/100, rv$housing_years, 
                              npv(input$hurdle_rate/100, 
                                  rv$table_cash_flow$downpayment[-1])+rv$table_cash_flow$downpayment[1])
  
  rv$capital_cost_total <- rv$capital_recovery_robot + rv$capital_recovery_housing +
    + rv$cost_downpayment + rv$robot_end_PV
  
  
  ## ------------ Breakeven Calculations ------------
  # making it reactive to the following variables; 
  input$inflation_labor
  input$inflation_margin
  input$dep_method
  input$n_yr_milking1
  input$n_yr_milking2
  input$n_yr_housing
  
  isolate({
    
    n_years <- length(rv$table_cash_flow$depreciation) - 1  
    
    table_breakeven <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
    
    colnames(table_breakeven) <- c("year","increased_expense","capital_cost_minus_downpayment", "cost_downpayment",
                                   "increased_revenue","reduced_labor_management","reduced_heat_detection",
                                   "reduced_labor","cost_capital_WACC","tax_deduction")
    
    table_breakeven$increased_expense <- lapply(c(1:n_years), function(t) {
      rv$inc_exp_total*(1+input$inflation_margin/100)^(t-1)
    }) %>% unlist()
    
    table_breakeven$increased_revenue <- lapply(c(1:n_years), function(t) {
      rv$inc_rev_total*(1+input$inflation_margin/100)^(t-1)
    }) %>% unlist()
    
    table_breakeven$capital_cost_minus_downpayment <- rep((rv$capital_cost_total - rv$cost_downpayment),n_years)
    
    table_breakeven$cost_downpayment <- rep((rv$cost_downpayment),n_years)
    
    table_breakeven$reduced_labor_management <- lapply(c(1:n_years), function(t) {
      rv$dec_exp_labor_management *(1+input$inflation_labor/100)^(t-1)
    }) %>% unlist()
    
    table_breakeven$reduced_heat_detection <- lapply(c(1:n_years), function(t) {
      rv$dec_exp_heat_detection *(1+input$inflation_labor/100)^(t-1)
    }) %>% unlist()
    
    table_breakeven$reduced_labor <- lapply(c(1:n_years), function(t) {
      rv$dec_exp_labor *(1+input$inflation_labor/100)^(t-1)
    }) %>% unlist()
    
    if (rv$housing_years < n_years) {
      for (i in c("increased_expense","increased_revenue","reduced_labor_management",
                  "reduced_heat_detection","reduced_labor"))
        table_breakeven[[i]][(rv$housing_years+1):n_years] <- 0 
    }
    
    table_breakeven <- rbind(0, table_breakeven)
    
    table_breakeven$cost_capital_WACC <- -(rv$table_cash_flow$downpayment + rv$table_cash_flow$salvage +
                                             + rv$table_cash_flow$interest + rv$table_cash_flow$principal)
    
    table_breakeven$tax_deduction <- -input$tax_rate/100 * (rv$table_cash_flow$depreciation + rv$table_cash_flow$interest) 
    
    
    rv$npv_interest <- list()
    rv$annuity_interest <- list()
    rv$npv_WACC <- list()
    rv$annuity_WACC <- list()
    rv$npv_hurdle <- list()
    rv$annuity_hurdle <- list()
    
    cnames1 <- colnames(table_breakeven)
    lapply(cnames1, function(x) {
      
      rv$npv_interest[[paste0(x)]] <- npv(input$interest/100, table_breakeven[[paste0(x)]][-1]) +
        + table_breakeven[[paste0(x)]][1]
      
      rv$npv_WACC[[paste0(x)]] <- npv(WACC()/100, table_breakeven[[paste0(x)]][-1]) +
        + table_breakeven[[paste0(x)]][1]
      
      rv$annuity_interest[[paste0(x)]] <- -pmt(input$interest/100, n_years,rv$npv_interest[[paste0(x)]])
      rv$annuity_WACC[[paste0(x)]] <- -pmt(WACC()/100, n_years,rv$npv_WACC[[paste0(x)]])
    })
    
    rv$npv_hurdle[["cost_downpayment"]] <- npv(input$hurdle_rate/100, table_breakeven[["cost_downpayment"]][-1]) +
      + table_breakeven[["cost_downpayment"]][1]
    rv$annuity_hurdle[["cost_downpayment"]] <- -pmt(WACC()/100, n_years,rv$npv_hurdle[["cost_downpayment"]])
    
    
    rv$bw_wage_before_tax <- (rv$annuity_interest$increased_expense + rv$annuity_interest$capital_cost_minus_downpayment +
                                + rv$annuity_hurdle$cost_downpayment - rv$annuity_interest$increased_revenue + 
                                - rv$annuity_interest$reduced_labor_management)/
      ((rv$annuity_interest$reduced_heat_detection + rv$annuity_interest$reduced_labor)/input$labor_rate)
    
    rv$bw_wage_after_tax <-  ((rv$annuity_WACC$increased_expense - rv$annuity_WACC$increased_revenue + 
                                 - rv$annuity_WACC$reduced_labor_management)*(1-input$tax_rate/100)  + 
                                rv$annuity_WACC$cost_capital_WACC - rv$annuity_WACC$tax_deduction)/
      ((rv$annuity_WACC$reduced_heat_detection + rv$annuity_WACC$reduced_labor)*(1-input$tax_rate/100)/input$labor_rate)
    
    payment1 <- -rv$dec_exp_total/(1 + input$interest/100)
    payment2 <- -rv$dec_exp_total/(1 + WACC()/100)*(1-input$tax_rate/100)
    
    npv1 <- rv$npv_interest$increased_expense + rv$npv_interest$capital_cost_minus_downpayment +
      + rv$npv_hurdle$cost_downpayment - rv$npv_interest$increased_revenue + payment1
    
    npv2 <- (rv$npv_WACC$increased_expense  - rv$npv_WACC$increased_revenue) *(1-input$tax_rate/100) + 
      + rv$npv_WACC$cost_capital_WACC - rv$npv_WACC$tax_deduction + payment2 
    
    rv$bw_wage_inflation_before_tax <- (1 + input$interest/100)/(1 + rate(n_years-1, payment1, npv1)) - 1
    
    rv$bw_wage_inflation_after_tax <-  (1 + WACC()/100)/(1 + rate(n_years-1, payment2, npv2)) - 1
    
  })
  
  # This is used for alerting the base-value change in sensitivity and scenario analysis  
  createAlert(session, "c_input_change", "ref_c_input_change", 
              content = "New base values. 
            Press ``Calculate'' to updated the results.",
              append = FALSE) 
  
  createAlert(session, "s_input_change", "ref_s_input_change", 
              content = "New base values. 
            Press ``Calculate'' to updated the results.",
              append = FALSE) 
  
  createAlert(session, "c_toggle", "ref_c_toggle", 
              content = "Change sensitivity items to refresh the results.",
              append = FALSE) 
  
  createAlert(session, "s_toggle", "ref_s_toggle", 
              content = "Change scenarios to refresh the results.",
              append = FALSE)
  
  createAlert(session, "p_input_change", "ref_p_input_change", 
              content = "New base values. 
            Press ``Calculate'' to updated the results.",
              append = FALSE) 
}) 


# --- Partial Budget Analysis-Specific items:  They need to respond to input$budget_year ---
positive_total <- reactive({
  rv$inc_rev_total * (1+input$inflation_margin/100)^(input$budget_year-1) +
    + rv$dec_exp_total *  (1+input$inflation_labor/100)^(input$budget_year-1)
})

negative_total <- reactive({
  rv$inc_exp_total * (1+input$inflation_margin/100)^(input$budget_year-1) + rv$capital_cost_total 
}) 

inflation_adjustment <- reactive({ 
  - pmt(input$interest/100, rv$housing_years, npv(input$interest/100, rv$table_cash_flow$revenue_minus_expense[-1])) +
    - (positive_total() - negative_total() + rv$capital_cost_total)
}) 

positive_minus_negative <-  reactive({ 
  positive_total() - negative_total()
})

revenue_minus_expense <-  reactive({ 
  positive_total() -(negative_total()-rv$capital_cost_total) + inflation_adjustment()
})

net_annual_impact_before_tax <- reactive({
  positive_minus_negative() + inflation_adjustment()
})

tax_revenue_minus_expense <- reactive({
  -input$tax_rate/100 * revenue_minus_expense() 
}) 

tax_interest <- reactive({
  input$tax_rate/100 * pmt(input$interest/100, rv$housing_years, 
                           npv(input$interest/100, rv$table_cash_flow$interest[-1]))
})

tax_depreciation <- reactive({
  input$tax_rate/100 * pmt(input$interest/100, rv$housing_years, 
                           npv(input$interest/100, rv$table_cash_flow$depreciation[-1])) 
})


tax_deduction_robot <- reactive({  
  -input$tax_rate/100 *(pmt(input$interest/100, rv$housing_years, 
                            npv(input$interest/100, rv$table_depreciation$depreciation_robot))
                        +  pmt(input$interest/100, rv$housing_years, 
                               npv(input$interest/100, rv$table_debt$robot_interest)))
})  

tax_deduction_housing <- reactive({  
  -input$tax_rate/100 *(pmt(input$interest/100, rv$housing_years, 
                            npv(input$interest/100, rv$table_depreciation$depreciation_housing))
                        +  pmt(input$interest/100, rv$housing_years, 
                               npv(input$interest/100, rv$table_debt$barn_interest)))
})  

adj_WACC_interest <- reactive({
  depr <- -pmt(WACC()/100, rv$housing_years, 
               npv(WACC()/100, rv$table_cash_flow$depreciation[-1])) +
    + pmt(input$interest/100, rv$housing_years, 
          npv(input$interest/100, rv$table_cash_flow$depreciation[-1])) 
  
  salvage <- -pmt(WACC()/100, rv$housing_years, 
                  npv(WACC()/100, rv$table_cash_flow$salvage[-1])) +
    + pmt(input$interest/100, rv$housing_years, 
          npv(input$interest/100, rv$table_cash_flow$salvage[-1])) 
  
  interest <- -pmt(WACC()/100, rv$housing_years, 
                   npv(WACC()/100, rv$table_cash_flow$interest[-1])) +
    + pmt(input$interest/100, rv$housing_years, 
          npv(input$interest/100, rv$table_cash_flow$interest[-1]))
  
  principal <- -pmt(WACC()/100, rv$housing_years, 
                    npv(WACC()/100, rv$table_cash_flow$principal[-1])) +
    + pmt(input$interest/100, rv$housing_years, 
          npv(input$interest/100, rv$table_cash_flow$principal[-1])) 
  
  revenue_minus_expense <- -pmt(WACC()/100, rv$housing_years, 
                                npv(WACC()/100, rv$table_cash_flow$revenue_minus_expense[-1])) +
    + pmt(input$interest/100, rv$housing_years, 
          npv(input$interest/100, rv$table_cash_flow$revenue_minus_expense[-1])) 
  
  return( (revenue_minus_expense + interest)*(1-input$tax_rate/100) + 
            - depr*input$tax_rate/100 + principal + salvage )
})

adj_WACC_hurdle <- reactive({
  -pmt(WACC()/100, rv$housing_years, 
       npv(WACC()/100,  rv$table_cash_flow$downpayment[-1])+rv$table_cash_flow$downpayment[1]) +
    + pmt(input$hurdle_rate/100, rv$housing_years, 
          npv(input$hurdle_rate/100, rv$table_cash_flow$downpayment[-1])+rv$table_cash_flow$downpayment[1])
})

net_annual_impact_after_tax <- reactive({
  net_annual_impact_before_tax() + tax_revenue_minus_expense() + tax_interest() +
    + tax_depreciation() + adj_WACC_interest() + adj_WACC_hurdle()
})

be_wage_positive_minus_negative <- reactive({ 
  (negative_total() - rv$inc_rev_total - rv$dec_exp_labor_management)/ 
    ((rv$dec_exp_heat_detection + rv$dec_exp_labor )/input$labor_rate)
})


# This needs to respond to change in any data input or change in input$budget_year
observe({ 
  
  if (is.null(net_annual_impact_after_tax())) {  return() } 
  rv$positive_total <- positive_total()
  rv$negative_total <- negative_total()
  rv$positive_minus_negative <- positive_minus_negative()
  rv$inflation_adjustment <- inflation_adjustment()
  rv$revenue_minus_expense <- revenue_minus_expense()
  rv$net_annual_impact_before_tax <- net_annual_impact_before_tax()
  rv$tax_revenue_minus_expense <- tax_revenue_minus_expense()
  rv$tax_interest <- tax_interest()
  rv$tax_depreciation <- tax_depreciation()
  rv$net_annual_impact_after_tax <- net_annual_impact_after_tax()
  rv$adj_WACC_interest <- adj_WACC_interest()
  rv$adj_WACC_hurdle <- adj_WACC_hurdle()
  rv$be_wage_positive_minus_negative <- be_wage_positive_minus_negative() 
}) 


#  ------ Dashboard  triggered by any change in; -----------
#   input$NAI
# rv$inc_rev_total
# rv$dec_exp_total
# rv$inc_exp_total
# rv$capital_cost_total
observe({
  if (is.na(rv$inc_rev_total)) { return() }
  
  input$NAI
  rv$inc_rev_total
  rv$dec_exp_total
  rv$inc_exp_total
  rv$capital_cost_total
  
  
  isolate({
    
    rv$tax_factor <- (1-(input$NAI=="after tax")*input$tax_rate/100)
    
    if (input$NAI=="before tax") {
      rv$NAI <- net_annual_impact_before_tax()
    } else {
      rv$NAI <- net_annual_impact_after_tax()
    }
    
    rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - rv$DMI_day * input$cost_DM )*330 * rv$tax_factor
    
    rv$IOFC2 <- (rv$milk_day_cow_alt * input$price_milk/100 + 
                   - rv$DMI_projected * input$cost_DM - input$pellets * input$cost_pellets/2000)*330 *
      rv$tax_factor  
    
    rv$IOFC_cwt <- rv$IOFC /365 /input$milk_cow_day * 330 
    
    rv$IOFC2_cwt <- rv$IOFC2 /365 /rv$milk_day_cow_alt * 330 
    
    rv$milk_current <- 
      input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                      +  input$scc_premium/100 * input$scc_average/1000) *
      rv$tax_factor
    
    rv$milk_robot <- (rv$herd_size2 * 330 * rv$milk_day_cow_alt *
                        (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000)) *
      rv$tax_factor 
    
    rv$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365 *
      rv$tax_factor 
    
    rv$labor_robot <- ((input$anticipated_hours_heat + rv$anticipated_hours_milking) * input$labor_rate *365 + 
                         + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
                         + input$additional_labor * input$herd_increase) * rv$tax_factor 
    
    rv$feed_current <-  rv$DMI_day * input$cost_DM * 330 * input$herd_size * rv$tax_factor
    
    rv$feed_robot <- (rv$DMI_projected * input$cost_DM + input$pellets *
                        input$cost_pellets/2000) * 330 * rv$herd_size2 * rv$tax_factor 
    
    rv$milk_feed <-  (-(rv$feed_robot - rv$feed_current) + rv$milk_robot -  rv$milk_current )
    
    rv$labor_repair <- -(rv$labor_robot - rv$labor_current + rv$inc_exp_repair) 
    
    rv$inflation <- - pmt(input$interest/100, rv$housing_years, 
                          npv(input$interest/100, rv$table_cash_flow$revenue_minus_expense[-1])) +
      - (rv$inc_rev_total + rv$dec_exp_total - rv$inc_exp_total) 
    
    rv$capital <- -rv$capital_cost_total + 
      +(input$NAI=="after tax")*(tax_interest() + tax_depreciation())
    
    rv$misc <- rv$NAI - (rv$milk_feed + rv$labor_repair + rv$capital + rv$inflation) 
    
    rv$capital_recovery_robot2 <- rv$capital_recovery_robot +
      - (input$NAI=="after tax")*tax_deduction_robot()
    rv$capital_recovery_housing2 <- rv$capital_recovery_housing +
      - (input$NAI=="after tax")*tax_deduction_housing()
    
  })
})




