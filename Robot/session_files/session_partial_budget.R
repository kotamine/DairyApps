
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increaseRobots, input$herd_increaseRetrofit, input$herd_increaseNew
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "cost_housing_cow",
#     "down_housing", "down_milking1", "down_milking2",
#     "salvage_housing", "salvage_milking1", 
#     "planning_horizon", "cost_parlors", "cost_robot", "useful_years", "n_robot")
# 


lapply(base_profiles, function(x) {
  
  
# This needs to respond to change in any data input or change in input[[paste0("budget_year",x)]]
observe({ 
  
  if (is.null(ans[[x]]$positive_total)) {  return() } 
  ans[[x]]$pb_positive_total <- ans[[x]]$inc_rev_total * (1+input$inflation_margin/100)^(input[[paste0("budget_year",x)]]-1) +
    + ans[[x]]$dec_exp_total *  (1+input$inflation_labor/100)^(input[[paste0("budget_year",x)]]-1)
  
  ans[[x]]$pb_negative_total <- ans[[x]]$inc_exp_total * (1+input$inflation_margin/100)^(input[[paste0("budget_year",x)]]-1) +
    + ans[[x]]$capital_cost_total
  
  ans[[x]]$pb_positive_minus_negative <-  ans[[x]]$pb_positive_total - ans[[x]]$pb_negative_total 
 
  ans[[x]]$pb_inflation_adjustment <-  - pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
    - (ans[[x]]$pb_positive_total - ans[[x]]$pb_negative_total + ans[[x]]$capital_cost_total)
  
  ans[[x]]$pb_net_annual_impact_before_tax <- ans[[x]]$pb_positive_minus_negative + ans[[x]]$pb_inflation_adjustment
  
})
  


observe({ 

  ans[[x]]$inc_rev_total
  ans[[x]]$dec_exp_total
  ans[[x]]$inc_exp_total
  ans[[x]]$capital_cost_total
  
  isolate({
  # if ( is.null(ans[[x]]$planning_horizon) ) {  return() } 

  ans[[x]]$tax_revenue_minus_expense <-  input$tax_rate/100 * 
         pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
           npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) 
  
  # cost of cash flows for interest payments (evaluated at separate instests for milking and housing)
  ans[[x]]$interest_at_interest <-  pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon, 
                                         npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_debt$milking_interest)) +
                                       + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon, 
                                             npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_debt$barn_interest))
  
  ans[[x]]$tax_interest <-  -input$tax_rate/100 * ans[[x]]$interest_at_interest 
    
                         
  ans[[x]]$principal_at_interest <-  pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon, 
                                           npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_debt$milking_principal)) +
                                         + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon, 
                                               npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_debt$barn_principal))
    
  
    # cost of depreciation (evaluated at separate instests for milking and housing)
    ans[[x]]$depreciation_at_interest <-   
      (pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon, 
              npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_depreciation$depreciation_milking_system)) +
            + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon, 
                       npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_depreciation$depreciation_housing)))
   
     ans[[x]]$tax_depreciation <- -input$tax_rate/100 * ans[[x]]$depreciation_at_interest
      
  
  
  # axillary functions for  ans[[x]]$adj_WACC_interest
  ans[[x]]$adj_depr <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
               npv(ans[[x]]$WACC/100, ans[[x]]$table_cash_flow$depreciation[-1])) +
    - ans[[x]]$depreciation_at_interest
  
  ans[[x]]$adj_salvage <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
                  npv(ans[[x]]$WACC/100, ans[[x]]$table_cash_flow$salvage[-1])) +
    + pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon, 
          npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_cash_flow$salvage[-1])) 
  
  ans[[x]]$adj_interest <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
                   npv(ans[[x]]$WACC/100, ans[[x]]$table_cash_flow$interest[-1])) +
    - ans[[x]]$interest_at_interest
  
  ans[[x]]$adj_principal <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
                    npv(ans[[x]]$WACC/100, ans[[x]]$table_cash_flow$principal[-1])) +
    - ans[[x]]$principal_at_interest
  
  ans[[x]]$adj_revenue_minus_expense <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
                                npv(ans[[x]]$WACC/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
    + pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
          npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) 

  ans[[x]]$adj_WACC_interest <- (ans[[x]]$adj_revenue_minus_expense + ans[[x]]$adj_interest)*(1-input$tax_rate/100) + 
    - ans[[x]]$adj_depr*input$tax_rate/100 + ans[[x]]$adj_principal + ans[[x]]$adj_salvage
  
  ans[[x]]$adj_WACC_hurdle <- -pmt(ans[[x]]$WACC/100, ans[[x]]$planning_horizon, 
                                     npv(ans[[x]]$WACC/100,  ans[[x]]$table_cash_flow$downpayment[-1])+
                                       + ans[[x]]$table_cash_flow$downpayment[1]) +
          + pmt(input$hurdle_rate/100, ans[[x]]$planning_horizon, 
          npv(input$hurdle_rate/100, ans[[x]]$table_cash_flow$downpayment[-1])+ans[[x]]$table_cash_flow$downpayment[1])
  
  ans[[x]]$net_annual_impact_after_tax <-  ans[[x]]$pb_net_annual_impact_before_tax + ans[[x]]$tax_revenue_minus_expense +
    + ans[[x]]$tax_interest + ans[[x]]$tax_depreciation + ans[[x]]$adj_WACC_interest + ans[[x]]$adj_WACC_hurdle
  
  ans[[x]]$tax_deduction_milking <- 
    -input$tax_rate/100 *(pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                              npv(ans[[x]]$avg_interest/100, ans[[x]]$table_depreciation$depreciation_robot))
                          +  pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                                 npv(ans[[x]]$avg_interest/100, ans[[x]]$table_debt$robot_interest)))
  
  ans[[x]]$tax_deduction_housing <-   
    -input$tax_rate/100 *(pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                              npv(ans[[x]]$avg_interest/100, ans[[x]]$table_depreciation$depreciation_housing))
                          +  pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                                 npv(ans[[x]]$avg_interest/100, ans[[x]]$table_debt$barn_interest)))
  })
})
  
})


# 
# 
# 
# #  ------ Dashboard  triggered by any change in; -----------
# #   input$NAI
# # ans[[x]]$inc_rev_total
# # ans[[x]]$dec_exp_total
# # ans[[x]]$inc_exp_total
# # ans[[x]]$capital_cost_total
# lapply(base_profiles, function(x) {
#   
#   
# observe({
#   browser()
#   
#   if (is.na(ans[[x]]$inc_rev_total)) { return() }
#   
#   input$NAI
#   ans[[x]]$inc_rev_total
#   ans[[x]]$dec_exp_total
#   ans[[x]]$inc_exp_total
#   ans[[x]]$capital_cost_total
#   
#   
#   isolate({
#     
#     ans[[x]]$tax_factor <- (1-(input$NAI=="after tax")*input$tax_rate/100)
#     
#     if (input$NAI=="before tax") {
#       ans[[x]]$NAI <- ans[[x]]$net_annual_impact_before_tax
#     } else {
#       ans[[x]]$NAI <- ans[[x]]$net_annual_impact_after_tax
#     }
#     
#     ans[[x]]$IOFC <- (input$milk_cow_day * input$price_milk/100 - ans[[x]]$DMI_day * input$cost_DM )*330 * ans[[x]]$tax_factor
#     
#     ans[[x]]$IOFC2 <- (ans[[x]]$milk_day_cow_alt * input$price_milk/100 + 
#                    - ans[[x]]$DMI_projected * input$cost_DM - input[[paste0("pellets",x)]] *
#                      input[[paste0("cost_pellets",x)]]/2000)*330 * ans[[x]]$tax_factor  
#     
#     ans[[x]]$IOFC_cwt <- ans[[x]]$IOFC /365 /input$milk_cow_day * 330 
#     
#     ans[[x]]$IOFC2_cwt <- ans[[x]]$IOFC2 /365 /ans[[x]]$milk_day_cow_alt * 330 
#     
#     ans[[x]]$milk_current <- 
#       input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
#                            +  input$scc_premium/100 * input$scc_average/1000) * ans[[x]]$tax_factor
#     
#     ans[[x]]$milk_robot <- (ans[[x]]$herd_size2 * 330 * ans[[x]]$milk_day_cow_alt *
#                         (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*
#                            (1-input[[paste0("scc_change",x)]]/100)/1000)) * ans[[x]]$tax_factor 
#     
#     ans[[x]]$labor_current <-  (input$hr_heat_detection + input$hours_milking) * 
#       input$labor_rate*365 * ans[[x]]$tax_factor 
#     
#     ans[[x]]$labor_robot <- ((input[[paste0("anticipated_hours_heat",x)]] + ans[[x]]$anticipated_hours_milking) * input$labor_rate*365 +
#                          + (input[[paste0("increase_rc_mgt",x)]] - input[[paste0("decrease_lab_mgt",x)]]) * input$labor_rate_rc_mgt *365 +
#                          + input$additional_labor * input[[paste0("herd_increase",x)]]) * ans[[x]]$tax_factor 
#     
#     ans[[x]]$feed_current <-  ans[[x]]$DMI_day * input$cost_DM * 330 * input$herd_size * ans[[x]]$tax_factor
#     
#     ans[[x]]$feed_robot <- (ans[[x]]$DMI_projected * input$cost_DM + input[[paste0("pellets",x)]] *
#                         input[[paste0("cost_pellets",x)]]/2000) * 330 * ans[[x]]$herd_size2 * ans[[x]]$tax_factor 
#     
#     ans[[x]]$milk_feed <-  (-(ans[[x]]$feed_robot - ans[[x]]$feed_current) + ans[[x]]$milk_robot -  ans[[x]]$milk_current )
#     
#     ans[[x]]$labor_repair <- -(ans[[x]]$labor_robot - ans[[x]]$labor_current + ans[[x]]$inc_exp_repair) 
#     
#     ans[[x]]$inflation <- - pmt(input$interest/100, ans[[x]]$planning_horizon, 
#                           npv(input$interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
#       - (ans[[x]]$inc_rev_total + ans[[x]]$dec_exp_total - ans[[x]]$inc_exp_total) 
#     
#     ans[[x]]$capital <- -ans[[x]]$capital_cost_total + 
#       +(input$NAI=="after tax")*(ans[[x]]$tax_interest + ans[[x]]$tax_depreciation)
#     
#     ans[[x]]$misc <- ans[[x]]$NAI - (ans[[x]]$milk_feed + ans[[x]]$labor_repair + ans[[x]]$capital + ans[[x]]$inflation) 
#     
#     ans[[x]]$capital_recovery_robot2 <- ans[[x]]$capital_recovery_robot +
#       - (input$NAI=="after tax")*ans[[x]]$tax_deduction_milking
#     ans[[x]]$capital_recovery_housing2 <- ans[[x]]$capital_recovery_housing +
#       - (input$NAI=="after tax")*ans[[x]]$tax_deduction_housing
#     
#   })
# })
# 
# })
# 
# 
# 
# 
# ## ------------ Breakeven Calculations ------------
# ## Triggered when Partial Budget tab is active, Profile-sepcific tab is active,
# ##  and it needs to be updated (signaled from a variable updated by calculation_main). 
#  
# lapply(base_profiles, function(x) { 
#   observe({
#     # Trigger Mechanism 
#     
#     
# isolate({
#   
#   n_years <- ans[[x]]$planning_horizon 
#   
#   table_breakeven <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
#   
#   colnames(table_breakeven) <- c("year","increased_expense","capital_cost_minus_downpayment", "cost_downpayment",
#                                  "increased_revenue","reduced_labor_management","reduced_heat_detection",
#                                  "reduced_labor","cost_capital_WACC","tax_deduction")
#   
#   table_breakeven$increased_expense <- lapply(c(1:n_years), function(t) {
#     ans[[x]]$inc_exp_total*(1+input$inflation_margin/100)^(t-1)
#   }) %>% unlist()
#   
#   table_breakeven$increased_revenue <- lapply(c(1:n_years), function(t) {
#     ans[[x]]$inc_rev_total*(1+input$inflation_margin/100)^(t-1)
#   }) %>% unlist()
#   
#   table_breakeven$capital_cost_minus_downpayment <- rep((ans[[x]]$capital_cost_total - ans[[x]]$cost_downpayment),n_years)
#   
#   table_breakeven$cost_downpayment <- rep((ans[[x]]$cost_downpayment),n_years)
#   
#   table_breakeven$reduced_labor_management <- lapply(c(1:n_years), function(t) {
#     ans[[x]]$dec_exp_labor_management *(1+input$inflation_labor/100)^(t-1)
#   }) %>% unlist()
#   
#   table_breakeven$reduced_heat_detection <- lapply(c(1:n_years), function(t) {
#     ans[[x]]$dec_exp_heat_detection *(1+input$inflation_labor/100)^(t-1)
#   }) %>% unlist()
#   
#   table_breakeven$reduced_labor <- lapply(c(1:n_years), function(t) {
#     ans[[x]]$dec_exp_labor *(1+input$inflation_labor/100)^(t-1)
#   }) %>% unlist()
#   
#   if (ans[[x]]$planning_horizon < n_years) {
#     for (i in c("increased_expense","increased_revenue","reduced_labor_management",
#                 "reduced_heat_detection","reduced_labor"))
#       table_breakeven[[i]][(ans[[x]]$planning_horizon+1):n_years] <- 0 
#   }
#   
#   table_breakeven <- rbind(0, table_breakeven)
#   
#   table_breakeven$cost_capital_WACC <- -(ans[[x]]$table_cash_flow$downpayment + ans[[x]]$table_cash_flow$salvage +
#                                            + ans[[x]]$table_cash_flow$interest + ans[[x]]$table_cash_flow$principal)
#   
#   table_breakeven$tax_deduction <- -input$tax_rate/100 * (ans[[x]]$table_cash_flow$depreciation + ans[[x]]$table_cash_flow$interest) 
#   
#   
#   ans[[x]]$npv_interest <- list()
#   ans[[x]]$annuity_interest <- list()
#   ans[[x]]$npv_WACC <- list()
#   ans[[x]]$annuity_WACC <- list()
#   ans[[x]]$npv_hurdle <- list()
#   ans[[x]]$annuity_hurdle <- list()
#   
#   cnames1 <- colnames(table_breakeven)
#   lapply(cnames1, function(x) {
#     
#     ans[[x]]$npv_interest[[paste0(x)]] <- npv(input$interest/100, table_breakeven[[paste0(x)]][-1]) +
#       + table_breakeven[[paste0(x)]][1]
#     
#     ans[[x]]$npv_WACC[[paste0(x)]] <- npv(ans[[x]]$WACC/100, table_breakeven[[paste0(x)]][-1]) +
#       + table_breakeven[[paste0(x)]][1]
#     
#     ans[[x]]$annuity_interest[[paste0(x)]] <- -pmt(input$interest/100, n_years,ans[[x]]$npv_interest[[paste0(x)]])
#     ans[[x]]$annuity_WACC[[paste0(x)]] <- -pmt(ans[[x]]$WACC/100, n_years,ans[[x]]$npv_WACC[[paste0(x)]])
#   })
#   
#   ans[[x]]$npv_hurdle[["cost_downpayment"]] <- npv(input$hurdle_rate/100, table_breakeven[["cost_downpayment"]][-1]) +
#     + table_breakeven[["cost_downpayment"]][1]
#   ans[[x]]$annuity_hurdle[["cost_downpayment"]] <- -pmt(ans[[x]]$WACC/100, n_years,ans[[x]]$npv_hurdle[["cost_downpayment"]])
#   
# 
#   ans[[x]]$bw_wage_before_tax <- (ans[[x]]$annuity_interest$increased_expense + ans[[x]]$annuity_interest$capital_cost_minus_downpayment +
#                               + ans[[x]]$annuity_hurdle$cost_downpayment - ans[[x]]$annuity_interest$increased_revenue + 
#                               - ans[[x]]$annuity_interest$reduced_labor_management)/
#     ((ans[[x]]$annuity_interest$reduced_heat_detection + ans[[x]]$annuity_interest$reduced_labor)/input$labor_rate)
#   
#   ans[[x]]$bw_wage_after_tax <-  ((ans[[x]]$annuity_WACC$increased_expense - ans[[x]]$annuity_WACC$increased_revenue + 
#                                - ans[[x]]$annuity_WACC$reduced_labor_management)*(1-input$tax_rate/100)  + 
#                               ans[[x]]$annuity_WACC$cost_capital_WACC - ans[[x]]$annuity_WACC$tax_deduction)/
#     ((ans[[x]]$annuity_WACC$reduced_heat_detection + ans[[x]]$annuity_WACC$reduced_labor)*(1-input$tax_rate/100)/input$labor_rate)
#   
#   payment1 <- -ans[[x]]$dec_exp_total/(1 + input$interest/100)
#   payment2 <- -ans[[x]]$dec_exp_total/(1 + ans[[x]]$WACC/100)*(1-input$tax_rate/100)
#   
#   npv1 <- ans[[x]]$npv_interest$increased_expense + ans[[x]]$npv_interest$capital_cost_minus_downpayment +
#     + ans[[x]]$npv_hurdle$cost_downpayment - ans[[x]]$npv_interest$increased_revenue + payment1
#   
#   npv2 <- (ans[[x]]$npv_WACC$increased_expense  - ans[[x]]$npv_WACC$increased_revenue) *(1-input$tax_rate/100) + 
#     + ans[[x]]$npv_WACC$cost_capital_WACC - ans[[x]]$npv_WACC$tax_deduction + payment2 
#   
#   ans[[x]]$be_wage_positive_minus_negative <-  (ans[[x]]$negative_total - ans[[x]]$inc_rev_total - ans[[x]]$dec_exp_labor_management)/ 
#     ((ans[[x]]$dec_exp_heat_detection + ans[[x]]$dec_exp_labor )/input$labor_rate) 
#   
#   ans[[x]]$bw_wage_inflation_before_tax <- (1 + input$interest/100)/(1 + rate(n_years-1, payment1, npv1)) - 1
#   
#   ans[[x]]$bw_wage_inflation_after_tax <-  (1 + ans[[x]]$WACC/100)/(1 + rate(n_years-1, payment2, npv2)) - 1
#   
# })
# })
# }) 
#   
