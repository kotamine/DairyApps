
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
  
  observeEvent(ans[[x]]$planning_horizon, {
    updateSliderInput(session, paste0("budget_year",x), "Select budget year",value=1, min=1, max=ans[[x]]$planning_horizon)
  })  
  
  
# This needs to respond to change in any data input or change in input[[paste0("budget_year",x)]]
# Results are stored in a separate list, e.g., ans[["Robots_pb"]]  etc. to avoid triggering other calculations. 
#observe({ 
output[[paste0("pb_net_annual_impact_before_tax",x)]] <- renderUI({
  browser()
  # if (is.null(ans[[x]]$positive_total)) {  return() } 
  
  isolate({
  ans[[paste0(x,"_pb")]]$pb_positive_total <- ans[[x]]$inc_rev_total * (1+input$inflation_margin/100)^(input[[paste0("budget_year",x)]]-1) +
    + ans[[x]]$dec_exp_total *  (1+input$inflation_labor/100)^(input[[paste0("budget_year",x)]]-1)

  ans[[paste0(x,"_pb")]]$pb_negative_total <- ans[[x]]$inc_exp_total * (1+input$inflation_margin/100)^(input[[paste0("budget_year",x)]]-1) +
    + ans[[x]]$capital_cost_total

  ans[[paste0(x,"_pb")]]$pb_positive_minus_negative <-  ans[[paste0(x,"_pb")]]$pb_positive_total - ans[[paste0(x,"_pb")]]$pb_negative_total

  ans[[paste0(x,"_pb")]]$pb_inflation_adjustment <-  - pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
    - (ans[[paste0(x,"_pb")]]$pb_positive_total - ans[[paste0(x,"_pb")]]$pb_negative_total + ans[[x]]$capital_cost_total)

  ans[[paste0(x,"_pb")]]$pb_net_annual_impact_before_tax <- ans[[paste0(x,"_pb")]]$pb_positive_minus_negative +
    + ans[[paste0(x,"_pb")]]$pb_inflation_adjustment 
  })
  ans[[paste0(x,"_pb")]]$pb_net_annual_impact_before_tax  %>% formatdollar() %>% helpText() %>% div(align="right")
})


# Create tax and WACC adjustment calculations  -----------
# observe({ 

output[[paste0("net_annual_impact_after_tax",x)]] <- renderUI({  
  
  ans[[x]]$inc_rev_total
  ans[[x]]$dec_exp_total
  ans[[x]]$inc_exp_total
  ans[[x]]$capital_cost_total
  
  browser() 
  isolate({
    # if ( is.null(ans[[x]]$planning_horizon) ) {  return() } 
ans[[x]]$tax_revenue_minus_expense <-  input$tax_rate/100 *
  pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon,
      npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1]))

# # cost of cash flows for interest payments (evaluated at separate instests for milking and housing)
# ans[[x]]$interest_at_interest <-  pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon,
#                                       npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_debt$milking_interest)) +
#   + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon,
#         npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_debt$barn_interest))
# 
# ans[[x]]$tax_interest <-  -input$tax_rate/100 * ans[[x]]$interest_at_interest
# 
# 
# ans[[x]]$principal_at_interest <-  pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon,
#                                        npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_debt$milking_principal)) +
#   + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon,
#         npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_debt$barn_principal))
# 
# 
# # cost of depreciation (evaluated at separate instests for milking and housing)
# ans[[x]]$depreciation_at_interest <-
#   (pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon,
#        npv(input[[paste0("r_milking1",x)]]/100, ans[[x]]$table_depreciation$depreciation_milking_system)) +
#      + pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon,
#            npv(input[[paste0("r_housing",x)]]/100, ans[[x]]$table_depreciation$depreciation_housing)))
# 
# ans[[x]]$tax_depreciation <- -input$tax_rate/100 * ans[[x]]$depreciation_at_interest



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

ans[[x]]$net_annual_impact_after_tax <-  ans[[x]]$net_annual_impact_before_tax + ans[[x]]$tax_revenue_minus_expense +
  + ans[[x]]$tax_interest + ans[[x]]$tax_depreciation + ans[[x]]$adj_WACC_interest + ans[[x]]$adj_WACC_hurdle

# ans[[x]]$tax_deduction_milking <-
#   -input$tax_rate/100 *(pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon,
#                             npv(ans[[x]]$avg_interest/100, ans[[x]]$table_depreciation$depreciation_robot))
#                         +  pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon,
#                                npv(ans[[x]]$avg_interest/100, ans[[x]]$table_debt$robot_interest)))
# 
# ans[[x]]$tax_deduction_housing <-
#   -input$tax_rate/100 *(pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon,
#                             npv(ans[[x]]$avg_interest/100, ans[[x]]$table_depreciation$depreciation_housing))
#                         +  pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon,
#                                npv(ans[[x]]$avg_interest/100, ans[[x]]$table_debt$barn_interest)))

  ans[[x]]$net_annual_impact_after_tax %>% formatdollar() %>% helpText() %>% div(align="right")
})
})

})


# Creat plots for Partial Budget:
#   "PB_plot_pos_neg_impact"
#   "PB_plot_before_tax_impact"
#   "PB_plot_after_tax_impact"
#   "PB_plot_after_tax_impact_const"

project_inflation <- function(T, value, inflation, round=0) {
lapply(c(1:T), function(t) { 
  value * (1+inflation)^(t-1) }) %>% unlist(use.names = FALSE) %>% round(round)
}

lapply(base_profiles, function(x) {
  
  output[[paste0("PB_plot_pos_neg_impact",x)]] <- renderGvis({
      need(!is.null(ans[[x]]$net_annual_impact_after_tax),"NA") %>% validate()
  
      n_year <- ans[[x]]$planning_horizon 
      df <- data.frame(Year=c(1:n_year))
      df$Increased_Revenue_Total <- project_inflation(n_year, ans[[x]]$inc_rev_total, input$inflation_margin/100)
      df$Decreased_Expense_Total <- project_inflation(n_year, ans[[x]]$dec_exp_total, input$inflation_margin/100)
      df$Increased_Expense_Total <- project_inflation(n_year, ans[[x]]$inc_exp_total, input$inflation_margin/100)
      df$Annualized_Capital_Cost <-  rep(round(ans[[x]]$capital_cost_total), n_year)
      
      gvisLineChart(df, xvar="Year", 
                    yvar=c("Increased_Revenue_Total", "Decreased_Expense_Total",
                           "Increased_Expense_Total","Annualized_Capital_Cost"),
                    options=list(
                      title=paste("Partial Budget Components for", refProfileName(x)), 
                      vAxis="{title:'Cash flows and annualized values ($)'}",
                      hAxis="{title:'Year', ticks: [5,10,15,20,25,30] }", 
                       legend="{position: 'right'}" ,
                       chartArea ='{width: "50%", height: "65%" }' 
                    ))
  })
  
  
  output[[paste0("PB_plot_before_tax_impact",x)]] <- renderGvis({
    need(!is.null(ans[[x]]$net_annual_impact_after_tax),"NA") %>% validate()
    
    n_year <- ans[[x]]$planning_horizon 
    df <- data.frame(Year=c(1:n_year))
    df$Positive_Minus_Negative <- project_inflation(n_year, ans[[x]]$positive_total-ans[[x]]$inc_exp_total, 
                                                    input$inflation_margin/100) - round(ans[[x]]$capital_cost_total) 
    df$Inflation_Adjustments <-   - pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                                        npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
      - (project_inflation(n_year, ans[[x]]$positive_total, input$inflation_margin/100) +
         - project_inflation(n_year, ans[[x]]$inc_exp_total, input$inflation_margin/100)) %>% round() 
    
    df$Annualized_Before_Tax_Impact <-  rep(round(ans[[paste0(x,"_pb")]]$pb_net_annual_impact_before_tax), n_year) 
    
    gvisLineChart(df, xvar="Year", 
                  yvar=c("Positive_Minus_Negative", "Inflation_Adjustments",
                         "Annualized_Before_Tax_Impact"),
                  options=list(
                    title=paste("Definition of Inflation Adjustments:", refProfileName(x)), 
                    vAxis="{title:'Cash flows and annualized values ($)'}",
                    hAxis="{title:'Year', ticks: [5,10,15,20,25,30] }", 
                    legend="{position: 'right'}" ,
                    chartArea ='{width: "50%", height: "65%" }' 
                  ))
  }) 
  
  output[[paste0("PB_plot_after_tax_impact",x)]] <- renderGvis({
    need(!is.null(ans[[x]]$net_annual_impact_after_tax),"NA") %>% validate()
    
    n_year <- ans[[x]]$planning_horizon 
    df <- data.frame(Year=c(0:n_year))
    df$Before_Tax_Cashflow <-  round(ans[[x]]$table_cash_flow$before_tax_cash_flow)
    df$Tax_Operating_Income <- round(ans[[x]]$table_cash_flow$operating_income * input$tax_rate/100)
    df$Deduction_Interest <-  round(-ans[[x]]$table_cash_flow$interest_total * input$tax_rate/100)
    df$Deduction_Depreciation <- round(-ans[[x]]$table_cash_flow$depreciation * input$tax_rate/100)
    df$After_Tax_Cashflow <- round(ans[[x]]$table_cash_flow$after_tax_cash_flow)
    
    gvisLineChart(df, xvar="Year", 
                  yvar=c("Before_Tax_Cashflow", "Tax_Operating_Income",
                         "Deduction_Interest","Deduction_Depreciation","After_Tax_Cashflow"),
                  options=list(
                    title=paste("Impact of Tax and Tax Deductions:", refProfileName(x)), 
                    vAxis="{title:'Cash flows ($)'}",
                    hAxis="{title:'Year', ticks: [5,10,15,20,25,30] }", 
                    legend="{position: 'right'}" ,
                    chartArea ='{width: "50%", height: "65%" }' 
                  ))
  })
  
  output[[paste0("PB_plot_after_tax_impact_const",x)]] <- renderGvis({
    need(!is.null(ans[[x]]$net_annual_impact_after_tax),"NA") %>% validate()
    
    n_year <- ans[[x]]$planning_horizon 
    df <- data.frame(Year=c(1:n_year))
    df$Before_Tax_Cashflow <-  rep(round(ans[[x]]$net_annual_impact_before_tax), n_year)
    df$Tax_Operating_Income <- rep(round(ans[[x]]$tax_revenue_minus_expense),n_year)
    df$Deduction_Interest <-   rep(round(ans[[x]]$tax_interest),n_year)
    df$Deduction_Depreciation <- rep(round(ans[[x]]$tax_depreciation),n_year)
    df$After_Tax_Cashflow <-  rep(round(ans[[x]]$net_annual_impact_after_tax), n_year)
    
    gvisLineChart(df, xvar="Year", 
                  yvar=c("Before_Tax_Cashflow", "Tax_Operating_Income",
                         "Deduction_Interest","Deduction_Depreciation","After_Tax_Cashflow"),
                  options=list(
                    title=paste("Annualized Cash Flows:", refProfileName(x)), 
                    vAxis="{title:'Annualized values ($)'}",
                    hAxis="{title:'Year', ticks: [5,10,15,20,25,30] }", 
                    legend="{position: 'right'}" ,
                    chartArea ='{width: "50%", height: "65%" }' 
                  ))
  })
  
})  
  

## ------------ Breakeven Calculations ------------
## Triggered when Partial Budget tab is active, Profile-sepcific tab is active,
##  and it needs to be updated (signaled from a variable updated by calculation_main).

lapply(base_profiles, function(x) {
  output[[paste0("breakeven_chart",x)]] <- renderGvis({
    
    browser() 
    
    # Trigger Mechanism
    ans[[x]]$net_annual_impact_after_tax

isolate({

  n_year <- ans[[x]]$planning_horizon

  table_breakeven <- matrix(c(c(1:n_year), rep(rep(0,n_year),9)),ncol=10,byrow=FALSE)  %>% data.frame()

  colnames(table_breakeven) <- c("year","increased_expense","capital_cost_minus_downpayment", "cost_downpayment",
                                 "increased_revenue","reduced_labor_management","reduced_heat_detection",
                                 "reduced_labor","cost_capital_WACC","tax_deduction")

  table_breakeven$increased_expense <- lapply(c(1:n_year), function(t) {
    ans[[x]]$inc_exp_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()

  table_breakeven$increased_revenue <- lapply(c(1:n_year), function(t) {
    ans[[x]]$inc_rev_total*(1+input$inflation_margin/100)^(t-1)
  }) %>% unlist()

  table_breakeven$capital_cost_minus_downpayment <- rep((ans[[x]]$capital_cost_total - ans[[x]]$cost_downpayment),n_year)

  table_breakeven$cost_downpayment <- rep((ans[[x]]$cost_downpayment),n_year)

  table_breakeven$reduced_labor_management <- lapply(c(1:n_year), function(t) {
    ans[[x]]$dec_exp_labor_management *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()

  table_breakeven$reduced_heat_detection <- lapply(c(1:n_year), function(t) {
    ans[[x]]$dec_exp_heat_detection *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()

  table_breakeven$reduced_labor <- lapply(c(1:n_year), function(t) {
    ans[[x]]$dec_exp_labor *(1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()

  if (ans[[x]]$planning_horizon < n_year) {
    for (i in c("increased_expense","increased_revenue","reduced_labor_management",
                "reduced_heat_detection","reduced_labor"))
      table_breakeven[[i]][(ans[[x]]$planning_horizon+1):n_year] <- 0
  }

  table_breakeven <- rbind(0, table_breakeven)

  table_breakeven$cost_capital_WACC <- -(ans[[x]]$table_cash_flow$downpayment + ans[[x]]$table_cash_flow$salvage +
                                           + ans[[x]]$table_cash_flow$interest + ans[[x]]$table_cash_flow$principal)

  table_breakeven$tax_deduction <- -input$tax_rate/100 * (ans[[x]]$table_cash_flow$depreciation + ans[[x]]$table_cash_flow$interest)

  ## --- Breakeven wage calcution for before-tax impact is currently suppressed -------
  # ans[[paste0(x,"_bw")]]$npv_interest <- list()
  # ans[[paste0(x,"_bw")]]$annuity_interest <- list()
  npv_WACC <- list()
  annuity_WACC <- list()
  npv_hurdle <- list()
  # ans[[paste0(x,"_bw")]]$annuity_hurdle <- list()

  cnames1 <- colnames(table_breakeven)
  for (z in cnames1) {

    # ans[[paste0(x,"_bw")]]$npv_interest[[paste0(x)]] <- npv(input$interest/100, table_breakeven[[paste0(x)]][-1]) +
    #   + table_breakeven[[paste0(x)]][1]

    npv_WACC[[paste0(z)]] <- npv(ans[[x]]$WACC/100, table_breakeven[[paste0(z)]][-1]) +
      + table_breakeven[[paste0(z)]][1]

    # ans[[paste0(x,"_bw")]]$annuity_interest[[paste0(x)]] <- -pmt(input$interest/100, n_year,ans[[x]]$npv_interest[[paste0(x)]])
    annuity_WACC[[paste0(z)]] <- -pmt(ans[[x]]$WACC/100, n_year, npv_WACC[[paste0(z)]])
  }

  # ans[[paste0(x,"_bw")]]$npv_hurdle[["cost_downpayment"]] <- npv(input$hurdle_rate/100, table_breakeven[["cost_downpayment"]][-1]) +
  #   + table_breakeven[["cost_downpayment"]][1]
  # ans[[x]]$annuity_hurdle[["cost_downpayment"]] <- -pmt(ans[[x]]$WACC/100, n_year,ans[[x]]$npv_hurdle[["cost_downpayment"]])


  # ans[[paste0(x,"_bw")]]$bw_wage_before_tax <- (ans[[x]]$annuity_interest$increased_expense + ans[[x]]$annuity_interest$capital_cost_minus_downpayment +
  #                             + ans[[x]]$annuity_hurdle$cost_downpayment - ans[[x]]$annuity_interest$increased_revenue +
  #                             - ans[[x]]$annuity_interest$reduced_labor_management)/
  #   ((ans[[x]]$annuity_interest$reduced_heat_detection + ans[[x]]$annuity_interest$reduced_labor)/input$labor_rate)

  ans[[paste0(x,"_bw")]]$bw_wage_after_tax <-  ((annuity_WACC$increased_expense -annuity_WACC$increased_revenue +
                               - annuity_WACC$reduced_labor_management)*(1-input$tax_rate/100)  +
                               annuity_WACC$cost_capital_WACC - annuity_WACC$tax_deduction)/
    ((annuity_WACC$reduced_heat_detection + annuity_WACC$reduced_labor)*(1-input$tax_rate/100)/input$labor_rate)

  # payment1 <- -ans[[x]]$dec_exp_total/(1 + input$interest/100)
  payment2 <- -ans[[x]]$dec_exp_total/(1 + ans[[x]]$WACC/100)*(1-input$tax_rate/100)

  # npv1 <- ans[[x]]$npv_interest$increased_expense + ans[[x]]$npv_interest$capital_cost_minus_downpayment +
  #   + ans[[x]]$npv_hurdle$cost_downpayment - ans[[x]]$npv_interest$increased_revenue + payment1

  npv2 <- (npv_WACC$increased_expense  - npv_WACC$increased_revenue) *(1-input$tax_rate/100) +
    + npv_WACC$cost_capital_WACC - npv_WACC$tax_deduction + payment2

  # ans[[x]]$be_wage_positive_minus_negative <-  (ans[[x]]$negative_total - ans[[x]]$inc_rev_total - ans[[x]]$dec_exp_labor_management)/
  #   ((ans[[x]]$dec_exp_heat_detection + ans[[x]]$dec_exp_labor )/input$labor_rate)

  # ans[[x]]$bw_wage_inflation_before_tax <- (1 + input$interest/100)/(1 + rate(n_year-1, payment1, npv1)) - 1

  ans[[paste0(x,"_bw")]]$bw_wage_inflation_after_tax <-  (1 + ans[[x]]$WACC/100)/(1 + rate(n_year-1, payment2, npv2)) - 1

  
  ## Create breakeven wage chart ----------------
  labor_rate <- ans[[paste0(x,"_bw")]]$bw_wage_after_tax
  inflation <- ans[[paste0(x,"_bw")]]$bw_wage_inflation_after_tax

  df <- data.frame(Year=c(1:n_year))
  df$Baseline_projection <- project_inflation(n_year, input$labor_rate, input$inflation_labor/100, round=2)
  df$Breakeven_wage_shift <- project_inflation(n_year,labor_rate, input$inflation_labor/100, round=2)
  df$Wage_inflation_shift <- project_inflation(n_year, input$labor_rate, inflation, round=2)
  
  gvisLineChart(df, xvar="Year",
                yvar=c("Baseline_projection", "Breakeven_wage_shift","Wage_inflation_shift"),
                options=list(
                  title=paste("After-tax Breakeven Wage:", refProfileName(x)),
                  vAxis="{title:'Wage Trajectory ($)'}", 
                  hAxis="{title:'Year'}", 
                  legend="{position: 'right'}",
                  chartArea ="{width: '50%', height: '65%' }"
                ) 
  )
  })   
}) 
})


# output[[paste0("breakeven_chart",x)]] <- renderGvis({
#   browser()
#   
#   
#     need(!is.null(ans[[paste0(x,"_bw")]]$bw_wage_before_tax),"NA") %>% validate()
# })


# 
# output$breakeven_numbers <- renderUI({
#   validate(
#     need(!is.null(ans[[x]]$bw_wage_before_tax),"NA")
#   )
#   if (input$breakeven_option=="wage") {
#     option <- "Wage:"
#     if (input$NAI=="before tax") {
#       labor_rate <- ans[[x]]$bw_wage_before_tax
#     } else {
#       labor_rate <- ans[[x]]$bw_wage_after_tax
#     }
#     be_val <- paste0("$", round(labor_rate, 2))
#     inflation <-  input$inflation_labor/100
#   } else {
#     option <- "Inflation:"
#     if (input$NAI=="before tax") {
#       inflation <- ans[[x]]$bw_wage_inflation_before_tax
#     } else {
#       inflation <- ans[[x]]$bw_wage_inflation_after_tax
#     }
#     be_val <- paste0(round(inflation*100,3),"%")
#     labor_rate <- input$labor_rate
#   }
# 
#   yr_one <- paste("Year", round(ans[[x]]$housing_years/3), ": ")
#   yr_two <- paste("Year", round(ans[[x]]$housing_years*(2/3)),": ")
#   yr_three <- paste("Year", round(ans[[x]]$housing_years),": ")
#   wage_zero <- labor_rate  %>% formatdollar(2)
#   wage_one <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years/3)-1))  %>% formatdollar(2)
#   wage_two <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years*2/3)-1))  %>% formatdollar(2)
#   wage_three <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years)-1))  %>% formatdollar(2)
# 
#   div(class="well well-sm", style= "background-color:	#778899; color:white;",
#       h4("Breakeven", option, be_val, align="center"),
#       h5("Year 1: ", wage_zero),
#       h5(yr_one, wage_one),
#       h5(yr_two, wage_two),
#       h5(yr_three, wage_three),
#       h5("under", robot_or_parlor())
#   )
# })
# 




