

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
#     "planning_horizon", "cost_parlors", "cost_robot", "useful_years", "n_robot")
#     

lapply(base_profiles, function(x) {
  observe({
    
    # Calculations given a profile 
    
    browser()

    if (x=="Robots") {
      ans[[x]]$cost_milking <- input[[paste0("n_robot",x)]] * input[[paste0("cost_robot",x)]]
      ans[[x]]$repair_total <- input[[paste0("repair",x)]]* input[[paste0("n_robot",x)]] 
    } else {
      ans[[x]]$cost_milking <- input[[paste0("cost_parlors",x)]]
      ans[[x]]$repair_total <-  input[[paste0("repair",x)]] 
    }
    if ( input[[paste0("n_sets",x)]]=="2") ans[[x]]$n_sets <- 2
    else  ans[[x]]$n_sets <- 1
  
    ans[[x]]$cost_milking2 <-  ans[[x]]$cost_milking*(1+input$inflation_robot/100)^
      (input[[paste0("useful_years",x)]] + input[[paste0("yr_system1",x)]])*
      (input[[paste0("n_sets",x)]] == 2) 
    
    ans[[x]]$planning_horizon <- ans[[x]]$n_sets * input[[paste0("useful_years",x)]] + input[[paste0("yr_system1",x)]] 
    
    ans[[x]]$salvage_milking_fv1 <- input[[paste0("salvage_milking1",x)]] *
      (1+input$inflation_robot/100)^(input[[paste0("useful_years",x)]] + input[[paste0("yr_system1",x)]])
    
    ans[[x]]$salvage_milking_fv2 <- input[[paste0("salvage_milking1",x)]] * 
      (1+input$inflation_robot/100)^(input[[paste0("useful_years",x)]]*2+ input[[paste0("yr_system1",x)]]) *
      (input[[paste0("n_sets",x)]] >=2)

    # Partial Budget Calculations: Data Entry side calculations
    ans[[x]]$herd_size2 <- input$herd_size + input[[paste0("herd_increase",x)]]
    
    ans[[x]]$cost_housing <- input[[paste0("cost_housing_cow",x)]] * ans[[x]]$herd_size2
    
    ans[[x]]$total_investment_cow <-  input[[paste0("cost_housing_cow",x)]] + ans[[x]]$cost_milking/ans[[x]]$herd_size2
    
    ans[[x]]$total_investment <- ans[[x]]$total_investment_cow  * ans[[x]]$herd_size2
    
    ans[[x]]$increased_insurance <- ans[[x]]$total_investment
    
    ans[[x]]$anticipated_hours_milking <- input[[paste0("hours_milking",x)]] - input[[paste0("hr_sv_milking",x)]]
    
    ans[[x]]$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
      + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
    
    ans[[x]]$milk_day_cow_alt <- input$milk_cow_day + input[[paste0("milk_change",x)]]
    
    ans[[x]]$milk_lb_alt_day <- ans[[x]]$milk_day_cow_alt * ans[[x]]$herd_size2/input[[paste0("n_robot",x)]]
    
    ans[[x]]$adj_milk_cow_day2 <- ans[[x]]$milk_day_cow_alt * input$milk_cow_coeff + 
      + ans[[x]]$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 
    
    ans[[x]]$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
    
    
    ans[[x]]$DMI_day <-  ans[[x]]$stage_lactation * (ans[[x]]$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                           +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    
    if (x=="Robots") {
    ans[[x]]$DMI_projected <-  ans[[x]]$stage_lactation *
      (ans[[x]]$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
        +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    } else {
      ans[[x]]$DMI_projected <- ans[[x]]$DMI_day
    }
    
    ans[[x]]$DMI_change <- ans[[x]]$DMI_projected - ans[[x]]$DMI_day
    
    
    ans[[x]]$adj_milk_cow_day2 <- ans[[x]]$milk_day_cow_alt * input$milk_cow_coeff + 
      + ans[[x]]$milk_day_cow_alt  * input$milk_fat/100 * input$milk_fat_coeff 
    
    
    # Cash Flow items to render in Data Entry
    ans[[x]]$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero
    
    ans[[x]]$loan_housing <- ans[[x]]$cost_housing - input[[paste0("down_housing",x)]] 
                              -input[[paste0("delay_housing1",x)]]
    ans[[x]]$loan_milking1 <- ans[[x]]$cost_milking - input[[paste0("down_milking1",x)]]  
                              +input[[paste0("delay_housing1",x)]]
    ans[[x]]$loan_milking2 <- ans[[x]]$cost_milking2 - input[[paste0("down_milking2",x)]] 
    
    
    ans[[x]]$yr_sustem2 <- input[[paste0("useful_years",x)]]
    ans[[x]]$copy_salvage_milking1 <- input[[paste0("salvage_milking1",x)]]*
        (1+input$inflation_robot/100)^(1+input[[paste0("yr_system1",x)]]) 
    ans[[x]]$copy_salvage_milking2 <- ans[[x]]$salvage_milking_fv1
    
    ans[[x]]$copy_cost_housing <- ans[[x]]$cost_housing
    ans[[x]]$copy_cost_milking1 <- ans[[x]]$cost_milking
    ans[[x]]$copy_cost_milking2 <- ans[[x]]$cost_milking2
    
    ans[[x]]$r_milking2 <- input[[paste0("r_milking1",x)]]
    
    # Positive Impacts (year 1)
    ans[[x]]$inc_rev_herd_size <- ans[[x]]$milk_day_cow_alt * 330 *
      (input$price_milk/100) * input[[paste0("herd_increase",x)]]
    
    ans[[x]]$inc_rev_per_cow <- input[[paste0("milk_change",x)]] * 330 * (input$price_milk/100) * input$herd_size
    
    ans[[x]]$inc_rev_milk_premium  <- ans[[x]]$milk_day_cow_alt *330 * input$scc_premium/100*
      (input$scc_average*(-input[[paste0("scc_change",x)]])/100)/1000 * ans[[x]]$herd_size2
    
    ans[[x]]$inc_rev_cull_sale   <- ans[[x]]$herd_size2 * input[[paste0("change_turnover",x)]]/100 * input$cull_price
    
    ans[[x]]$inc_rev_software  <- input[[paste0("software",x)]] * ans[[x]]$herd_size2
    
    ans[[x]]$inc_rev_total <- ans[[x]]$inc_rev_herd_size + ans[[x]]$inc_rev_per_cow + ans[[x]]$inc_rev_milk_premium +
      + ans[[x]]$inc_rev_cull_sale + ans[[x]]$inc_rev_software
    
    ans[[x]]$dec_exp_heat_detection <- (input$hr_heat_detection - input[[paste0("anticipated_hours_heat",x)]] )*input$labor_rate *365
    
    ans[[x]]$dec_exp_labor <- input[[paste0("hr_sv_milking",x)]] * input$labor_rate *365 
    
    ans[[x]]$dec_exp_labor_management <- input[[paste0("decrease_lab_mgt",x)]] * input$labor_rate_rc_mgt * 365
    
    ans[[x]]$dec_exp_total <- ans[[x]]$dec_exp_heat_detection  + ans[[x]]$dec_exp_labor + ans[[x]]$dec_exp_labor_management
    
    ans[[x]]$positive_total <- ans[[x]]$inc_rev_total +  ans[[x]]$dec_exp_total
    
    
    # Negative Impacts (year 1)
    ans[[x]]$inc_exp_herd_increase <- (input[[paste0("additional_labor",x)]] + input[[paste0("additional_cost",x)]])*input[[paste0("herd_increase",x)]]
    
    
    ans[[x]]$inc_exp_repair <-ans[[x]]$repair_total + input[[paste0("insurance_rate",x)]]/100 * ans[[x]]$increased_insurance
    
    
    ans[[x]]$inc_exp_feed <-  ans[[x]]$DMI_change * input$cost_DM * 330 * ans[[x]]$herd_size2
    
    ans[[x]]$inc_exp_pellet <- input[[paste0("cost_pellets",x)]] * 330 * ans[[x]]$herd_size2 * input[[paste0("pellets",x)]]/2000
    
    ans[[x]]$inc_exp_replacement <- input$cost_heifer * input[[paste0("change_turnover",x)]]/100 * ans[[x]]$herd_size2
    
    ans[[x]]$inc_exp_utilities <- (input[[paste0("change_electricity",x)]] + input[[paste0("change_water",x)]] +
                                    + input[[paste0("change_chemical",x)]] )* ans[[x]]$herd_size2 
    
    ans[[x]]$inc_exp_record_management <- input[[paste0("increase_rc_mgt",x)]] * input$labor_rate_rc_mgt * 365
    
    ans[[x]]$inc_exp_total <- ans[[x]]$inc_exp_herd_increase + ans[[x]]$inc_exp_repair + ans[[x]]$inc_exp_feed + ans[[x]]$inc_exp_pellet +
      + ans[[x]]$inc_exp_replacement +  ans[[x]]$inc_exp_utilities + ans[[x]]$inc_exp_record_management  
    
    
    ans[[x]]$WACC <- ((input[[paste0("down_housing",x)]] +input[[paste0("down_milking1",x)]]) * input$hurdle_rate +
                  + (ans[[x]]$loan_housing * input$r_housing + ans[[x]]$loan_milking1 * input$r_milking1)*
                  (1-input$tax_rate/100))/(ans[[x]]$cost_housing + ans[[x]]$cost_milking) 
    
    source(file.path("session_files", "session_cash_flow.R"), local=TRUE)  # Calculates cash flow tables
    
    
    ans[[x]]$capital_recovery_milking <-  -pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon, 
                                       npv(input[[paste0("r_milking1",x)]]/100, 
                                           ans[[x]]$table_debt$milking_interest+ans[[x]]$table_debt$milking_principal)) 
    
    ans[[x]]$capital_recovery_housing  <- -pmt(input[[paste0("r_housing",x)]]/100, ans[[x]]$planning_horizon, 
                                         npv(input[[paste0("r_housing",x)]]/100, 
                                             ans[[x]]$table_debt$barn_interest+ans[[x]]$table_debt$barn_principal))
    
    ans[[x]]$milking_end_PV <-   pmt(input[[paste0("r_milking1",x)]]/100, ans[[x]]$planning_horizon,  
                             npv(input[[paste0("r_milking1",x)]]/100, 
                                 ans[[x]]$table_cash_flow$salvage[-1])) 
    # This will be shown as negative cost
    
    ans[[x]]$cost_downpayment <-  pmt(input$hurdle_rate/100, ans[[x]]$planning_horizon, 
                                npv(input$hurdle_rate/100, 
                                    ans[[x]]$table_cash_flow$downpayment[-1])+ans[[x]]$table_cash_flow$downpayment[1]) 
    
    ans[[x]]$capital_cost_total <- ans[[x]]$capital_recovery_milking + ans[[x]]$capital_recovery_housing +
      + ans[[x]]$cost_downpayment + ans[[x]]$milking_end_PV
    
    
    
  })  
})







