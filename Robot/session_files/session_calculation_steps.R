

if (x=="Robots") {
  ans[[X]]$cost_milking1 <- input[[paste0("n_robot",x)]] * ans[[X]]$cost_robot
  ans[[X]]$repair_total <- input[[paste0("n_robot",x)]] * input[[paste0("repair",x)]]
} else {
  ans[[X]]$cost_milking1 <- ans[[X]]$cost_parlors
  ans[[X]]$repair_total <-  input[[paste0("repair",x)]]
}

if ( input[[paste0("n_sets",x)]]=="2") {
  ans[[X]]$n_sets <- 2
} else  {
  ans[[X]]$n_sets <- 1
}

ans[[X]]$cost_milking2 <-  ans[[X]]$cost_milking1*(1+ans[[X]]$inflation_robot/100)^
  (ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]]) *(ans[[X]]$n_sets == 2) 

ans[[X]]$milking_horizon <- ans[[X]]$n_sets * max(ans[[X]]$useful_years)+ input[[paste0("yr_system1",x)]]

ans[[X]]$planning_horizon <- max(ans[[X]]$n_sets * max(ans[[X]]$useful_years,  
                                 input[[paste0("n_yr_milking1",x)]])+ input[[paste0("yr_system1",x)]], 
                                 input[[paste0("n_yr_housing",x)]])  
  
 
ans[[X]]$salvage_milking_fv1 <- input[[paste0("salvage_milking1",x)]] *
  (1+ans[[X]]$inflation_robot/100)^(ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]])

ans[[X]]$salvage_milking_fv2 <- input[[paste0("salvage_milking1",x)]] * 
  (1+ans[[X]]$inflation_robot/100)^(ans[[X]]$useful_years*2+ input[[paste0("yr_system1",x)]]) *
  (ans[[X]]$n_sets >=2)

ans[[X]]$yr_system2 <- ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]]

ans[[X]]$r_milking2 <- ans[[X]]$r_milking1

# Partial Budget Calculations: Data Entry side calculations
ans[[X]]$herd_size2 <- input$herd_size + input[[paste0("herd_increase",x)]]

ans[[X]]$cost_housing <-ans[[X]]$cost_housing_cow* ans[[X]]$herd_size2

ans[[X]]$total_investment_cow <- ans[[X]]$cost_housing_cow+ ans[[X]]$cost_milking1/ans[[X]]$herd_size2

ans[[X]]$total_investment <- ans[[X]]$total_investment_cow  * ans[[X]]$herd_size2

ans[[X]]$increased_insurance <- ans[[X]]$total_investment

ans[[X]]$anticipated_hours_milking <- input$hours_milking - input[[paste0("hr_sv_milking",x)]]

ans[[X]]$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
  + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff

ans[[X]]$milk_day_cow_alt <- input$milk_cow_day + ans[[X]]$milk_change 

ans[[X]]$milk_lb_alt_day <- ans[[X]]$milk_day_cow_alt * ans[[X]]$herd_size2/input[[paste0("n_robot",x)]]

ans[[X]]$adj_milk_cow_day2 <- ans[[X]]$milk_day_cow_alt * input$milk_cow_coeff + 
  + ans[[X]]$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 

ans[[X]]$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 

ans[[X]]$DMI_day <-  ans[[X]]$stage_lactation * 
  (ans[[X]]$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
     +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

ans[[X]]$DMI_projected <-  ans[[X]]$stage_lactation *
  (ans[[X]]$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
     +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

ans[[X]]$DMI_change <- ans[[X]]$DMI_projected - ans[[X]]$DMI_day



# Cash Flow items to render in Data Entry
ans[[X]]$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero

ans[[X]]$loan_housing <- ans[[X]]$cost_housing - input[[paste0("down_housing",x)]] +
  -input[[paste0("delay_housing1",x)]]
ans[[X]]$loan_milking1 <- ans[[X]]$cost_milking1 - input[[paste0("down_milking1",x)]]  +
  +input[[paste0("delay_housing1",x)]]
ans[[X]]$loan_milking2 <- ans[[X]]$cost_milking2 - input[[paste0("down_milking2",x)]] 
ans[[X]]$yr_sustem2 <- ans[[X]]$useful_years

# Positive Impacts (year 1)
ans[[X]]$inc_rev_herd_size <- ans[[X]]$milk_day_cow_alt * 330 *
  (input$price_milk/100) * input[[paste0("herd_increase",x)]]

ans[[X]]$inc_rev_per_cow <- ans[[X]]$milk_change* 330 * (input$price_milk/100) * input$herd_size

ans[[X]]$inc_rev_milk_premium  <- ans[[X]]$milk_day_cow_alt *330 * input$scc_premium/100*
  (input$scc_average*(-input[[paste0("scc_change",x)]])/100)/1000 * ans[[X]]$herd_size2

ans[[X]]$inc_rev_cull_sale   <- ans[[X]]$herd_size2 * input[[paste0("change_turnover",x)]]/100 * input$cull_price

ans[[X]]$inc_rev_software  <- input[[paste0("software",x)]] * ans[[X]]$herd_size2

ans[[X]]$inc_rev_total <- ans[[X]]$inc_rev_herd_size + ans[[X]]$inc_rev_per_cow + ans[[X]]$inc_rev_milk_premium +
  + ans[[X]]$inc_rev_cull_sale + ans[[X]]$inc_rev_software

ans[[X]]$dec_exp_heat_detection <- (input$hr_heat_detection - input[[paste0("anticipated_hours_heat",x)]] )*ans[[X]]$labor_rate *365

ans[[X]]$dec_exp_labor <- input[[paste0("hr_sv_milking",x)]] * ans[[X]]$labor_rate *365 

ans[[X]]$dec_exp_labor_management <- input[[paste0("decrease_lab_mgt",x)]] * input$labor_rate_rc_mgt * 365

ans[[X]]$dec_exp_total <- ans[[X]]$dec_exp_heat_detection  + ans[[X]]$dec_exp_labor + ans[[X]]$dec_exp_labor_management

ans[[X]]$positive_total <- ans[[X]]$inc_rev_total +  ans[[X]]$dec_exp_total


# Negative Impacts (year 1)
ans[[X]]$inc_exp_herd_increase <- (input[[paste0("additional_labor",x)]] + input[[paste0("additional_cost",x)]])*input[[paste0("herd_increase",x)]]


ans[[X]]$inc_exp_repair <-ans[[X]]$repair_total + input[[paste0("insurance_rate",x)]]/100 * ans[[X]]$increased_insurance


ans[[X]]$inc_exp_feed <-  ans[[X]]$DMI_change * input$cost_DM * 330 * ans[[X]]$herd_size2

ans[[X]]$inc_exp_pellet <- input[[paste0("cost_pellets",x)]] * 330 * ans[[X]]$herd_size2 * input[[paste0("pellets",x)]]/2000

ans[[X]]$inc_exp_replacement <- input$cost_heifer * input[[paste0("change_turnover",x)]]/100 * ans[[X]]$herd_size2

ans[[X]]$inc_exp_utilities <- (input[[paste0("change_electricity",x)]] + input[[paste0("change_water",x)]] +
                                 + input[[paste0("change_chemical",x)]] )* ans[[X]]$herd_size2 

ans[[X]]$inc_exp_record_management <- input[[paste0("increase_rc_mgt",x)]] * input$labor_rate_rc_mgt * 365

ans[[X]]$inc_exp_total <- ans[[X]]$inc_exp_herd_increase + ans[[X]]$inc_exp_repair + ans[[X]]$inc_exp_feed + ans[[X]]$inc_exp_pellet +
  + ans[[X]]$inc_exp_replacement +  ans[[X]]$inc_exp_utilities + ans[[X]]$inc_exp_record_management  


ans[[X]]$WACC <- ((input[[paste0("down_housing",x)]] +input[[paste0("down_milking1",x)]]) * input$hurdle_rate +
                    + (ans[[X]]$loan_housing * ans[[X]]$r_housing +
                         + ans[[X]]$loan_milking1 * ans[[X]]$r_milking1)*
                    (1-input$tax_rate/100))/(ans[[X]]$cost_housing + ans[[X]]$cost_milking1)  

ans[[X]]$avg_interest <-  (ans[[X]]$loan_housing * ans[[X]]$r_housing + 
                             + ans[[X]]$loan_milking1 * ans[[X]]$r_milking1)/
  (ans[[X]]$loan_housing + ans[[X]]$loan_milking1)  


# Calculate revenues and expenses during a delayed investment period   
if (input[[paste0("yr_system1",x)]]>0) {
  source(file.path("session_files", "session_calculation_delay.R"), local=TRUE)  # Calculates cash flow tables
} else {
  ans[[paste0(X,"_delay")]] <- list()
} 

# Calculate cash flow tables
source(file.path("session_files", "session_cash_flow.R"), local=TRUE)  


if ( input[[paste0("n_sets",x)]]=="1" & x=="Robots") browser()


ans[[X]]$capital_recovery_milking <-  -pmt(ans[[X]]$r_milking1/100, ans[[X]]$planning_horizon,
                                           npv(ans[[X]]$r_milking1/100,
                                               ans[[X]]$table_debt$milking_interest+ans[[X]]$table_debt$milking_principal))

ans[[X]]$capital_recovery_housing  <- -pmt(ans[[X]]$r_housing/100, ans[[X]]$planning_horizon,
                                           npv(ans[[X]]$r_housing/100,
                                               ans[[X]]$table_debt$barn_interest+ans[[X]]$table_debt$barn_principal))

ans[[X]]$salvage_milking_PV <-   pmt(ans[[X]]$r_milking1/100, ans[[X]]$planning_horizon,
                                     npv(ans[[X]]$r_milking1/100,
                                         ans[[X]]$table_cash_flow$salvage[-1]))
# The salvage value will be shown as negative cost

ans[[X]]$cost_downpayment <-  pmt(input$hurdle_rate/100, ans[[X]]$planning_horizon,  npv(input$hurdle_rate/100,
                                                                                         ans[[X]]$table_cash_flow$downpayment[-1]) + ans[[X]]$table_cash_flow$downpayment[1])

ans[[X]]$capital_cost_total <- ans[[X]]$capital_recovery_milking + ans[[X]]$capital_recovery_housing +
  + ans[[X]]$cost_downpayment + ans[[X]]$salvage_milking_PV

ans[[X]]$negative_total <- ans[[X]]$inc_exp_total + ans[[X]]$capital_cost_total

# Inflation adjustment for year 1
ans[[X]]$inflation_adjustment <-  - pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon,
                                        npv(ans[[X]]$avg_interest/100, ans[[X]]$table_cash_flow$revenue_minus_expense[-1])) +
  - (ans[[X]]$positive_total - ans[[X]]$negative_total + ans[[X]]$capital_cost_total)

ans[[X]]$net_annual_impact_before_tax <- ans[[X]]$positive_total - ans[[X]]$negative_total + ans[[X]]$inflation_adjustment


if (calc_type == "full") { # skip some calculations that are not needed for some use
  
  # Create copies of input$vars for Cash Flow tab etc.
  ans[[X]]$copy_salvage_milking1 <- input[[paste0("salvage_milking1",x)]]*
    (1+ans[[X]]$inflation_robot/100)^(1+input[[paste0("yr_system1",x)]]) 
  ans[[X]]$copy_salvage_milking2 <- ans[[X]]$salvage_milking_fv1
  ans[[X]]$copy_DMI_projected <-   ans[[X]]$DMI_projected
  
  var_stem <- c("copy_r", "copy_down", "copy_hr", "copy_loan", "copy_cost")
  var_ending <- c("housing","milking1","milking2")
  
  vars <- lapply(var_stem, function(stem) {
    lapply(var_ending, function(ending) {
      paste0(stem,"_",ending)
    })
  }) %>% unlist()
  
  lapply(vars, function(z) {
    if (grep("copy_r",z) %>% length() >0 | grep("copy_down",z) %>% length() >0) {
      # Assume interest rate for the second set of Milking system is the same as for the first set
      if (z=="copy_r_milking2")  ans[[X]]$copy_r_milking2 <- ans[[X]]$r_milking1
      else ans[[X]][[paste0(z)]] <- input[[paste0(sub("copy_","",z),x)]]
    }
    if (grep("copy_hr",z) %>% length() >0)  ans[[X]][[paste0(z)]] <- input$hurdle_rate
    if (grep("copy_loan",z) %>% length() >0 | grep("copy_cost",z) %>% length() >0) {
      ans[[X]][[paste0(z)]] <- ans[[X]][[sub("copy_","",z)]]
    } 
  })
  
  
  # cost of cash flows for interest payments (evaluated at separate instests for milking and housing)
  ans[[X]]$interest_at_interest <-  pmt(ans[[X]]$r_milking1/100, ans[[X]]$planning_horizon,
                                        npv(ans[[X]]$r_milking1/100, ans[[X]]$table_debt$milking_interest)) +
    + pmt(ans[[X]]$r_housing/100, ans[[X]]$planning_horizon,
          npv(ans[[X]]$r_housing/100, ans[[X]]$table_debt$barn_interest))
  
  ans[[X]]$tax_interest <-  -input$tax_rate/100 * ans[[X]]$interest_at_interest
  
  
  ans[[X]]$principal_at_interest <-  pmt(ans[[X]]$r_milking1/100, ans[[X]]$planning_horizon,
                                         npv(ans[[X]]$r_milking1/100, ans[[X]]$table_debt$milking_principal)) +
    + pmt(ans[[X]]$r_housing/100, ans[[X]]$planning_horizon,
          npv(ans[[X]]$r_housing/100, ans[[X]]$table_debt$barn_principal))
  
  # Calculations used in Parital Budget and Cash Flow 
  # cost of depreciation (evaluated at separate instests for milking and housing)
  ans[[X]]$depreciation_at_interest <-
    (pmt(ans[[X]]$r_milking1/100, ans[[X]]$planning_horizon,
         npv(ans[[X]]$r_milking1/100, ans[[X]]$table_depreciation$depreciation_milking_system)) +
       + pmt(ans[[X]]$r_housing/100, ans[[X]]$planning_horizon,
             npv(ans[[X]]$r_housing/100, ans[[X]]$table_depreciation$depreciation_housing)))
  
  ans[[X]]$tax_depreciation <- -input$tax_rate/100 * ans[[X]]$depreciation_at_interest
  
  ans[[X]]$tax_deduction_milking <-
    -input$tax_rate/100 *(pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon,
                              npv(ans[[X]]$avg_interest/100, ans[[X]]$table_depreciation$depreciation_milking_system))
                          +  pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon,
                                 npv(ans[[X]]$avg_interest/100, ans[[X]]$table_debt$milking_interest)))
  
  ans[[X]]$tax_deduction_housing <-
    -input$tax_rate/100 *(pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon,
                              npv(ans[[X]]$avg_interest/100, ans[[X]]$table_depreciation$depreciation_housing))
                          +  pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon,
                                 npv(ans[[X]]$avg_interest/100, ans[[X]]$table_debt$barn_interest)))
  
  
}