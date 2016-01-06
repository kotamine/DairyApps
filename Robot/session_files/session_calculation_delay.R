
# Delayed investment calculation replaces the forthcoming changes in three ways
# A. Partial commitment:  anticipated milk increase and labor saving 
#     This category uses the portions that are installed with the initial housing investment
#     i.e., ans[[X]]$milk_change * input[[paste0("delay_milk_increase",x)]]/100
# B. Complete commitment: SCC change, turnover rate
#     This category keeps the influence of forthcoming changes as they are
# C. Non-commitment: repair, pellets, utilities, software 
#     This category supresses the influence of forthcoming changes

# The results are stored under list: ans[[paste0(X,"_delay")]]
# The code below comes from a subset of session_calculation_steps.R
# Part unnecessary to change may be commented instead of being deleted 

ans[[paste0(X,"_delay")]]$milk_change <- ans[[X]]$milk_change * input[[paste0("delay_milk_increase",x)]]/100
ans[[paste0(X,"_delay")]]$hr_sv_milking <- input[[paste0("hr_sv_milking",x)]] * input[[paste0("delay_labor_saving",x)]]/100

ans[[paste0(X,"_delay")]]$repair <- 0 
ans[[paste0(X,"_delay")]]$pellets <- 0 
ans[[paste0(X,"_delay")]]$change_water <- 0 
ans[[paste0(X,"_delay")]]$change_electricity <- 0 
ans[[paste0(X,"_delay")]]$change_chemical <- 0 
ans[[paste0(X,"_delay")]]$software <- 0

browser()

if (x=="Robots") {
# ans[[X]]$cost_milking1 <- input[[paste0("n_robot",x)]] * ans[[X]]$cost_robot
  ans[[X]]$repair_total <- input[[paste0("n_robot",x)]] * ans[[paste0(X,"_delay")]]$repair
} else {
#   ans[[X]]$cost_milking1 <- ans[[X]]$cost_parlors
  ans[[X]]$repair_total <-  ans[[paste0(X,"_delay")]]$repair
}

# if ( input[[paste0("n_sets",x)]]=="2") {
#   ans[[X]]$n_sets <- 2
# } else  {
#   ans[[X]]$n_sets <- 1
# }

# ans[[X]]$cost_milking2 <-  ans[[X]]$cost_milking1*(1+ans[[X]]$inflation_robot/100)^
#   (ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]]) *(input[[paste0("n_sets",x)]] == 2) 
# 
# ans[[X]]$planning_horizon <- ans[[X]]$n_sets * max(ans[[X]]$useful_years, 
#                                                    input[[paste0("n_yr_milking1",x)]])+ input[[paste0("yr_system1",x)]]
# 
# 
# ans[[X]]$salvage_milking_fv1 <- input[[paste0("salvage_milking1",x)]] *
#   (1+ans[[X]]$inflation_robot/100)^(ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]])
# 
# ans[[X]]$salvage_milking_fv2 <- input[[paste0("salvage_milking1",x)]] * 
#   (1+ans[[X]]$inflation_robot/100)^(ans[[X]]$useful_years*2+ input[[paste0("yr_system1",x)]]) *
#   (input[[paste0("n_sets",x)]] >=2)
# 
# ans[[X]]$yr_system2 <- ans[[X]]$useful_years+ input[[paste0("yr_system1",x)]]
# 
# ans[[X]]$r_milking2 <- ans[[X]]$r_milking1

# Partial Budget Calculations: Data Entry side calculations
# ans[[X]]$herd_size2 <- input$herd_size + input[[paste0("herd_increase",x)]]
# 
# ans[[X]]$cost_housing <-ans[[X]]$cost_housing_cow* ans[[X]]$herd_size2
# 
# ans[[X]]$total_investment_cow <- ans[[X]]$cost_housing_cow+ ans[[X]]$cost_milking1/ans[[X]]$herd_size2
# 
# ans[[X]]$total_investment <- ans[[X]]$total_investment_cow  * ans[[X]]$herd_size2
# 
# ans[[X]]$increased_insurance <- ans[[X]]$total_investment

ans[[paste0(X,"_delay")]]$anticipated_hours_milking <- input$hours_milking - ans[[paste0(X,"_delay")]]$hr_sv_milking

# ans[[X]]$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
#   + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff


ans[[paste0(X,"_delay")]]$milk_day_cow_alt <- input$milk_cow_day + ans[[paste0(X,"_delay")]]$milk_change

ans[[paste0(X,"_delay")]]$milk_lb_alt_day <- ans[[paste0(X,"_delay")]]$milk_day_cow_alt * ans[[X]]$herd_size2/input[[paste0("n_robot",x)]]

ans[[paste0(X,"_delay")]]$adj_milk_cow_day2 <- ans[[paste0(X,"_delay")]]$milk_day_cow_alt * input$milk_cow_coeff + 
  + ans[[paste0(X,"_delay")]]$milk_day_cow_alt * input$milk_fat/100 * input$milk_fat_coeff 

# ans[[X]]$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 

ans[[paste0(X,"_delay")]]$DMI_day <-  ans[[X]]$stage_lactation * 
  (ans[[X]]$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
     +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

ans[[paste0(X,"_delay")]]$DMI_projected <-  ans[[X]]$stage_lactation *
  (ans[[paste0(X,"_delay")]]$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
     +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

ans[[paste0(X,"_delay")]]$DMI_change <- ans[[paste0(X,"_delay")]]$DMI_projected - ans[[paste0(X,"_delay")]]$DMI_day

# Cash Flow items to render in Data Entry
# ans[[X]]$salvage_housing_fv <- 0  # Currently salvage value of housing is set at zero
# 
# ans[[X]]$loan_housing <- ans[[X]]$cost_housing - input[[paste0("down_housing",x)]] +
#   -input[[paste0("delay_housing1",x)]]
# ans[[X]]$loan_milking1 <- ans[[X]]$cost_milking1 - input[[paste0("down_milking1",x)]]  +
#   +input[[paste0("delay_housing1",x)]]
# ans[[X]]$loan_milking2 <- ans[[X]]$cost_milking2 - input[[paste0("down_milking2",x)]] 
# ans[[X]]$yr_sustem2 <- ans[[X]]$useful_years

# Positive Impacts (year 1)
ans[[paste0(X,"_delay")]]$inc_rev_herd_size <- ans[[paste0(X,"_delay")]]$milk_day_cow_alt * 330 *
  (input$price_milk/100) * input[[paste0("herd_increase",x)]]

ans[[paste0(X,"_delay")]]$inc_rev_per_cow <- ans[[paste0(X,"_delay")]]$milk_change * 330 * (input$price_milk/100) * input$herd_size

ans[[paste0(X,"_delay")]]$inc_rev_milk_premium  <- ans[[paste0(X,"_delay")]]$milk_day_cow_alt *330 * input$scc_premium/100*
  (input$scc_average*(-input[[paste0("scc_change",x)]])/100)/1000 * ans[[X]]$herd_size2

ans[[paste0(X,"_delay")]]$inc_rev_cull_sale   <- ans[[X]]$herd_size2 * input[[paste0("change_turnover",x)]]/100 * input$cull_price

ans[[paste0(X,"_delay")]]$inc_rev_software  <- ans[[paste0(X,"_delay")]]$software * ans[[X]]$herd_size2

ans[[paste0(X,"_delay")]]$inc_rev_total <- ans[[paste0(X,"_delay")]]$inc_rev_herd_size + 
  + ans[[paste0(X,"_delay")]]$inc_rev_per_cow + ans[[paste0(X,"_delay")]]$inc_rev_milk_premium +
  + ans[[X]]$inc_rev_cull_sale + ans[[paste0(X,"_delay")]]$inc_rev_software

ans[[paste0(X,"_delay")]]$dec_exp_heat_detection <- 
  (input$hr_heat_detection - input[[paste0("anticipated_hours_heat",x)]] )*ans[[X]]$labor_rate *365

ans[[paste0(X,"_delay")]]$dec_exp_labor <- ans[[paste0(X,"_delay")]]$hr_sv_milking * ans[[X]]$labor_rate *365 

ans[[paste0(X,"_delay")]]$dec_exp_labor_management <- input[[paste0("decrease_lab_mgt",x)]] * input$labor_rate_rc_mgt * 365

ans[[paste0(X,"_delay")]]$dec_exp_total <- ans[[paste0(X,"_delay")]]$dec_exp_heat_detection  +
+ ans[[paste0(X,"_delay")]]$dec_exp_labor + ans[[paste0(X,"_delay")]]$dec_exp_labor_management

ans[[paste0(X,"_delay")]]$positive_total <- ans[[paste0(X,"_delay")]]$inc_rev_total +  ans[[paste0(X,"_delay")]]$dec_exp_total


# Negative Impacts (year 1)
ans[[paste0(X,"_delay")]]$inc_exp_herd_increase <- (input[[paste0("additional_labor",x)]] + input[[paste0("additional_cost",x)]])*input[[paste0("herd_increase",x)]]


ans[[paste0(X,"_delay")]]$inc_exp_repair <- ans[[paste0(X,"_delay")]]$repair_total + 
  + input[[paste0("insurance_rate",x)]]/100 * ans[[X]]$increased_insurance

ans[[paste0(X,"_delay")]]$inc_exp_feed <-  ans[[paste0(X,"_delay")]]$DMI_change * input$cost_DM * 330 * ans[[X]]$herd_size2

ans[[paste0(X,"_delay")]]$inc_exp_pellet <- input[[paste0("cost_pellets",x)]] *
  330 * ans[[X]]$herd_size2 * ans[[paste0(X,"_delay")]]$pellets/2000

ans[[paste0(X,"_delay")]]$inc_exp_replacement <- input$cost_heifer * input[[paste0("change_turnover",x)]]/100 * ans[[X]]$herd_size2

ans[[paste0(X,"_delay")]]$inc_exp_utilities <- (ans[[paste0(X,"_delay")]]$change_electricity + ans[[paste0(X,"_delay")]]$change_water +
                                 + ans[[paste0(X,"_delay")]]$change_chemical)* ans[[X]]$herd_size2 

ans[[paste0(X,"_delay")]]$inc_exp_record_management <- input[[paste0("increase_rc_mgt",x)]] * input$labor_rate_rc_mgt * 365

ans[[paste0(X,"_delay")]]$inc_exp_total <- ans[[X]]$inc_exp_herd_increase + ans[[paste0(X,"_delay")]]$inc_exp_repair +
  + ans[[paste0(X,"_delay")]]$inc_exp_feed + ans[[paste0(X,"_delay")]]$inc_exp_pellet +
  + ans[[paste0(X,"_delay")]]$inc_exp_replacement +  ans[[paste0(X,"_delay")]]$inc_exp_utilities +
  + ans[[paste0(X,"_delay")]]$inc_exp_record_management  


