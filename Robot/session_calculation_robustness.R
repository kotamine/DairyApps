
## --- For robustoness analysis we will calculate almost everything  all over again ---
## - Technically we don't need to store all varibles under "rb" 
## - but it makes it easier to retrieve them later. 

isolate({ # isolate the robustness calculation  
  
 # Initialize changed values 
  rb$cost_robot <- input$cost_robot
  rb$cost_housing_cow <- input$cost_housing_cow
  rb$repair <- input$repair
  rb$robot_years <- input$robot_years
  rb$salvage_robot <- input$salvage_robot
  rb$hr_sv_milking <- input$hr_sv_milking
  rb$milk_change <- input$milk_change
  rb$scc_change <- input$scc_change
  rb$pellets <- input$pellets 
  
  
if (robust=="Sensitivity") {
  
# Change the selected value by c_val (value)
rb$cost_robot <- input$cost_robot*(1+ (c_choice=="c1")*c_val/100)
rb$cost_housing_cow <- input$cost_housing_cow*(1+ (c_choice=="c2")*c_val/100)
rb$repair <- input$repair*(1+ (c_choice=="c3")*c_val/100)
rb$robot_years <- input$robot_years*(1+ (c_choice=="c4")*c_val/100)
rb$salvage_robot <- input$salvage_robot*(1+ (c_choice=="c5")*c_val/100)
rb$hr_sv_milking <- input$hr_sv_milking*(1+ (c_choice=="c6")*c_val/100)
rb$milk_change <- input$milk_change*(1+ (c_choice=="c7")*c_val/100)

}

if (robust=="Scenarios") {
  
  # Change the selected value by s_val (array)
  rb$cost_robot <- new_val[1]
  rb$cost_housing_cow <- new_val[2]
  rb$milk_change <- new_val[3]
  rb$scc_change < - new_val[4]
  rb$pellets <- new_val[5] 
  
}

# Data Entry Level Calculations
rb$herd_size2 <- input$herd_size + input$herd_increase

rb$robot_invest <- input$n_robot * rb$cost_robot

rb$cost_housing <- rb$cost_housing_cow * rb$herd_size2

rb$total_investment_cow <-  rb$cost_housing_cow + rb$robot_invest/rb$herd_size2

rb$total_investment <- rb$total_investment_cow  * rb$herd_size2

rb$housing_years <- input$n_robot_life * rb$robot_years

rb$increased_insurance <- rb$total_investment

rb$anticipated_hours_milking <- input$hours_milking - rb$hr_sv_milking

rb$milk_day_cow_robot <- input$milk_cow_day + rb$milk_change

rb$milk_lb_robot_day <- rb$milk_day_cow_robot * rb$herd_size2/input$n_robot 

rb$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
  + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff

rb$adj_milk_cow_day2 <- rb$milk_day_cow_robot * input$milk_cow_coeff + 
  + rb$milk_day_cow_robot * input$milk_fat/100 * input$milk_fat_coeff 

rb$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 

rb$DMI_day <-  rb$stage_lactation * (rb$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                       +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

rb$DMI_projected <-  rb$stage_lactation * (rb$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                                             +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

rb$DMI_change <- rb$DMI_projected - rb$DMI_day

rb$adj_milk_cow_day2 <- rb$milk_day_cow_robot * input$milk_cow_coeff + 
  + rb$milk_day_cow_robot  * input$milk_fat/100 * input$milk_fat_coeff 

rb$IOFC <- (input$milk_cow_day * input$price_milk/100 - rb$DMI_day * input$cost_DM )*330

rb$IOFC2 <- (rb$milk_day_cow_robot * input$price_milk/100 + 
               - rb$DMI_projected * input$cost_DM - rb$pellets * input$cost_pellets/2000)*330 

rb$IOFC_cwt <- rb$IOFC /365 /input$milk_cow_day * 330

rb$IOFC2_cwt <- rb$IOFC2 /365 /rb$milk_day_cow_robot * 330


# Positive Impacts
rb$inc_rev_herd_size <- rb$milk_day_cow_robot * 330 *
  (input$price_milk/100) * input$herd_increase

rb$inc_rev_per_cow <- rb$milk_change * 330 * (input$price_milk/100) * input$herd_size

rb$inc_rev_milk_premium  <- rb$milk_day_cow_robot *330 * input$scc_premium/100*
  (input$scc_average*(-rb$scc_change)/100)/1000 * rb$herd_size2

rb$inc_rev_cull_sale   <- rb$herd_size2 * input$change_turnover/100 * input$cull_price

rb$inc_rev_software  <- input$software * rb$herd_size2

rb$inc_rev_total <- rb$inc_rev_herd_size + rb$inc_rev_per_cow + rb$inc_rev_milk_premium +
  + rb$inc_rev_cull_sale + rb$inc_rev_software

rb$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365

rb$dec_exp_labor <- rb$hr_sv_milking * input$labor_rate *365 

rb$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365

rb$dec_exp_total <- rb$dec_exp_heat_detection  + rb$dec_exp_labor + rb$dec_exp_labor_management

rb$positive_total <- rb$inc_rev_total +  rb$dec_exp_total


# Negative Impacts
rb$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase

rb$inc_exp_repair <-rb$repair * input$n_robot + input$insurance_rate/100 * rb$increased_insurance

rb$inc_exp_feed <-  rb$DMI_change * input$cost_DM * 330 * rb$herd_size2

rb$inc_exp_pellet <- input$cost_pellets * 330 * rb$herd_size2 * rb$pellets/2000

rb$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rb$herd_size2

rb$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rb$herd_size2

rb$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365

if (is.na(input$n_robot_life) | is.na(input$interest) | 
    is.na(rb$housing_years) | is.na(rb$robot_invest) | is.na(input$inflation_robot) |
    is.na(rb$robot_years)) {
  tmp <- NA
} else { 
  if (input$n_robot_life > 1) {
    tmp <-  - pmt(input$interest/100, rb$housing_years, 
                  rb$robot_invest*(1 + input$inflation_robot/100)^rb$robot_years/
                    (1 + input$interest/100)^(rb$robot_years))   
  } else {
    tmp <- 0
  }
}

rb$inc_exp_capital_recovery <-   - pmt(input$interest/100, rb$housing_years, rb$robot_invest) + tmp

rb$inc_exp_total <- rb$inc_exp_herd_increase + rb$inc_exp_repair + rb$inc_exp_feed + rb$inc_exp_pellet +
  + rb$inc_exp_replacement +  rb$inc_exp_utilities + rb$inc_exp_record_management + rb$inc_exp_capital_recovery

rb$negative_total  <-  rb$inc_exp_total

rb$impact_without_housing <-  rb$positive_total - rb$negative_total

rb$capital_recovery_housing  <- - pmt(input$interest/100, rb$housing_years, rb$cost_housing)

rb$capital_recovery_total <- rb$inc_exp_capital_recovery + rb$capital_recovery_housing

rb$impact_with_housing <- rb$impact_without_housing - rb$capital_recovery_housing

rb$robot_end_PV <- -pmt(input$interest/100, rb$housing_years, 
                        rb$salvage_robot/(1 + input$interest/100)^rb$housing_years)

rb$impact_with_robot_salvage <- rb$impact_with_housing + rb$robot_end_PV

rb$impact_with_inflation  <- "Depends on cash flow"

# others for display in the dashboard

# all non-reactive to base input changes and input$NAI 
if (input$NAI=="w/o housing") {
  rb$NAI <- rb$impact_without_housing
} else if (input$NAI=="w/ housing") {
  rb$NAI <- rb$impact_with_housing
} else {
  rb$NAI <- rb$impact_with_robot_salvage
}

if(input$NAI=="w/o housing") {
  rb$capital_cost <- -rb$inc_exp_capital_recovery
} else if (input$NAI=="w/ housing") {
  rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing)
} else {
  rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing) +
    + rb$robot_end_PV
} 


rb$milk_current <- 
  input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                  +  input$scc_premium/100 * input$scc_average/1000) 

rb$milk_robot <-  rb$herd_size2 * 330 * rb$milk_day_cow_robot *
  (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 

rb$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365


rb$labor_robot <- (input$anticipated_hours_heat + rb$anticipated_hours_milking) * input$labor_rate *365 + 
  + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
  + input$additional_labor * input$herd_increase  

rb$feed_current <-  rb$DMI_day * input$cost_DM * 330 * input$herd_size 

rb$feed_robot <- (rb$DMI_projected * input$cost_DM + input$pellets *
                    input$cost_pellets/2000) * 330 * rb$herd_size2

rb$milk_feed <-  -(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current 

rb$labor_repair <- -(rb$labor_robot - rb$labor_current + rb$inc_exp_repair)

rb$misc <- rb$NAI - (rb$milk_feed + rb$labor_repair + rb$capital_cost)


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

# rb$new_row <- new_row 
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
  
  # rb$new_row <- new_row
}
  



})


