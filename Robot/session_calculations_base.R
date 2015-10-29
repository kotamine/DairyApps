
## --- Rewritten version of session_variables.R,  session_variables.R, and session_budget.R --- 
## --- This file deals with most of the calculations for the base analysis 

# This sets the default value for additional_labor and additional_cost when hidden from the user
updateNumericInput(session, "additional_labor",NULL,value=450,step=50,min=0)
updateNumericInput(session, "additional_cost",NULL,value=200,step=50,min=0)


# Show/hide DMI calculations 
shinyjs::onclick("customDMI",
                 shinyjs::toggle(id="DMI_inputs", anim = TRUE)
)

#   observe({
#     toggle(id="DMI_inputs", condition = input$customDMI, anim = TRUE)
#   })

observeEvent(input$coeff_reset,{ 
  updateNumericInput(session, "milk_cow_coeff",NULL,value=0.4,min=0,step=0.1)
  updateNumericInput(session, "milk_fat",NULL,value=3.65,min=0,step=0.2)
  updateNumericInput(session, "milk_fat_coeff",NULL,value=15,min=0,step=0.5)
  updateNumericInput(session, "adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.1)
  updateNumericInput(session, "body_weight_coeff1",NULL,value=0.0968,min=0,step=0.005)
  updateNumericInput(session, "body_weight_coeff2",NULL,value=0.75,min=0,step=0.05)
  updateNumericInput(session, "lactation_coeff1",NULL,value=-0.192,step=0.01)
  updateNumericInput(session, "lactation_coeff2",NULL,value=3.67,min=0,step=0.05)
}) 


# Enable calculation button in Economic Analysis when Data Entry tabs are viewed by the user 
observe(
  if (input$budget==0) {
    if(!is.null(rv$DMI_change) & !is.null(rv$DMI_day) & 
       !is.null(rv$herd_size2) & !is.null(rv$increased_insurance) &
       !is.null(rv$anticipated_hours_milking) & !is.null(rv$milk_lb_robot_day)
    ) {
      updateButton(session, "budget", disabled = FALSE, style = "primary", icon = "")
    } 
    else {
      return()
    }
  } else {
    return()
  }
)



## ----------- Main Calculations: all restuls are stored under reactive value "rv" -----------
observe({

# Data Entry Level Calculations
rv$herd_size2 <- input$herd_size + input$herd_increase

rv$robot_invest <- input$n_robot * input$cost_robot

rv$cost_housing <- input$cost_housing_cow * rv$herd_size2

rv$total_investment_cow <-  input$cost_housing_cow + rv$robot_invest/rv$herd_size2

rv$total_investment <- rv$total_investment_cow  * rv$herd_size2

rv$housing_years <- input$n_robot_life * input$robot_years

rv$increased_insurance <- rv$total_investment

rv$anticipated_hours_milking <- input$hours_milking - input$hr_sv_milking

rv$milk_day_cow_robot <- input$milk_cow_day + input$milk_change

rv$milk_lb_robot_day <- rv$milk_day_cow_robot * rv$herd_size2/input$n_robot 

rv$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
  + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff

rv$adj_milk_cow_day2 <- rv$milk_day_cow_robot * input$milk_cow_coeff + 
  + rv$milk_day_cow_robot * input$milk_fat/100 * input$milk_fat_coeff 

rv$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 

rv$DMI_day <-  rv$stage_lactation * (rv$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                                       +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

rv$DMI_projected <-  rv$stage_lactation * (rv$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                                             +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor

rv$DMI_change <- rv$DMI_projected - rv$DMI_day

rv$adj_milk_cow_day2 <- rv$milk_day_cow_robot * input$milk_cow_coeff + 
  + rv$milk_day_cow_robot  * input$milk_fat/100 * input$milk_fat_coeff 

rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - rv$DMI_day * input$cost_DM )*330

rv$IOFC2 <- (rv$milk_day_cow_robot * input$price_milk/100 + 
               - rv$DMI_projected * input$cost_DM - input$pellets * input$cost_pellets/2000)*330 

rv$IOFC_cwt <- rv$IOFC /365 /input$milk_cow_day * 330

rv$IOFC2_cwt <- rv$IOFC2 /365 /rv$milk_day_cow_robot * 330


# Positive Impacts
rv$inc_rev_herd_size <- rv$milk_day_cow_robot * 330 *
  (input$price_milk/100) * input$herd_increase

rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * input$herd_size

rv$inc_rev_milk_premium  <- rv$milk_day_cow_robot *330 * input$scc_premium/100*
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


# Negative Impacts
rv$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase

rv$inc_exp_repair <-input$repair * input$n_robot + input$insurance_rate/100 * rv$increased_insurance

rv$inc_exp_feed <-  rv$DMI_change * input$cost_DM * 330 * rv$herd_size2

rv$inc_exp_pellet <- input$cost_pellets * 330 * rv$herd_size2 * input$pellets/2000

rv$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rv$herd_size2

rv$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rv$herd_size2

rv$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365

if (is.na(input$n_robot_life) | is.na(input$interest) | 
    is.na(rv$housing_years) | is.na(rv$robot_invest) | is.na(input$inflation_robot) |
    is.na(input$robot_years)) {
  tmp <- NA
} else { 
  if (input$n_robot_life > 1) {
    tmp <-  - pmt(input$interest/100, rv$housing_years, 
                  rv$robot_invest*(1 + input$inflation_robot/100)^input$robot_years/
                    (1 + input$interest/100)^(input$robot_years))   
  } else {
    tmp <- 0
  }
}

rv$inc_exp_capital_recovery <-   - pmt(input$interest/100, rv$housing_years, rv$robot_invest) + tmp

rv$inc_exp_total <- rv$inc_exp_herd_increase + rv$inc_exp_repair + rv$inc_exp_feed + rv$inc_exp_pellet +
  + rv$inc_exp_replacement +  rv$inc_exp_utilities + rv$inc_exp_record_management + rv$inc_exp_capital_recovery

rv$negative_total  <-  rv$inc_exp_total

## Net Impact
rv$impact_without_housing <-  rv$positive_total - rv$negative_total

rv$capital_recovery_housing  <- - pmt(input$interest/100, rv$housing_years, rv$cost_housing)

rv$capital_recovery_total <- rv$inc_exp_capital_recovery + rv$capital_recovery_housing

rv$impact_with_housing <- rv$impact_without_housing - rv$capital_recovery_housing

rv$robot_end_PV <- -pmt(input$interest/100, rv$housing_years, 
                        input$salvage_robot/(1 + input$interest/100)^rv$housing_years)

rv$impact_with_robot_salvage <- rv$impact_with_housing + rv$robot_end_PV

rv$impact_with_inflation  <- "Depends on cash flow"



#  Dashboard
if (input$NAI=="w/o housing") {
  rv$NAI <- rv$impact_without_housing
} else if (input$NAI=="w/ housing") {
  rv$NAI <- rv$impact_with_housing
} else {
  rv$NAI <- rv$impact_with_robot_salvage
}

if(input$NAI=="w/o housing") {
  rv$capital_cost <- -rv$inc_exp_capital_recovery
} else if (input$NAI=="w/ housing") {
  rv$capital_cost <- -(rv$inc_exp_capital_recovery + rv$capital_recovery_housing)
} else {
  rv$capital_cost <- -(rv$inc_exp_capital_recovery + rv$capital_recovery_housing) +
    + rv$robot_end_PV
} 


rv$milk_current <- 
  input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                  +  input$scc_premium/100 * input$scc_average/1000) 

rv$milk_robot <-  rv$herd_size2 * 330 * rv$milk_day_cow_robot *
  (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 

rv$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365


rv$labor_robot <- (input$anticipated_hours_heat + rv$anticipated_hours_milking) * input$labor_rate *365 + 
  + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
  + input$additional_labor * input$herd_increase  

rv$feed_current <-  rv$DMI_day * input$cost_DM * 330 * input$herd_size 

rv$feed_robot <- (rv$DMI_projected * input$cost_DM + input$pellets *
                    input$cost_pellets/2000) * 330 * rv$herd_size2

rv$milk_feed <-  -(rv$feed_robot - rv$feed_current) + rv$milk_robot -  rv$milk_current 

rv$labor_repair <- -(rv$labor_robot - rv$labor_current + rv$inc_exp_repair)

rv$misc <- rv$NAI - (rv$milk_feed + rv$labor_repair + rv$capital_cost)

# This is used later for alerting base value change in robustness analysis  
createAlert(session, "c_input_change", "ref_c_input_change", 
            content = "New base values. 
            Press [Calculate] to updated the results.",
            append = FALSE) 

createAlert(session, "s_input_change", "ref_s_input_change", 
            content = "New base values. 
            Press [Calculate] to updated the results.",
            append = FALSE) 

})



