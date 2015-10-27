# There are three ways the sensitivity analysis can be triggered. 
# 1. change of variable to consider the sensitivity 
# 2. change of value for that variable 
# 3. triger of "calculate" button after a change of any input value 

# ----------- Sensitivity Analysis -----------
rb$c_values <- c(20, 20, 50, 50, -50, -50, 50)

rb$colnames <- c("variable", "% change","base value","new value",
                 "net impact w/o housing","change: impact w/o housing", 
                 "net impact w/ housing","change: impact w/ housing",
                 "net impact w/ salvage", "change: impact w/ salvage",
                 "IFOC gain cow/year", "change: IOFC gain cow",
                 "IFOC gain cwt", "change: IOFC gain cwt",
                 "milk - feed", "change: milk - feed",
                 "labor + repair", "change: labor + repair",
                 "capital cost", "change: capital cost",
                 "the rest", "change: the rest")

rb$order1 <- c("variable", "% change","base value","new value",
               "net impact w/ salvage", "change: impact w/ salvage")

rb$table_sensitivity <- data.frame(Column1 = numeric(0)) # creating an emptry table
# rb$c_choice_val <- c("c1","c2","c3","c4","c5","c6","c7")
rb$c_varnames <- c("cost_robot","cost_housing_cow", "repair",
                   "robot_years","salvage_robot", "hr_sv_milking",
                   "milk_change")
rb$c_labels <- c("Estimated cost per robot", "Related housing changes needed per cow",
                  "Estimated annual change in milking system repair",
                  "Robots: years of useful life",
                  "Value of the robots after useful life",
                   "Anticipated savings in milking & chore labor",
                    "Projected change in milk production") 


observeEvent(input$c_choice, {
#   if (input$c_choice=="c1") {
#     updateNumericInput(session,"c_val",NULL, value=rb$c_values[1], step=10)
#   } 
#   else  if (input$c_choice=="c2") {
#     updateNumericInput(session,"c_val",NULL, value=rb$c_values[2], step=10)
#   }
#   else  if (input$c_choice=="c3") {
#     updateNumericInput(session,"c_val",NULL, value=rb$c_values[3], step=10)
#   }
#   else  if (input$c_choice=="c4") {
#     updateNumericInput(session,"c_val",NULL, value=rb$c_values[4], step=10)
#   }
#   else  if (input$c_choice=="c5") {
#     updateNumericInput(session,"c_val",NULL, value= rb$c_values[5], step=10)
#   }
#   else  if (input$c_choice=="c6") {
#     updateNumericInput(session,"c_val",NULL, value= rb$c_values[6], step=10)
#   }
#   else  if (input$c_choice=="c7") {
#     updateNumericInput(session,"c_val",NULL, value=rb$c_values[7], step=10)
#   }
  
#   updateNumericInput(session,"c_val",NULL, 
#                      value=rb$c_values[as.integer(gsub("c","", input$c_choice))], step=10)
#  
  
  # This trigger simply renders sensitivity choice to UI 
   n <- as.integer(gsub("c","", input$c_choice))
   rb$c_val <- rb$c_values[n]
   rb$value <- paste(rb$c_varnames[n])
   rb$new_value <- (rb$value * (1 + rb$c_val/100))
   rb$var <- rb$c_lables[n]
   
})

output$c_text <- renderUI({
  
  if (input$c_choice=="c1") {
    val0 <-  rb$value %>% formatdollar()
    val1 <-  rb$new_value %>% formatdollar()
    unit <- ""
  } else if (input$c_choice=="c2") { 
    val0 <-  rb$value %>% formatdollar()
    val1 <-  rb$new_value %>% formatdollar()
    unit <- ""
  } else if (input$c_choice=="c3") {
    val0 <-  rb$value %>% formatdollar()
    val1 <-  rb$new_value %>% formatdollar()
    unit <- ""
  } else if (input$c_choice=="c4") {
    val0 <-  rb$value  %>% round(2)
    val1 <-  rb$new_value  %>% round(2)
    unit <- "years"
  } else if (input$c_choice=="c5") {
    val0 <-  rb$value %>% formatdollar()
    val1 <-  rb$new_value %>% formatdollar()
    unit <- ""
  } else if (input$c_choice=="c6") {
    val0 <-  rb$value %>% round(2)
    val1 <-  rb$new_value  %>% round(2)
    unit <- "hrs/day"
  } else if (input$c_choice=="c7") {
    val0 <-  rb$value %>% round(2)
    val1 <-  rb$new_value  %>% round(2)
    unit <- "lb/cow/day"
  }
  
  paste("from", val0," to ", val1, unit) %>% h5()
})

# Update table_sensitivity when input$c_val is changed 
observeEvent(input$c_val, { 
  
  n <- as.integer(gsub("c","", input$c_choice))
  rb$c_values[n] <- input$c_val  # update the value for % change 
  
  rb$c_choice <- input$c_choice
  rb$c_val <- rb$c_values[n]
  
  rb$value <- paste(rb$c_varnames[n])
  rb$new_value <- (rb$value * (1 + rb$c_val/100))
  rb$var <- rb$c_lables[n]
  
  isolate(
    source("calculation_sensitivity.R", local=TRUE)
  )
})


# Recalculate all rows of table_sensitivity 
observeEvent(input$sensitivity_calculate, {
  
  rb$table_sensitivity <- data.frame(Column1 = numeric(0)) # creating an emptry table
  
  # replace "n" in the previous case with "x" that goes from 1 to 7 
  lapply(c(1:7), 
         function(x) { 
#     updateSelectInput(session, "c_choice", NULL,
#                       choices = rb$c_choice_val, selected = pasete0("'c",x,"'")) 
           # updateNumericInput(session,"c_val",NULL, value=rb$c_values[x], step=10)
           
             rb$c_choice <- paste0("c",x) 
             rb$c_val <- rb$c_values[x]
             
             rb$value <- paste(rb$c_varnames[x])
             rb$new_value <- (rb$value * (1 + rb$c_val/100))
             rb$var <- rb$c_lables[x]
             
             source("calculation_sensitivity.R", local=TRUE)
         }
  )
  
})




#   # Initialize changed values 
#   rb$cost_robot <- input$cost_robot
#   rb$cost_housing_cow <- input$cost_housing_cow
#   rb$repair <- input$repair
#   rb$robot_years <- input$robot_years
#   rb$salvage_robot <- input$salvage_robot
#   rb$hr_sv_milking <- input$hr_sv_milking
#   rb$milk_change <- input$milk_change
#   
#   # Change the selected value by input$c_val
#   rb$cost_robot <- input$cost_robot*(1+ (input$c_choice=="c1")*input$c_val/100)
#   rb$cost_housing_cow <- input$cost_housing_cow*(1+ (input$c_choice=="c2")*input$c_val/100)
#   rb$repair <- input$repair*(1+ (input$c_choice=="c3")*input$c_val/100)
#   rb$robot_years <- input$robot_years*(1+ (input$c_choice=="c4")*input$c_val/100)
#   rb$salvage_robot <- input$salvage_robot*(1+ (input$c_choice=="c5")*input$c_val/100)
#   rb$hr_sv_milking <- input$hr_sv_milking*(1+ (input$c_choice=="c6")*input$c_val/100)
#   rb$milk_change <- input$milk_change*(1+ (input$c_choice=="c7")*input$c_val/100)
#   
#   
#   ## --- For robustoness analysis we will calculate almost everything  all over again ---
#   # Techincally, we don't need to store all calclation results, but we store them all under "rb".  
#   # Data Entry Level Calculations
#   rb$herd_size2 <- input$herd_size + input$herd_increase
#   
#   rb$robot_invest <- input$n_robot * rb$cost_robot
#   
#   rb$cost_housing <- rb$cost_housing_cow * rb$herd_size2
#   
#   rb$total_investment_cow <-  rb$cost_housing_cow + rb$robot_invest/rb$herd_size2
#   
#   rb$total_investment <- rb$total_investment_cow  * rb$herd_size2
#   
#   rb$housing_years <- input$n_robot_life * rb$robot_years
#   
#   rb$increased_insurance <- rb$total_investment
#   
#   rb$anticipated_hours_milking <- input$hours_milking - rb$hr_sv_milking
#   
#   rb$milk_lb_robot_day <- (input$milk_cow_day + rb$milk_change) * rb$herd_size2/input$n_robot 
#   
#   rb$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
#     + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
#   
#   rb$adj_milk_cow_day2 <- (input$milk_cow_day + rb$milk_change) * input$milk_cow_coeff + 
#     + (input$milk_cow_day + rb$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
#   
#   rb$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
#   
#   rb$DMI_day <-  rb$stage_lactation * (rb$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
#                                          +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
#   
#   rb$DMI_projected <-  rb$stage_lactation * (rb$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
#                                                +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
#   
#   rb$DMI_change <- rb$DMI_projected - rb$DMI_day
#   
#   rb$adj_milk_cow_day2 <- (input$milk_cow_day +  rb$milk_change) * input$milk_cow_coeff + 
#     + (input$milk_cow_day + rb$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
#   
#   rb$IOFC <- (input$milk_cow_day * input$price_milk/100 - rb$DMI_day * input$cost_DM )*330
#   
#   rb$IOFC2 <- ((input$milk_cow_day +  rb$milk_change) * input$price_milk/100 + 
#                  - rb$DMI_projected * input$cost_DM - input$pellets * input$cost_pellets/2000)*330 
#   
#   rb$IOFC_cwt <- rb$IOFC /365 /input$milk_cow_day * 330
#   
#   rb$IOFC2_cwt <- rb$IOFC2 /365 /(input$milk_cow_day + rb$milk_change) * 330
#   
#   
#   
#   # Positive Impacts
#   rb$inc_rev_herd_size <- (input$milk_cow_day + rb$milk_change) * 330 *
#     (input$price_milk/100) * input$herd_increase
#   
#   rb$inc_rev_per_cow <- rb$milk_change * 330 * (input$price_milk/100) * input$herd_size
#   
#   rb$inc_rev_milk_premium  <- (input$milk_cow_day + rb$milk_change )*330 * input$scc_premium/100*
#     (input$scc_average*(-input$scc_change)/100)/1000 * rb$herd_size2
#   
#   rb$inc_rev_cull_sale   <- rb$herd_size2 * input$change_turnover/100 * input$cull_price
#   
#   rb$inc_rev_software  <- input$software * rb$herd_size2
#   
#   rb$inc_rev_total <- rb$inc_rev_herd_size + rb$inc_rev_per_cow + rb$inc_rev_milk_premium +
#     + rb$inc_rev_cull_sale + rb$inc_rev_software
#   
#   rb$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
#   
#   rb$dec_exp_labor <- rb$hr_sv_milking * input$labor_rate *365 
#   
#   rb$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
#   
#   rb$dec_exp_total <- rb$dec_exp_heat_detection  + rb$dec_exp_labor + rb$dec_exp_labor_management
#   
#   rb$positive_total <- rb$inc_rev_total +  rb$dec_exp_total
#   
#   # Negative Impacts
#   rb$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase
#   
#   rb$inc_exp_repair <-rb$repair * input$n_robot + input$insurance_rate/100 * rb$increased_insurance
#   
#   rb$inc_exp_feed <-  rb$DMI_change * input$cost_DM * 330 * rb$herd_size2
#   
#   rb$inc_exp_pellet <- input$cost_pellets * 330 * rb$herd_size2 * input$pellets/2000
#   
#   rb$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rb$herd_size2
#   
#   rb$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rb$herd_size2
#   
#   rb$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
#   
#   if (is.na(input$n_robot_life) | is.na(input$interest) | 
#       is.na(rb$housing_years) | is.na(rb$robot_invest) | is.na(input$inflation_robot) |
#       is.na(rb$robot_years)) {
#     tmp <- NA
#   }
#   else {
#     if (input$n_robot_life > 1) {
#       tmp <-  - pmt(input$interest/100, rb$housing_years, 
#                     rb$robot_invest*(1 + input$inflation_robot/100)^rb$robot_years/
#                       (1 + input$interest/100)^(rb$robot_years))   
#     } 
#     else {
#       tmp <- 0
#     }
#   }
#   rb$inc_exp_capital_recovery <-   - pmt(input$interest/100, rb$housing_years, rb$robot_invest) + tmp
#   
#   rb$inc_exp_total <- rb$inc_exp_herd_increase + rb$inc_exp_repair + rb$inc_exp_feed + rb$inc_exp_pellet +
#     + rb$inc_exp_replacement +  rb$inc_exp_utilities + rb$inc_exp_record_management + rb$inc_exp_capital_recovery
#   
#   rb$negative_total  <-  rb$inc_exp_total
#   
#   rb$impact_without_housing <-  rb$positive_total - rb$negative_total
#   
#   rb$capital_recovery_housing  <- - pmt(input$interest/100, rb$housing_years, rb$cost_housing)
#   
#   rb$capital_recovery_total <- rb$inc_exp_capital_recovery + rb$capital_recovery_housing
#   
#   rb$impact_with_housing <- rb$impact_without_housing - rb$capital_recovery_housing
#   
#   rb$robot_end_PV <- -pmt(input$interest/100, rb$housing_years, 
#                           rb$salvage_robot/(1 + input$interest/100)^rb$housing_years)
#   
#   rb$impact_with_robot_salvage <- rb$impact_with_housing + rb$robot_end_PV
#   
#   rb$impact_with_inflation  <- "Depends on cash flow"
#   
#   # others for display in the dashboard
#   rb$milk_current <- 
#     input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
#                                                     +  input$scc_premium/100 * input$scc_average/1000) 
#   
#   rb$milk_robot <-  rb$herd_size2 * 330 * (input$milk_cow_day + rb$milk_change) *
#     (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 
#   
#   rb$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365
#   
#   
#   rb$labor_robot <- (input$anticipated_hours_heat + rb$anticipated_hours_milking) * input$labor_rate *365 + 
#     + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
#     + input$additional_labor * input$herd_increase  
#   
#   rb$feed_current <-  rb$DMI_day * input$cost_DM * 330 * input$herd_size 
#   
#   rb$feed_robot <- (rb$DMI_projected * input$cost_DM + input$pellets *
#                       input$cost_pellets/2000) * 330 * rb$herd_size2
# 
#     # rb$new_row <- TRUE
# })

