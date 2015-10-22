library(shiny)
library(shinyBS)
library(shinyjs)
suppressPackageStartupMessages(library(dplyr))
# library(magrittr)

source("helper.R")


shinyServer(function(input, output, session) {


  # ---------- Fill out the calclated values in Data Entry ----------
  # Create a list of reactive values 
  rv <- reactiveValues(a=NULL)
  conv_factor <- 2.2046 

  # --- Farm Finaces ---
  output$herd_size2 <- renderUI({
    rv$herd_size2 <- input$herd_size + input$herd_increase
    rv$herd_size2 %>% formatcomma() %>% helpText()
  })
  
  output$robot_invest <- renderUI({
    rv$robot_invest <- input$n_robot * input$cost_robot
    rv$robot_invest %>% formatcomma() %>% helpText() 
  })
  
  output$cost_housing <- renderUI({
    rv$related_changes <- input$cost_housing_cow * rv$herd_size2
    rv$related_changes %>% formatcomma() %>% helpText() 
  })
  
  output$total_investment <- renderUI({
    rv$total_investment <- rv$total_investment_cow  * rv$herd_size2
    rv$total_investment  %>% formatcomma()  %>% helpText() 
  })
  
  output$total_investment_cow <- renderUI({
    rv$total_investment_cow <-  input$cost_housing_cow + rv$robot_invest/rv$herd_size2
    rv$total_investment_cow %>% formatcomma() %>% helpText() 
  })
  
  # --- Maintenance ---
  output$housing_years <- renderUI({
    rv$housing_years <- input$n_robot_life * input$robot_years
    rv$housing_years  %>% formatcomma() %>% helpText() 
  })
  
  output$increased_insurance <- renderUI({
    rv$increased_insurance <- rv$total_investment
    rv$increased_insurance  %>% formatcomma() %>% helpText() 
  })
  
  # --- Labor Savings ---
  output$anticipated_hours_milking <- renderUI({
    rv$anticipated_hours_milking <- input$hours_milking - input$hr_sv_milking
    rv$anticipated_hours_milking  %>% formatcomma()  %>% helpText() 
  })

  # --- Milk Outputs ---
  output$milk_lb_robot_day <- renderUI({
    rv$milk_lb_robot_day <- (input$milk_cow_day + input$milk_change) * rv$herd_size2/input$n_robot 
    rv$milk_lb_robot_day  %>% formatcomma() %>% helpText() 
  })
  
  # --- Feed --- 
#   rv$DMI_change <- reactive({ 
#      return(rv$DMI_projected - rv$DMI_day) 
#   })

  output$DMI_change <- renderUI({
    rv$DMI_change <- rv$DMI_projected - rv$DMI_day 
    rv$DMI_change  %>% round(3) %>% helpText()
  })
  
  shinyjs::onclick("customDMI",
                  shinyjs::toggle(id="DMI_inputs", anim = TRUE)
                  )
# 
#   observe({
#     if (is.null(rv$DMI_change))
#       browser()
#       rv$DMI <- 3.507
#   })  
  
  output$rep_milk_cow_day <- renderUI({
   input$milk_cow_day %>% formatcomma() %>% helpText() 
  })
  
  output$rep_milk_change <- renderUI({
    input$milk_change  %>% formatcomma() %>% helpText() 
  })
  
  output$DMI_change_copy <- renderUI({
    rv$DMI_change %>% round(3) %>% helpText()
  })
  
  output$adj_milk_cow_day <- renderUI({
    rv$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
    rv$adj_milk_cow_day2 <- (input$milk_cow_day + input$milk_change) * input$milk_cow_coeff + 
      + (input$milk_cow_day + input$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
    rv$adj_milk_cow_day  %>% round(2) %>% helpText()
  })
  
  output$stage_lactation <- renderUI({
    rv$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
    rv$stage_lactation  %>% round(4) %>% helpText()
  })
  
  output$DMI_day <- renderUI({
    rv$DMI_day <-  rv$stage_lactation * (rv$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
      +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    rv$DMI_day  %>% round(3) %>% helpText()
  })
  
  output$DMI_projected <- renderUI({
    rv$DMI_projected <-  rv$stage_lactation * (rv$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
           +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    rv$DMI_projected  %>% round(3) %>% helpText()
  })
  
  # --- Replacement ---
  # --- Utilities --- 
  # --- Inflations ---
  
  # ---------- Partial Budget Analysis ----------
  IOFC <- reactive({
     (input$milk_cow_day * input$price_milk/100 - rv$DMI_day * input$costDM )*330
        - input$additional_labor - input$additional_cost 
        - (input$culling_rate + input$death_rate) * input$cost_heifer + input$cull_price * input$culling_rate
  })
  
#   observeEvent(input$budget, {
#     if (is.null(input$budget)) {
#       return() 
#     } 
#     shinyjs::show(id="partial_budget")
#   })
  
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
  
 
  # --- Positive Impacts---
  output$inc_rev_herd_size  <- renderUI({
    rv$inc_rev_herd_size <- input$herd_increase * IOFC()
    rv$inc_rev_herd_size %>% formatdollar() %>% helpText() %>% span(align="right") 
  })
  
  output$inc_rev_per_cow  <- renderUI({
    rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * rv$herd_size2
    rv$inc_rev_per_cow %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_rev_milk_premium  <- renderUI({
    rv$inc_rev_milk_premium  <- (input$milk_cow_day + input$milk_change )*330 * input$scc_premium/100*
        (input$scc_average*(-input$scc_change)/100)/1000 * rv$herd_size2
    rv$inc_rev_milk_premium  %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_rev_cull_sale  <- renderUI({
    rv$inc_rev_cull_sale   <- rv$herd_size2 * input$change_turnover/100 * input$cull_price
      rv$inc_rev_cull_sale  %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_rev_software  <- renderUI({
    rv$inc_rev_software  <- input$software * rv$herd_size2
      rv$inc_rev_software  %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_rev_total  <- renderUI({
    rv$inc_rev_total <- rv$inc_rev_herd_size + rv$inc_rev_per_cow + rv$inc_rev_milk_premium +
      + rv$inc_rev_cull_sale + rv$inc_rev_software
      rv$inc_rev_total %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$dec_exp_heat_detection  <- renderUI({
    rv$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
      rv$dec_exp_heat_detection %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$dec_exp_labor  <- renderUI({
    rv$dec_exp_labor <- (input$hr_sv_milking + input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
      rv$dec_exp_labor %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$dec_exp_labor_management  <- renderUI({
    rv$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
      rv$dec_exp_labor_management %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$dec_exp_total  <- renderUI({
    rv$dec_exp_total <- rv$dec_exp_heat_detection  + rv$dec_exp_labor + rv$dec_exp_labor_management 
      rv$dec_exp_total %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$positive_total  <- renderUI({
    rv$positive_total <- rv$inc_rev_total +  rv$dec_exp_total
      rv$positive_total %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  # --- Negative Impacts ---
  output$inc_exp_repair  <- renderUI({
    rv$inc_exp_repair <-input$repair * input$n_robot + input$insurance_rate/100 * rv$increased_insurance
      rv$inc_exp_repair %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_feed  <- renderUI({
    rv$inc_exp_feed <-  rv$DMI_change * input$cost_DM * 330 * rv$herd_size2
      rv$inc_exp_feed %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_pellet  <- renderUI({
    rv$inc_exp_pellet <- input$cost_pellets * 330 * rv$herd_size2 * input$pellets/2000
      rv$inc_exp_pellet %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_replacement  <- renderUI({
    rv$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rv$herd_size2
      rv$inc_exp_replacement %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_utilities  <- renderUI({
    rv$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rv$herd_size2
      rv$inc_exp_utilities %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_record_management  <- renderUI({
    rv$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
      rv$inc_exp_record_management %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_capital_recovery  <- renderUI({
    if (input$n_robot_life > 1) {
      tmp <-  - pmt(input$interest/100, rv$housing_years, 
                    (rv$robot_invest*(1 + input$inflation_robot/100)^input$robot_years )/(1 + input$interest/100)^(input$robot_years)) 
    } else {
      tmp <- 0
    }  
     rv$inc_exp_capital_recovery <-   - pmt(input$interest/100, rv$housing_years, rv$robot_invest) + tmp
      rv$inc_exp_capital_recovery %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$inc_exp_total  <- renderUI({
    rv$inc_exp_total <- rv$inc_exp_repair + rv$inc_exp_feed + rv$inc_exp_pellet + rv$inc_exp_replacement + 
          + rv$inc_exp_utilities + rv$inc_exp_record_management + rv$inc_exp_capital_recovery 
      rv$inc_exp_total %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$negative_total  <- renderUI({
    rv$negative_total <- rv$inc_exp_total 
      rv$negative_total  %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  # --- Net Impacts ---
  output$impact_without_housing  <- renderUI({
    rv$impact_without_housing <- rv$positive_total - rv$negative_total
      rv$impact_without_housing %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$capital_recovery_housing  <- renderUI({
    rv$capital_recovery_housing <- - pmt(input$interest/100, rv$housing_years, rv$related_changes)
      rv$capital_recovery_housing %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$capital_recovery_total  <- renderUI({
    rv$capital_recovery_total <- rv$inc_exp_capital_recovery + rv$capital_recovery_housing
      rv$capital_recovery_total %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$impact_with_housing  <- renderUI({
    rv$impact_with_housing <- rv$impact_without_housing - rv$capital_recovery_housing 
      rv$impact_with_housing %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$robot_end_PV  <- renderUI({
    rv$robot_end_PV <- -pmt(input$interest/100, rv$housing_years, input$salvage_robot/(1 + input$interest/100)^rv$housing_years)
      rv$robot_end_PV %>% formatdollar() %>% helpText() %>% span(align="right")
  })
  
  output$impact_with_robot_salvage  <- renderUI({
    rv$impact_with_robot_salvage <- rv$impact_with_housing + rv$robot_end_PV
      rv$impact_with_robot_salvage %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$impact_with_inflation  <- renderUI({
      rv$impact_with_inflation <- "Depends on cash flow"
      rv$impact_with_inflation %>% helpText() %>% span(align="right")
  })
  
  # --- Break-even wage rates ---
  output$be_wage_without_housing  <- renderUI({
    rv$be_wage_without_housing <- (rv$negative_total - rv$inc_rev_total - rv$dec_exp_labor_management)/
        ((rv$dec_exp_heat_detection + rv$dec_exp_labor)/input$labor_rate)
    rv$be_wage_without_housing %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
  })
    
  output$be_wage_with_housing  <- renderUI({
    rv$be_wage_with_housing <- (rv$capital_recovery_housing + rv$negative_total - 
                                     rv$inc_rev_total - rv$dec_exp_labor_management)/
      ((rv$dec_exp_heat_detection + rv$dec_exp_labor)/input$labor_rate)
    rv$be_wage_with_housing %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$be_wage_with_salvage  <- renderUI({
    rv$be_wage_with_salvage <- (rv$capital_recovery_housing + rv$negative_total - rv$robot_end_PV -
                                     rv$inc_rev_total - rv$dec_exp_labor_management)/
        ((rv$dec_exp_heat_detection + rv$dec_exp_labor)/input$labor_rate)
    rv$be_wage_with_salvage %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
  })
  
  output$IOFC <- renderUI({
    IOFC <- IOFC()
    IOFC <- formatdollar(2) %>% strong() %>% helpText() 
  }) 
  
  })


