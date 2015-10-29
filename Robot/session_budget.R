

# ---------- Partial Budget Analysis ----------
## To give instant reaction to changes made in Data Entry tab, all calculations must be reactive to input$XXX. 
## This is done by calling intermediate items as functions and referring to input$XXX.  

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



## Q: Is it better to call IOFC something else?   IOFC and other variable costs?   
## Q: What cuttoff values are appropriate to change colors from green to orange to red? 
IOFC <- reactive({  browser() 
  # initial definiiton: Current IOFC and other variable costs
  #     rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - DMI_day() * input$cost_DM )*330 +
  #          - input$additional_labor - input$additional_cost +
  #         - (input$culling_rate + input$death_rate)/100 * input$cost_heifer + input$cull_price * input$culling_rate/100
  
  # modified as follows: 
  rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - DMI_day() * input$cost_DM )*330
  rv$IOFC
})

IOFC2 <- reactive({  browser()  # under_robot
  rv$IOFC2 <- ((input$milk_cow_day + input$milk_change) * input$price_milk/100 + 
                 - DMI_projected() * input$cost_DM - input$pellets*input$cost_pellets/2000)*330 
  rv$IOFC2
})

IOFC_cwt <- reactive({  browser() 
  browser()
  rv$IOFC_cwt <- IOFC() /365 /input$milk_cow_day * 330
  rv$IOFC_cwt
})

IOFC2_cwt <- reactive({  browser() 
  rv$IOFC2_cwt <- IOFC2() /365 /(input$milk_cow_day + input$milk_change) * 330
  rv$IOFC2_cwt
})


# --- Positive Impacts---
# -- calculations --
inc_rev_herd_size  <- reactive({  browser() 
  # rv$inc_rev_herd_size <- input$herd_increase * IOFC()
  ## Modified as follows 
  rv$inc_rev_herd_size <- (input$milk_cow_day + input$milk_change) * 330 *
    (input$price_milk/100) * input$herd_increase
  rv$inc_rev_herd_size
})

inc_rev_per_cow  <- reactive({  browser() 
  # rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * herd_size2()
  ## Modified as follows 
  rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * input$herd_size
  rv$inc_rev_per_cow 
})

inc_rev_milk_premium  <- reactive({  browser() 
  rv$inc_rev_milk_premium  <- (input$milk_cow_day + input$milk_change )*330 * input$scc_premium/100*
    (input$scc_average*(-input$scc_change)/100)/1000 * herd_size2()
  rv$inc_rev_milk_premium  
})

inc_rev_cull_sale  <- reactive({  browser() 
  rv$inc_rev_cull_sale   <- herd_size2() * input$change_turnover/100 * input$cull_price
  rv$inc_rev_cull_sale  
})

inc_rev_software  <- reactive({  browser() 
  rv$inc_rev_software  <- input$software * herd_size2()
  rv$inc_rev_software 
})

inc_rev_total  <- reactive({  browser() 
  rv$inc_rev_total <- inc_rev_herd_size() + inc_rev_per_cow() + inc_rev_milk_premium() +
    + inc_rev_cull_sale() + inc_rev_software()
  rv$inc_rev_total
})

dec_exp_heat_detection  <- reactive({  browser() 
  rv$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat)*input$labor_rate *365
  rv$dec_exp_heat_detection 
})

dec_exp_labor  <- reactive({  browser() 
  #rv$dec_exp_labor <- (input$hr_sv_milking + input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
  # Modified as follows
  rv$dec_exp_labor <- input$hr_sv_milking * input$labor_rate *365 
  rv$dec_exp_labor 
})

dec_exp_labor_management  <- reactive({  browser() 
  rv$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
  rv$dec_exp_labor_management 
})

dec_exp_total  <- reactive({  browser() 
  rv$dec_exp_total <- dec_exp_heat_detection()  + dec_exp_labor() + dec_exp_labor_management() 
  rv$dec_exp_total
})

positive_total  <- reactive({  browser() 
  rv$positive_total <- inc_rev_total() +  dec_exp_total()
  rv$positive_total
})

# -- rendering to UI -- 
output$inc_rev_herd_size  <- renderUI({
  inc_rev_herd_size() %>% formatdollar() %>% helpText() %>% span(align="right") 
})

output$inc_rev_per_cow  <- renderUI({
  inc_rev_per_cow() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_rev_milk_premium  <- renderUI({
  inc_rev_milk_premium()  %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_rev_cull_sale  <- renderUI({
  inc_rev_cull_sale()  %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_rev_software  <- renderUI({
  inc_rev_software()  %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_rev_total  <- renderUI({
  inc_rev_total() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$dec_exp_heat_detection  <- renderUI({
  dec_exp_heat_detection() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$dec_exp_labor  <- renderUI({
  dec_exp_labor() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$dec_exp_labor_management  <- renderUI({
  dec_exp_labor_management() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$dec_exp_total  <- renderUI({
  dec_exp_total() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$positive_total  <- renderUI({
  positive_total() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})


# --- Negative Impacts ---
# -- calculations --
inc_exp_herd_increase  <- reactive({  browser() 
  rv$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase
  rv$inc_exp_herd_increase
})

inc_exp_repair  <- reactive({  browser() 
  rv$inc_exp_repair <-input$repair * input$n_robot + input$insurance_rate/100 * increased_insurance()
  rv$inc_exp_repair 
})

inc_exp_feed  <- reactive({  browser() 
  rv$inc_exp_feed <-  DMI_change() * input$cost_DM * 330 * herd_size2()
  rv$inc_exp_feed 
})

inc_exp_pellet  <- reactive({  browser() 
  rv$inc_exp_pellet <- input$cost_pellets * 330 * herd_size2() * input$pellets/2000
  rv$inc_exp_pellet
})

inc_exp_replacement  <- reactive({  browser() 
  rv$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * herd_size2()
  rv$inc_exp_replacement 
})

inc_exp_utilities  <- reactive({  browser() 
  rv$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * herd_size2()
  rv$inc_exp_utilities 
})

inc_exp_record_management  <- reactive({  browser() 
  rv$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
  rv$inc_exp_record_management
})

inc_exp_capital_recovery  <- reactive({  browser() 
  if (is.na(input$n_robot_life) | is.na(input$interest) | 
      is.na(housing_years()) | is.na(robot_invest()) | is.na(input$inflation_robot) |
      is.na(input$robot_years)) {
    return(NA)
  }
  if (input$n_robot_life > 1) {
    tmp <-  - pmt(input$interest/100, housing_years(), 
                  (robot_invest()*(1 + input$inflation_robot/100)^input$robot_years )/
                    (1 + input$interest/100)^(input$robot_years)) 
  } else {
    tmp <- 0
  }  
  rv$inc_exp_capital_recovery <-   - pmt(input$interest/100, housing_years(), robot_invest()) + tmp
  rv$inc_exp_capital_recovery 
})

inc_exp_total  <- reactive({  browser() 
  rv$inc_exp_total <- inc_exp_herd_increase() + inc_exp_repair() + inc_exp_feed() + inc_exp_pellet() + inc_exp_replacement() + 
    + inc_exp_utilities() + inc_exp_record_management() + inc_exp_capital_recovery() 
  rv$inc_exp_total
})

negative_total  <- reactive({  browser() 
  rv$negative_total <- inc_exp_total()
  rv$negative_total  
})

# -- rendering to UI --
output$inc_exp_herd_increase <- renderUI({
  inc_exp_herd_increase()  %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_repair  <- renderUI({
  inc_exp_repair() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_feed  <- renderUI({
  inc_exp_feed() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_pellet  <- renderUI({
  inc_exp_pellet() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_replacement  <- renderUI({
  inc_exp_replacement() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_utilities  <- renderUI({
  inc_exp_utilities() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_record_management  <- renderUI({
  inc_exp_record_management() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_capital_recovery  <- renderUI({
  inc_exp_capital_recovery() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$inc_exp_total  <- renderUI({
  inc_exp_total() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$negative_total  <- renderUI({
  negative_total()  %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})


# --- Net Impacts ---
# -- calculations --
impact_without_housing  <- reactive({  browser() 
  rv$impact_without_housing <- positive_total() - negative_total()
  rv$impact_without_housing 
})

capital_recovery_housing  <- reactive({  browser() 
  if (is.na(input$interest) | is.na(housing_years()) | is.na(cost_housing())) {
    return(NA)
  }
  rv$capital_recovery_housing <- - pmt(input$interest/100, housing_years(), cost_housing())
  rv$capital_recovery_housing
})

capital_recovery_total  <- reactive({  browser() 
  rv$capital_recovery_total <- inc_exp_capital_recovery() + capital_recovery_housing()
  rv$capital_recovery_total 
})

impact_with_housing <- reactive({  browser() 
  rv$impact_with_housing <- impact_without_housing() - capital_recovery_housing()
  rv$impact_with_housing
})

robot_end_PV <- reactive({  browser() 
  if (is.na(input$interest) | is.na(input$salvage_robot) | is.na(housing_years()) | is.na(cost_housing())) {
    return(NA)
  }
  rv$robot_end_PV <- -pmt(input$interest/100, housing_years(), input$salvage_robot/(1 + input$interest/100)^housing_years())
  rv$robot_end_PV 
})

impact_with_robot_salvage <- reactive({  browser()    
  rv$impact_with_robot_salvage <- impact_with_housing() + robot_end_PV()
  
  # This is used later for alerting base value change in robustness analysis  
  createAlert(session, "c_input_change", "ref_c_input_change", 
              content = "New base values. 
            Press [Calculate] to updated the results.",
              append = FALSE) 
  
  createAlert(session, "s_input_change", "ref_s_input_change", 
              content = "New base values. 
            Press [Calculate] to updated the results.",
              append = FALSE) 
  
  rv$impact_with_robot_salvage
})

impact_with_inflation  <- reactive({  browser() 
  rv$impact_with_inflation <- "Depends on cash flow"
  rv$impact_with_inflation
})

## -- rendering to UI --
output$impact_without_housing  <- renderUI({
  impact_without_housing() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$capital_recovery_housing  <- renderUI({
  capital_recovery_housing() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$capital_recovery_total  <- renderUI({
  capital_recovery_total() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$impact_with_housing  <- renderUI({
  impact_with_housing() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$robot_end_PV  <- renderUI({
  robot_end_PV() %>% formatdollar() %>% helpText() %>% span(align="right")
})

output$impact_with_robot_salvage  <- renderUI({
  impact_with_robot_salvage() %>% formatdollar() %>% strong() %>% helpText() %>% span(align="right")
})

output$impact_with_inflation  <- renderUI({
  impact_with_inflation() %>% helpText() %>% span(align="right")
})


# --- Break-even wage rates ---
output$be_wage_without_housing  <- renderUI({
  rv$be_wage_without_housing <- (negative_total() - inc_rev_total() - dec_exp_labor_management())/
    ((dec_exp_heat_detection() + dec_exp_labor())/input$labor_rate)
  rv$be_wage_without_housing %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
})

output$be_wage_with_housing  <- renderUI({
  rv$be_wage_with_housing <- (capital_recovery_housing() + negative_total() - 
                                inc_rev_total() - dec_exp_labor_management())/
    ((dec_exp_heat_detection() + dec_exp_labor())/input$labor_rate)
  rv$be_wage_with_housing %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
})

output$be_wage_with_salvage  <- renderUI({
  rv$be_wage_with_salvage <- (capital_recovery_housing() + negative_total() - robot_end_PV() -
                                inc_rev_total() - dec_exp_labor_management())/
    ((dec_exp_heat_detection() + dec_exp_labor())/input$labor_rate)
  rv$be_wage_with_salvage %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
})


