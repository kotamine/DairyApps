library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

source("helper.R")

 
func1_for_feature_1 <- function(input, output, session) {
  # do some calculation for feature 1
  
}

shinyServer(function(input, output, session) {

  # Create a list of reactive values 
  rv <- reactiveValues(input_id=0)
  conv_factor <- 2.2046  # conversion factor from kg to pound 
  
  func1_for_feature_1(input, output, session)

  # ---------- Fill out the calclated values in Data Entry ----------
  # --- Calculations of variables (stored as functions), followed by rendering to User Interface ---

  # ----- calculations -----
  # --- Farm Finaces ---
  
  # This sets the default value for additional_labor and additional_cost when hidden from the user
  observe({
    if (input$herd_increase==0)
    { updateNumericInput(session, "additional_labor",NULL,value=450,step=50,min=0)
      updateNumericInput(session, "additional_cost",NULL,value=200,step=50,min=0)
    }
  })

   herd_size2 <- reactive({
     rv$herd_size2 <- input$herd_size + input$herd_increase
     rv$herd_size2
   })
  
  robot_invest <- reactive({
    rv$robot_invest <- input$n_robot * input$cost_robot
    rv$robot_invest
  })
  
  cost_housing <- reactive({
    rv$cost_housing <- input$cost_housing_cow * herd_size2()
    rv$cost_housing 
  })
  
  total_investment <- reactive({
    rv$total_investment <- total_investment_cow()  * herd_size2()
    rv$total_investment
  })
  
  total_investment_cow <- reactive({
    rv$total_investment_cow <-  input$cost_housing_cow + robot_invest()/herd_size2()
    rv$total_investment_cow
  })
  
  # --- Maintenance ---
  housing_years <- reactive({
    rv$housing_years <- input$n_robot_life * input$robot_years
    rv$housing_years 
  })
  
  increased_insurance <- reactive({
    rv$increased_insurance <- total_investment()
    rv$increased_insurance  
  })
  
  # --- Labor Savings ---
  anticipated_hours_milking <- reactive({
    rv$anticipated_hours_milking <- input$hours_milking - input$hr_sv_milking
    rv$anticipated_hours_milking  
  })

  # --- Milk Outputs ---
  milk_lb_robot_day <- reactive({
    rv$milk_lb_robot_day <- (input$milk_cow_day + input$milk_change) * herd_size2()/input$n_robot 
    rv$milk_lb_robot_day   
  })
  
  # --- Feed --- 
  DMI_change <- reactive({
    rv$DMI_change <- DMI_projected() - DMI_day()
    rv$DMI_change  
  })
  

  
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
  
  adj_milk_cow_day <- reactive({
    rv$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
      + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
    rv$adj_milk_cow_day 
  })
  
  adj_milk_cow_day2 <- reactive({  # adj_milk_cow_day with the herd size under using robots
    rv$adj_milk_cow_day2 <- (input$milk_cow_day + input$milk_change) * input$milk_cow_coeff + 
      + (input$milk_cow_day + input$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
    rv$adj_milk_cow_day2
  })
  
  stage_lactation <- reactive({
    rv$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
    rv$stage_lactation 
  })
  
  DMI_day <- reactive({
    rv$DMI_day <-  stage_lactation() * (adj_milk_cow_day()/conv_factor * input$adj_milk_cow_coeff + 
          +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    rv$DMI_day
  })
  
  DMI_projected <- reactive({
    rv$DMI_projected <-  stage_lactation() * (adj_milk_cow_day2()/conv_factor * input$adj_milk_cow_coeff + 
           +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    rv$DMI_projected 
  })
  
  # --- Replacement ---
  #  nothing here
  
  # --- Utilities --- 
  #  nothing here
  
  # --- Inflations ---
  #  nothing here
  
  # ------ rendering to UI ------
  output$herd_size2 <- renderUI({
    herd_size2() %>% formatcomma() %>% helpText()
  })
  
  output$robot_invest <- renderUI({
    robot_invest() %>% formatcomma() %>% helpText() 
  })
  
  output$cost_housing <- renderUI({
    cost_housing() %>% formatcomma() %>% helpText() 
  })
  
  output$total_investment <- renderUI({
    total_investment()  %>% formatcomma()  %>% helpText() 
  })
  
  output$total_investment_cow <- renderUI({
    total_investment_cow() %>% formatcomma() %>% helpText() 
  })
  
  output$housing_years <- renderUI({
    housing_years()  %>% formatcomma() %>% helpText() 
  })
  
  output$increased_insurance <- renderUI({
    increased_insurance()  %>% formatcomma() %>% helpText() 
  })
  
  output$anticipated_hours_milking <- renderUI({
    anticipated_hours_milking()  %>% formatcomma()  %>% helpText() 
  })
  
  output$milk_lb_robot_day <- renderUI({
    milk_lb_robot_day()  %>% formatcomma() %>% helpText() 
  })
  
  output$DMI_change <- renderUI({
    DMI_change()  %>% round(3) %>% helpText()
  })
  
  output$rep_milk_cow_day <- renderUI({
    input$milk_cow_day %>% formatcomma() %>% helpText() 
  })
  
  output$rep_milk_change <- renderUI({
    input$milk_change  %>% formatcomma() %>% helpText() 
  })
  
  output$DMI_change_copy <- renderUI({
    DMI_change() %>% round(3) %>% helpText()
  })
  
  output$adj_milk_cow_day <- renderUI({
    adj_milk_cow_day()  %>% round(2) %>% helpText()
  })
  
  output$stage_lactation <- renderUI({
    stage_lactation()  %>% round(4) %>% helpText()
  })
  
  output$DMI_day <- renderUI({
    DMI_day()  %>% round(3) %>% helpText()
  })
  
  output$DMI_projected <- renderUI({
    DMI_projected()  %>% round(3) %>% helpText()
  })
  
  # ---------- Partial Budget Analysis ----------
  ## To give instant reaction to changes made in Data Entry tab, all calculations must be reactive to input$XXX. 
  ## This is done by calling intermediate items as functions and referring to input$XXX.  
  
  ## Q: Is it better to call IOFC something else?   IOFC and other variable costs?   
  ## Q: What cuttoff values are appropriate to change colors from green to orange to red? 
  IOFC <- reactive({
    # initial definiiton: Current IOFC and other variable costs
#     rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - DMI_day() * input$cost_DM )*330 +
#          - input$additional_labor - input$additional_cost +
#         - (input$culling_rate + input$death_rate)/100 * input$cost_heifer + input$cull_price * input$culling_rate/100
     
     # modified as follows: 
     rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - DMI_day() * input$cost_DM )*330
     rv$IOFC
  })

  IOFC2 <- reactive({ # under_robot
    rv$IOFC2 <- ((input$milk_cow_day + input$milk_change) * input$price_milk/100 - 
                   + DMI_projected() * input$cost_DM + input$pellets*input$cost_pellets/2000)*330 
    rv$IOFC2
  })
  
  IOFC_cwt <- reactive({
    rv$IOFC_cwt <- IOFC() /365 /input$milk_cow_day * 330
    rv$IOFC_cwt
  })
  
  IOFC2_cwt <- reactive({
    rv$IOFC2_cwt <- IOFC2() /365 /(input$milk_cow_day + input$milk_change) * 330
    rv$IOFC2_cwt
  })
  
  
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
  # -- calculations --
 inc_rev_herd_size  <- reactive({
   # rv$inc_rev_herd_size <- input$herd_increase * IOFC()
   ## Modified as follows 
   rv$inc_rev_herd_size <- (input$milk_cow_day + input$milk_change) * 330 *
     (input$price_milk/100) * input$herd_increase
   rv$inc_rev_herd_size
 })
 
  inc_rev_per_cow  <- reactive({
    # rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * herd_size2()
    ## Modified as follows 
    rv$inc_rev_per_cow <- input$milk_change * 330 * (input$price_milk/100) * input$herd_size
    rv$inc_rev_per_cow 
  })
  
  inc_rev_milk_premium  <- reactive({
    rv$inc_rev_milk_premium  <- (input$milk_cow_day + input$milk_change )*330 * input$scc_premium/100*
        (input$scc_average*(-input$scc_change)/100)/1000 * herd_size2()
    rv$inc_rev_milk_premium  
  })
  
  inc_rev_cull_sale  <- reactive({
    rv$inc_rev_cull_sale   <- herd_size2() * input$change_turnover/100 * input$cull_price
      rv$inc_rev_cull_sale  
  })

  inc_rev_software  <- reactive({
    rv$inc_rev_software  <- input$software * herd_size2()
    rv$inc_rev_software 
  })
  
  inc_rev_total  <- reactive({
    rv$inc_rev_total <- inc_rev_herd_size() + inc_rev_per_cow() + inc_rev_milk_premium() +
      + inc_rev_cull_sale() + inc_rev_software()
      rv$inc_rev_total
  })
  
  dec_exp_heat_detection  <- reactive({
    rv$dec_exp_heat_detection <- (input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
      rv$dec_exp_heat_detection 
  })
  
  dec_exp_labor  <- reactive({
    #rv$dec_exp_labor <- (input$hr_sv_milking + input$hr_heat_detection - input$anticipated_hours_heat )*input$labor_rate *365
    # Modified as follows
    rv$dec_exp_labor <- input$hr_sv_milking * input$labor_rate *365 
      rv$dec_exp_labor 
  })
  
  dec_exp_labor_management  <- reactive({
    rv$dec_exp_labor_management <- input$decrease_lab_mgt * input$labor_rate_rc_mgt * 365
      rv$dec_exp_labor_management 
  })
  
  dec_exp_total  <- reactive({
    rv$dec_exp_total <- dec_exp_heat_detection()  + dec_exp_labor() + dec_exp_labor_management() 
      rv$dec_exp_total
  })

  positive_total  <- reactive({
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
  inc_exp_herd_increase  <- reactive({
    rv$inc_exp_herd_increase <- (input$additional_labor + input$additional_cost)*input$herd_increase
     rv$inc_exp_herd_increase
  })
  
  inc_exp_repair  <- reactive({
    rv$inc_exp_repair <-input$repair * input$n_robot + input$insurance_rate/100 * increased_insurance()
      rv$inc_exp_repair 
  })
  
  inc_exp_feed  <- reactive({
    rv$inc_exp_feed <-  DMI_change() * input$cost_DM * 330 * herd_size2()
      rv$inc_exp_feed 
  })
  
  inc_exp_pellet  <- reactive({
    rv$inc_exp_pellet <- input$cost_pellets * 330 * herd_size2() * input$pellets/2000
      rv$inc_exp_pellet
  })
  
  inc_exp_replacement  <- reactive({
    rv$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * herd_size2()
      rv$inc_exp_replacement 
  })
  
  inc_exp_utilities  <- reactive({
    rv$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * herd_size2()
      rv$inc_exp_utilities 
  })
  
  inc_exp_record_management  <- reactive({
    rv$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
      rv$inc_exp_record_management
  })
  
  inc_exp_capital_recovery  <- reactive({
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
  
  inc_exp_total  <- reactive({
    rv$inc_exp_total <- inc_exp_herd_increase() + inc_exp_repair() + inc_exp_feed() + inc_exp_pellet() + inc_exp_replacement() + 
          + inc_exp_utilities() + inc_exp_record_management() + inc_exp_capital_recovery() 
      rv$inc_exp_total
  })
  
  negative_total  <- reactive({
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
  impact_without_housing  <- reactive({
    rv$impact_without_housing <- positive_total() - negative_total()
      rv$impact_without_housing 
  })
  
  capital_recovery_housing  <- reactive({
    rv$capital_recovery_housing <- - pmt(input$interest/100, housing_years(), cost_housing())
    rv$capital_recovery_housing
  })
  
  capital_recovery_total  <- reactive({
    rv$capital_recovery_total <- inc_exp_capital_recovery() + capital_recovery_housing()
    rv$capital_recovery_total 
  })
  
  impact_with_housing <- reactive({
    rv$impact_with_housing <- impact_without_housing() - capital_recovery_housing()
    rv$impact_with_housing
  })

  robot_end_PV <- reactive({
    rv$robot_end_PV <- -pmt(input$interest/100, housing_years(), input$salvage_robot/(1 + input$interest/100)^housing_years())
    rv$robot_end_PV 
  })
  
  impact_with_robot_salvage <- reactive({ 
    rv$impact_with_robot_salvage <- impact_with_housing() + robot_end_PV()
    rv$new_input <- TRUE # This is used later for storing inputs in table 
    rv$impact_with_robot_salvage
  })
  
  impact_with_inflation  <- reactive({
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
  
  

  # ------ Dashboard features ------
  NAI <- reactive({
    if (input$NAI=="w/o housing") {
      NAI <- impact_without_housing()
    } 
    else if (input$NAI=="w/ housing") {
      NAI <- impact_with_housing()
    } else {
      NAI <- impact_with_robot_salvage()
    }
    rv$NAI <- NAI
    rv$NAI
  })
  
  capital_cost <- reactive({
    if(input$NAI=="w/o housing") {
      rv$capital_cost <- -inc_exp_capital_recovery()
    } else if (input$NAI=="w/ housing") {
      rv$capital_cost <- -(inc_exp_capital_recovery() + capital_recovery_housing())
    } else  {
      rv$capital_cost <- -(inc_exp_capital_recovery() + capital_recovery_housing()) +
        + robot_end_PV()
    } 
    rv$capital_cost
  }) 
  
  milk_current <- reactive({
    input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
    +  input$scc_premium/100 * input$scc_average/1000) 
    })
  
  milk_robot <- reactive({
    herd_size2() * 330 * (input$milk_cow_day + input$milk_change) * (input$price_milk/100 + 
    +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 
  })
  
  labor_current <- reactive({
    (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365
  })

  labor_robot <- reactive({
    (input$anticipated_hours_heat + anticipated_hours_milking()) * input$labor_rate *365 + 
       + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
      + input$additional_labor * input$herd_increase 
  }) 
  
  feed_current <- reactive({
    DMI_day() * input$cost_DM * 330 * input$herd_size
  })
  
  feed_robot <- reactive({
    (DMI_projected() * input$cost_DM + input$pellets * input$cost_pellets/2000) * 330 * herd_size2()
  })

  output$IOFC <- renderUI({
    if (input$IOFC=="per cow") {
      dash_IOFC(IOFC(), IOFC2(), basis=input$IOFC)
    } else {
      dash_IOFC(IOFC_cwt(), IOFC2_cwt(), basis=input$IOFC)
      
    }
  })  
  
  output$NAI <- renderUI({
    dash_NAI(NAI(),cutoff=0)
  }) 
  
  output$milk_feed <- renderUI({
     rv$milk_feed <- -(feed_robot() - feed_current()) + milk_robot() -  milk_current() 
     div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
         rv$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
         h5("Milk Income - Feed Cost"), h5("under robot"))
  })  
  
  output$labor_repair <- renderUI({
    rv$labor_repair <- -(labor_robot() - labor_current() +inc_exp_repair())
    div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
        rv$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
        h5("Labor + Repair Cost"), h5("under robot"))
  })  
  
  output$captial_cost <- renderUI({
    div(class="well well-sm", style= "background-color: #64E986; color:white;", 
        capital_cost() %>% formatdollar2() %>% strong() %>% h4(),
        h5("Cost of Capital"),  h5("under robot"))
  })  
  
  output$misc <- renderUI({
    NAI <- NAI()
    milk_feed <- -(feed_robot() - feed_current()) + milk_robot() -  milk_current() 
    labor_repair <- -(labor_robot() - labor_current() +inc_exp_repair())
    capital_cost <- capital_cost()
    
    rv$misc <- NAI - (milk_feed + labor_repair + capital_cost)
    div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
        rv$misc %>% formatdollar2() %>% strong %>% h4(), 
        h5("The Rest"), h5("under robot"))
  })  
  
  output$plot1 <- renderPlot({ 
    dash_plot1(feed_current(),feed_robot(),milk_current(),milk_robot())
  })
  
  output$plot2 <- renderPlot({
    dash_plot2(inc_exp_repair(),labor_current(),labor_robot()) 
  })
  
  output$plot3 <- renderPlot({ 
    dash_plot3(inc_exp_capital_recovery(),capital_recovery_housing(),robot_end_PV(),input$NAI)  
  })
  
  # ----------- Sensitivity Analysis -----------
  
  # Create a list of reactive values for robustness checks
  rb <- reactiveValues(new_row =TRUE)
  
  observeEvent(input$c_choice, {
    if (input$c_choice=="c1") {
      updateNumericInput(session,"c_val",NULL, value=20, step=10)
    } 
    else  if (input$c_choice=="c2") {
      updateNumericInput(session,"c_val",NULL, value=20, step=10)
    }
    else  if (input$c_choice=="c3") {
      updateNumericInput(session,"c_val",NULL, value=50, step=10)
    }
    else  if (input$c_choice=="c4") {
      updateNumericInput(session,"c_val",NULL, value=50, step=10)
    }
    else  if (input$c_choice=="c5") {
      updateNumericInput(session,"c_val",NULL, value=-50, step=10)
    }
    else  if (input$c_choice=="c6") {
      updateNumericInput(session,"c_val",NULL, value=-50, step=10)
    }
    else  if (input$c_choice=="c7") {
      updateNumericInput(session,"c_val",NULL, value=50, step=10)
    }
    
  })
  
#   observeEvent(input$c_choice, {
#     rb$c_choice <- "c1"
#   })
#   
  output$c_text <- renderUI({
    if (input$c_choice=="c1") {
      rb$var <- "Estimated cost per robot"
      rb$value <- input$cost_robot
      rb$new_value <- (input$cost_robot * (1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- ""
    } 
    else if (input$c_choice=="c2") {
      rb$var <-  "Related housing changes needed per cow"
      rb$value <-  input$cost_housing_cow
      rb$new_value <- (input$cost_housing_cow *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- ""
    } 
    else if (input$c_choice=="c3") {
      rb$var <- "Estimated annual change in milking system repair"
      rb$value <-   input$repair
      rb$new_value <- ( input$repair *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- ""
    } 
    else if (input$c_choice=="c4") {
      rb$var <- "Robots: years of useful life"
      rb$value <-   input$robot_years 
      rb$new_value <- (input$robot_years  *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- "years"
    }
    else if (input$c_choice=="c5") {
      rb$var <- "Value of the robots after useful life"
      rb$value <-  input$salvage_robot
      rb$new_value <- ( input$salvage_robot *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- ""
    }
    else if (input$c_choice=="c6") {
      rb$var <-  "Anticipated savings in milking & chore labor"
      rb$value <-  input$hr_sv_milking 
      rb$new_value <- (  input$hr_sv_milking  *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- "hrs/day"
    }
    else if (input$c_choice=="c7") {
      rb$var <- "Projected change in milk production"
      rb$value <-   input$milk_change
      rb$new_value <- ( input$milk_change *(1 + input$c_val/100))
      val0 <-  rb$value %>% formatdollar()
      val1 <-  rb$new_value %>% formatdollar()
      unit <- "lb/cow/day"
    }
    paste("from", val0," to ",val1, unit) %>% h5()
  })

  
  observe({  
    # Initialize changed values 
    rb$cost_robot <- input$cost_robot
    rb$cost_housing_cow <- input$cost_housing_cow
    rb$repair <- input$repair
    rb$robot_years <- input$robot_years
    rb$salvage_robot <- input$salvage_robot
    rb$hr_sv_milking <- input$hr_sv_milking
    rb$milk_change <- input$milk_change
    
    # Change the selected value by input$c_val
    rb$cost_robot <- input$cost_robot*(1+ (input$c_choice=="c1")*input$c_val/100)
    rb$cost_housing_cow <- input$cost_housing_cow*(1+ (input$c_choice=="c2")*input$c_val/100)
    rb$repair <- input$repair*(1+ (input$c_choice=="c3")*input$c_val/100)
    rb$robot_years <- input$robot_years*(1+ (input$c_choice=="c4")*input$c_val/100)
    rb$salvage_robot <- input$salvage_robot*(1+ (input$c_choice=="c5")*input$c_val/100)
    rb$hr_sv_milking <- input$hr_sv_milking*(1+ (input$c_choice=="c6")*input$c_val/100)
    rb$milk_change <- input$milk_change*(1+ (input$c_choice=="c7")*input$c_val/100)
    
    

   # isolate( 
    ## --- For robustoness analysis we will calculate almost everything  all over again ---
    # Techincally, we don't need to store all calclation results, but we store them all under "rb".  
    # Data Entry Level Calculations
      rb$herd_size2 <- input$herd_size + input$herd_increase
      
      rb$robot_invest <- input$n_robot * rb$cost_robot
   
      rb$cost_housing <- rb$cost_housing_cow * rb$herd_size2
      
      rb$total_investment_cow <-  rb$cost_housing_cow + rb$robot_invest/rb$herd_size2
      
      rb$total_investment <- rb$total_investment_cow  * rb$herd_size2
 
      rb$housing_years <- input$n_robot_life * rb$robot_years
    
      rb$increased_insurance <- rb$total_investment
      
      rb$anticipated_hours_milking <- input$hours_milking - rb$hr_sv_milking
    
      rb$milk_lb_robot_day <- (input$milk_cow_day + rb$milk_change) * rb$herd_size2/input$n_robot 
    
      rb$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
        + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff

      rb$adj_milk_cow_day2 <- (input$milk_cow_day + rb$milk_change) * input$milk_cow_coeff + 
        + (input$milk_cow_day + rb$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
    
      rb$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
      
      rb$DMI_day <-  rb$stage_lactation * (rb$adj_milk_cow_day/conv_factor * input$adj_milk_cow_coeff + 
                    +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    
      rb$DMI_projected <-  rb$stage_lactation * (rb$adj_milk_cow_day2/conv_factor * input$adj_milk_cow_coeff + 
                   +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
    
    rb$DMI_change <- rb$DMI_projected - rb$DMI_day
    
    rb$adj_milk_cow_day2 <- (input$milk_cow_day +  rb$milk_change) * input$milk_cow_coeff + 
      + (input$milk_cow_day + rb$milk_change)  * input$milk_fat/100 * input$milk_fat_coeff 
    
    rb$IOFC <- (input$milk_cow_day * input$price_milk/100 - rb$DMI_day * input$cost_DM )*330
    
    rb$IOFC2 <- ((input$milk_cow_day +  rb$milk_change) * input$price_milk/100 + 
                   - rb$DMI_projected * input$cost_DM + input$pellets * input$cost_pellets/2000)*330 
  
    rb$IOFC_cwt <- rb$IOFC /365 /input$milk_cow_day * 330
    
    rb$IOFC2_cwt <- rb$IOFC2 /365 /(input$milk_cow_day + rb$milk_change) * 330
      
    
    
    # Positive Impacts
      rb$inc_rev_herd_size <- (input$milk_cow_day + rb$milk_change) * 330 *
        (input$price_milk/100) * input$herd_increase

      rb$inc_rev_per_cow <- rb$milk_change * 330 * (input$price_milk/100) * input$herd_size

      rb$inc_rev_milk_premium  <- (input$milk_cow_day + rb$milk_change )*330 * input$scc_premium/100*
        (input$scc_average*(-input$scc_change)/100)/1000 * rb$herd_size2

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
    
      rb$inc_exp_pellet <- input$cost_pellets * 330 * rb$herd_size2 * input$pellets/2000
      
      rb$inc_exp_replacement <- input$cost_heifer * input$change_turnover/100 * rb$herd_size2

      rb$inc_exp_utilities <- (input$change_electricity + input$change_water + input$change_chemical) * rb$herd_size2
      
      rb$inc_exp_record_management <- input$increase_rc_mgt * input$labor_rate_rc_mgt * 365
    
      if (input$n_robot_life > 1) {
        tmp <-  - pmt(input$interest/100, rb$housing_years, 
                      rb$robot_invest*(1 + input$inflation_robot/100)^rb$robot_years/
                        (1 + input$interest/100)^(rb$robot_years))   
      } else {
        tmp <- 0
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
    rb$milk_current <- 
      input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                      +  input$scc_premium/100 * input$scc_average/1000) 
    
    rb$milk_robot <-  rb$herd_size2 * 330 * (input$milk_cow_day + rb$milk_change) *
      (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 
    
    rb$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365
  
    
    rb$labor_robot <- (input$anticipated_hours_heat + rb$anticipated_hours_milking) * input$labor_rate *365 + 
        + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
        + input$additional_labor * input$herd_increase  

    rb$feed_current <-  rb$DMI_day * input$cost_DM * 330 * input$herd_size 

    rb$feed_robot <- (rb$DMI_projected * input$cost_DM + input$pellets *
                        input$cost_pellets/2000) * 330 * rb$herd_size2
   # )
    rb$new_row <- TRUE
  })
  
  # --- Dashboard features ---
  rb_NAI <- reactive({
    if (input$NAI=="w/o housing") {
      rb$NAI <- rb$impact_without_housing
    } 
    else if (input$NAI=="w/ housing") {
      rb$NAI <- rb$impact_with_housing
    } else {
      rb$NAI <- rb$impact_with_robot_salvage
    }
    rb$NAI
  })
  
  rb_capital_cost <- reactive({
    if(input$NAI=="w/o housing") {
      rb$capital_cost <- -rb$inc_exp_capital_recovery
    } else if (input$NAI=="w/ housing") {
      rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing)
    } else  {
      rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing) +
        + rb$robot_end_PV
    } 
    rb$capital_cost
  }) 
  
  output$c_IOFC <- renderUI({
    if (input$IOFC=="per cow") {
      dash_IOFC(rb$IOFC, rb$IOFC2, basis=input$IOFC, 
              compare=IOFC(), compare2=IOFC2())
    } else {
      dash_IOFC(rb$IOFC_cwt, rb$IOFC2_cwt, basis=input$IOFC, 
                compare=IOFC_cwt(), compare2=IOFC2_cwt())
    }
  })  
  
  output$c_NAI <- renderUI({
    dash_NAI(rb_NAI(),cutoff=0, compare=NAI())
  }) 

  
  output$c_milk_feed <- renderUI({
    rb$milk_feed <- -(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current   
    diff <- rb$milk_feed - rv$milk_feed
    div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
        rb$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
        diff %>% formatdollar2b() %>% strong() %>% h4())
  })   
  
  output$c_labor_repair <- renderUI({
    rb$labor_repair <- -(rb$labor_robot - rb$labor_current + rb$inc_exp_repair)
    diff <- rb$labor_repair - rv$labor_repair
    div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
        rb$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
        diff %>% formatdollar2b() %>% strong() %>% h4())
  })  
  
  output$c_captial_cost <- renderUI({
    diff <- rb_capital_cost() - capital_cost()
    div(class="well well-sm", style= "background-color: #64E986; color:white;", 
        rb_capital_cost() %>% formatdollar2() %>% strong() %>% h4(),
        diff %>% formatdollar2b() %>% strong() %>% h4())
  })  
  
  output$c_misc <- renderUI({
    NAI <- rb_NAI()
    milk_feed <- -(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current 
    labor_repair <- -(rb$labor_robot - rb$labor_current +rb$inc_exp_repair)
    capital_cost <- rb_capital_cost()
    
    rb$misc <- NAI - (milk_feed + labor_repair + capital_cost)
    diff <- rb$misc - rv$misc 
    div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
        rb$misc %>% formatdollar2() %>% strong %>% h4(), 
        diff %>% formatdollar2b() %>% strong() %>% h4())
  })  
  
  output$c_plot1 <- renderPlot({ 
    dash_plot1(rb$feed_current,rb$feed_robot,rb$milk_current,rb$milk_robot)
  })
  
  output$c_plot2 <- renderPlot({
    dash_plot2(rb$inc_exp_repair,rb$labor_current,rb$labor_robot) 
  })
  
  output$c_plot3 <- renderPlot({ 
    dash_plot3(rb$inc_exp_capital_recovery,rb$capital_recovery_housing,rb$robot_end_PV, input$NAI)  
  })
  
  rb$colnames <- c("input_id","variable", "% change","value","new value",
                                   "net impact w/o housing","change: impact w/o housing", 
                                   "net impact w/ housing","change: impact w/ housing",
                                   "net impact w/ salvage", "change: impact w/ salvage",
                                   "IFOC gain cow/year", "change: IOFC gain cow",
                                   "IFOC gain cwt", "change: IOFC gain cwt",
                                   "milk - feed", "change: milk - feed",
                                   "labor + repair", "change: labor + repair",
                                   "capital cost", "change: capital cost",
                                   "the rest", "change: the rest")
  
  rb$table_sensitivity <- data.frame(Column1 = numeric(0)) # creating an emptry table
  
  rv$colnames <- c("input_id", "milk_cow_day","milk_change")
  rv$table_input <- data.frame(Column1 = numeric(0))
    
  
  observe({
    if (is.null(rv$new_input) | is.null(rb$new_row)) {
      return()
    } else if (rv$new_input | rb$new_row)  {
      updateButton(session, "c_store", disabled = FALSE, style = "primary", icon = "")
    } 
    else {
      updateButton(session, "c_store", disabled = TRUE, style = "default", icon = "")
    }    
  }) 
      
  observeEvent(input$c_store,{
    if (rv$new_input) {
      rv$input_id <- rv$input_id + 1
      tmp2 <- matrix(c(rv$input_id, input$milk_cow_day,input$milk_change),nrow=1)
      colnames(tmp2) <- rv$colnames
      # tmp2 <- apply(tmp2,2,round, 2)
      rv$table_input <- rbind(rv$table_input, tmp2)
    } 
    else {
    }
    
    if (rb$new_row) {  
    tmp <- matrix(c(rv$input_id, input$c_val, rb$value, rb$new_value,  
             rb$impact_without_housing, rb$impact_without_housing - rv$impact_without_housing, 
             rb$impact_with_housing, rb$impact_with_housing - rv$impact_with_housing,
             rb$impact_with_robot_salvage, rb$impact_with_robot_salvage - rv$impact_with_robot_salvage,
             rb$IOFC2 - rb$IOFC,  rb$IOFC2-rb$IOFC - (rv$IOFC2 - rv$IOFC),
             rb$IOFC2_cwt - rb$IOFC_cwt,  
             rb$IOFC2_cwt - rb$IOFC_cwt - (IOFC2_cwt() - IOFC_cwt()),           
             rb$milk_feed, rb$milk_feed - rv$milk_feed, 
             rb$labor_repair, rb$labor_repair - rv$labor_repair, 
             rb$capital_cost, rb$capital_cost - rv$capital_cost, 
             rb$misc, rb$misc - rv$misc 
             ), nrow=1)  
    tmp <- apply(tmp,2,round,0)
      tmp <- matrix(c(tmp[1],rb$var,tmp[2:length(tmp)]),nrow=1)
      colnames(tmp) <- rb$colnames

      rb$table_sensitivity <- rbind(rb$table_sensitivity,tmp)
    } 
    else {
    }
    rb$new_row <- FALSE
    rv$new_input <- FALSE
  })
  

  
  output$table_sensitivity <- DT::renderDataTable({
    if (dim( rb$table_sensitivity)[1]>0) {
      # order1 <- c()
      tbl <- rb$table_sensitivity # [,order1] 
      DT::datatable(tbl,
                    rownames = FALSE,
                    extensions = 'ColVis',
                    # extensions = 'ColReorder',
        options = list(
          dom = 'C<"clear">lfrtip',
          scrollX = TRUE,
          scrollCollapse = TRUE,
          scrollY = 500,
          scrollCollapse = TRUE,
          # colVis = list(exclude = c(0, 1,1,0),
                        showNone=TRUE, 
                        activate = 'mouseover'))
    } 
    else {
      return()
    }
    })
  
  output$table_input <-  DT::renderDataTable({
    if (dim( rv$table_input)[1]>0) {
      DT::datatable(rv$table_input,
                    rownames = FALSE,
                    extensions = 'ColReorder', options = list(dom = 'Rlfrtip'))
    } 
    else {
      return()
    }
  })
  
  
  })


