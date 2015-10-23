library(shiny)
library(shinyBS)
library(shinyjs)
suppressPackageStartupMessages(library(dplyr))
# library(magrittr)

source("helper.R")


shinyServer(function(input, output, session) {

  # Create a list of reactive values 
  rv <- reactiveValues(a=NULL)
  conv_factor <- 2.2046  # conversion factor from kg to pound 
  

  # ---------- Fill out the calclated values in Data Entry ----------
  # --- Calculations of variables (stored as functions), followed by rendering to User Interface ---

  # ----- calculations -----
  # --- Farm Finaces ---
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
  
#   shinyjs::onclick("customDMI",
#                   shinyjs::toggle(id="DMI_inputs", anim = TRUE)
#                   )
  
  # Show/hide DMI calculations 
  observe({
    toggle(id="DMI_inputs", condition = input$customDMI)
  })

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
  
  IOFC <- reactive({
    rv$IOFC <- (input$milk_cow_day * input$price_milk/100 - DMI_day() * input$cost_DM )*330 +
        - input$additional_labor - input$additional_cost +
        - (input$culling_rate + input$death_rate)/100 * input$cost_heifer + input$cull_price * input$culling_rate/100
     rv$IOFC
  })

  IOFC2 <- reactive({ # under_robot
    rv$IOFC <- ((input$milk_cow_day + input$milk_change) * input$price_milk/100 - DMI_day() * input$cost_DM )*330 +
      - input$additional_labor - input$additional_cost +
      - (input$culling_rate + input$death_rate)/100 * input$cost_heifer + input$cull_price * input$culling_rate/100
    rv$IOFC
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
   rv$inc_rev_herd_size <- (input$milk_cow_day + input$milk_change) * 330 * (input$price_milk/100) * input$herd_increase
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
    rv$inc_exp_total <- inc_exp_repair() + inc_exp_feed() + inc_exp_pellet() + inc_exp_replacement() + 
          + inc_exp_utilities() + inc_exp_record_management() + inc_exp_capital_recovery() 
      rv$inc_exp_total
  })
  
  negative_total  <- reactive({
    rv$negative_total <- inc_exp_total()
    rv$negative_total  
  })
  
  # -- rendering to UI --
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
  
  
  output$IOFC <- renderUI({
    IOFC <- IOFC()
    if (IOFC>0) { 
      style <- "background-color: #3EA055; color:white;"
    } 
    else {
      style <-  "background-color: #F70D1A; color:white;" 
    }
    div(class="well", style=style, 
        IOFC %>% formatdollar() %>% strong() %>% h3(),
        h5("IOFC ($/cow/year)"))
  })  
  
  output$NAI <- renderUI({
    NAI <- impact_with_robot_salvage()
    if (NAI>0) { 
      style <- "background-color: #306EFF; color:white;"
    } 
    else {
      style <-  "background-color: #F70D1A; color:white;" 
    }
    div(class="well", style=style, 
        NAI %>% formatdollar() %>% strong() %>% h3(),
        h5("Net Impact ($/year)"))
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
       + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365
  })
  
  feed_current <- reactive({
    DMI_day() * input$cost_DM * 330 * input$herd_size
  })
  
  feed_robot <- reactive({
    (DMI_projected() * input$cost_DM + input$pellets * input$cost_pellets/2000) * 330 * herd_size2()
  })

  output$milk_feed <- renderUI({
     val <- (feed_robot() - feed_current() + milk_robot() -  milk_current()) 
     div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
         val %>% formatdollar2() %>% strong() %>% h4(),
         h5("under robot"))
  })  
  
  output$labor_repair <- renderUI({
    val <- (labor_robot() - labor_current() +inc_exp_repair())
    div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
        val %>% formatdollar2() %>% strong() %>% h4(),
        h5("under robot"))
  })  
  
  output$captial_cost <- renderUI({
    val <- inc_exp_capital_recovery() + capital_recovery_housing()
    div(class="well well-sm", style= "background-color: #64E986; color:white;", 
        val %>% formatdollar2() %>% strong() %>% h4(),
        h5("under robot"))
  })  
  
  
  output$plot1 <- renderPlot({ 
    a <- data.frame("vars"=c("feed","feed","milk", "milk"), 
                    "values"=c(feed_current(),feed_robot(),milk_current(),milk_robot())/1000,"type"= c(0,1,0,1)) 
    a$label <- apply(cbind(a$values),2,round,0)
    a$label <- apply(a$label, 2,formatcomma) 
    a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
    
    ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
   #   ggtitle("Milk vs Feed")+ 
      geom_text(aes(label=label, ymax=max(values)*1.1), 
              vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
    scale_fill_brewer(palette="Paired", breaks=c(1,0), labels=c("Robots","Current")) +
    theme_minimal() +
    scale_x_discrete(
      limits=c("feed","milk"),   
      labels=c("Feed \n Expenses","Milk \n Income")    
    ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),  #removes y-axis label
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      text=element_text(family="sans", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
      legend.title=element_blank(), 
      legend.position=c(0.85,0.10)
    )
  })
  
  output$plot2 <- renderPlot({ 
    a <- data.frame("vars"=c("repair","repair","labor", "labor"), 
                    "values"=c(0,inc_exp_repair(),labor_current(),labor_robot())/1000,"type"= c(0,1,0,1)) 
    
    a$label <- apply(cbind(a$values),2,round,0)
    a$label <- apply(a$label, 2,formatcomma) 
    a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
    
    ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
      geom_bar(stat="identity", position=position_dodge()) +
      coord_flip() +
   #   ggtitle("Labor vs Repair") + 
      geom_text(aes(label=label, ymax=max(values)*1.1), 
                vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
      scale_fill_brewer(palette="Reds", breaks=c(1,0), labels=c("Robots","Current")) +
      theme_minimal() +
      scale_x_discrete(
        limits=c("repair","labor"),   
        labels=c(" Repair \n Expenses","Labor \n Expenses")    
      ) + 
      theme(
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  #removes y-axis label
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="sans", size=14),                       #changes font on entire graph
        plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
        legend.title=element_blank(), 
        legend.position=c(0.85,0.10)
      )
  })
  
  output$plot3 <- renderPlot({ 
    a <- data.frame("vars"=c("capital_housing","capital_robot"), 
                    "values"=c(inc_exp_capital_recovery(),capital_recovery_housing())/1000,"type"= c(1,1)) 
    
    a$label <- apply(cbind(a$values),2,round,0)
    a$label <- apply(a$label, 2,formatcomma) 
    a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
    
    ggplot(data=a, aes(x=vars, y=values, fill=factor(type))) + 
    geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
    coord_flip() +
   # ggtitle("Cost of Capital") + 
    geom_text(aes(label=label,ymax=max(values)*1.0), 
              vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
    theme_minimal() + 
    scale_x_discrete(
      limits=c("capital_housing","capital_robot"),   
      labels=c("Housing \n Recovery","Robot \n Recovery")    
    ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),  #removes y-axis label
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      text=element_text(family="sans", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0))  #changes font face and location for graph title
    )
  })
  
  })


