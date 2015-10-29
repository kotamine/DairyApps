

## --- Rewritten version of session_variables.R,  session_variables.R, and session_budget.R --- 
## --- This file deals with most of the rendering for the base analysis 



##  Data Entry Level 


var_to_render <- list()               
var_to_render_0 <- c("herd_size2", "robot_invest","cost_housing","total_investment_cow","housing_years",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_robot_day")
var_to_render_1 <- c()
var_to_render_2 <- c("adj_milk_cow_day")
var_to_render_3 <- c("DMI_change","DMI_day","DMI_projected", "DMI_change_copy") 
var_to_render_4 <- c("stage_lactation")

var_to_render_0_right <- c("inc_rev_herd_size","inc_rev_per_cow", "inc_rev_milk_premium", "inc_rev_cull_sale",
                           "inc_rev_software", "inc_rev_total", "dec_exp_heat_detection",  "dec_exp_labor",
                           "dec_exp_labor_management",  "dec_exp_total",  "positive_total",
                           "inc_exp_herd_increase", "inc_exp_repair", "inc_exp_feed", "inc_exp_pellet", 
                           "inc_exp_replacement", "inc_exp_utilities", "inc_exp_record_management", 
                           "inc_exp_capital_recovery", "inc_exp_total", "negative_total",
                           "impact_without_housing", "impact_with_housing", "capital_recovery_housing",
                           "capital_recovery_total", "robot_end_PV",  "impact_with_robot_salvage",
                           "impact_with_inflation")

var_to_render[[1]] <- var_to_render_1
var_to_render[[2]] <- var_to_render_2
var_to_render[[3]] <- var_to_render_3
var_to_render[[4]] <- var_to_render_4


lapply(var_to_render_0, 
       function(x) { 
         output[[paste0(x)]] <- renderUI({ 
           rv[[paste0(x)]] %>% formatcomma() %>% helpText()  })
       }
)


for (r in 1:4) {
lapply(var_to_render[[r]], 
              function(x) { 
                if (length(var_to_render[[r]])>0) {
                  output[[paste0(x)]] <- renderUI({ 
                  rv[[paste0(x)]] %>% round(r) %>% helpText() })
                }
              }
              )
}


lapply(var_to_render_0_right, 
       function(x) { 
         output[[paste0(x)]] <- renderUI({ 
           rv[[paste0(x)]] %>% formatcomma() %>% helpText()  %>% span(align="right") })
       }
)


# replication of inputs
output$rep_milk_cow_day <- renderUI({
  input$milk_cow_day %>% formatcomma() %>% helpText() 
})

output$rep_milk_change <- renderUI({
  input$milk_change  %>% formatcomma() %>% helpText() 
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
                                rv$inc_rev_total- rv$dec_exp_labor_management )/
    ((rv$dec_exp_heat_detection + rv$dec_exp_labor )/input$labor_rate)
  rv$be_wage_with_salvage %>% formatdollar(2) %>% strong() %>% helpText() %>% span(align="right")
}) 


## Dashboard 

output$IOFC <- renderUI({
  if (input$IOFC=="per cow") {
    dash_IOFC(rv$IOFC, rv$IOFC2, basis=input$IOFC)
  } else {
    dash_IOFC(rv$IOFC_cwt, rv$IOFC2_cwt, basis=input$IOFC)
  }
})  

output$NAI <- renderUI({
  dash_NAI(rv$NAI,cutoff=0)
}) 

output$milk_feed <- renderUI({
  validate(
    need(!is.na(rv$milk_feed),"NA")
  )
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rv$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      h5("Milk Income - Feed Cost"), h5("under robot"))
})  

output$labor_repair <- renderUI({
  validate(
    need(!is.na(rv$labor_repair ),"NA")
  )
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rv$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      h5("Labor + Repair Cost"), h5("under robot"))
})  

output$captial_cost <- renderUI({
  validate(
    need(!is.na(rv$capital_cost ),"NA")
  )
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rv$capital_cost %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),  h5("under robot"))
})  

output$misc <- renderUI({
  validate(
    need(!is.na(rv$misc ),"NA")
  )
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rv$misc %>% formatdollar2() %>% strong %>% h4(), 
      h5("The Rest"), h5("under robot"))
})  

output$plot1 <- renderPlot({ 
  dash_plot1(rv$feed_current,rv$feed_robot,rv$milk_current,rv$milk_robot)
})

output$plot2 <- renderPlot({
  dash_plot2(rv$inc_exp_repair,rv$labor_current,rv$labor_robot) 
})

output$plot3 <- renderPlot({ 
  dash_plot3(rv$inc_exp_capital_recovery,rv$capital_recovery_housing,rv$robot_end_PV,input$NAI)  
})



