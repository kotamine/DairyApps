

## --- Rewritten version of session_variables.R,  session_variables.R, and session_budget.R --- 
## --- This file deals with most of the rendering for the base analysis 



##  Data Entry Level 


var_to_render <- list()               
var_to_render_0 <- c("herd_size2", "robot_invest","cost_housing","total_investment_cow","total_investment", "housing_years",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_robot_day")
var_to_render_1 <- c()
var_to_render_2 <- c("adj_milk_cow_day")
var_to_render_3 <- c("DMI_change","DMI_day","DMI_projected") 
var_to_render_4 <- c("stage_lactation")


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

  lapply(var_to_render_2, 
         function(x) { 
             output[[paste0(x)]] <- renderUI({ 
               rv[[paste0(x)]] %>% round(2) %>% helpText() })
         }
  )

  lapply(var_to_render_3, 
         function(x) { 
           output[[paste0(x)]] <- renderUI({ 
             rv[[paste0(x)]] %>% round(3) %>% helpText() })
         }
  )
  
  lapply(var_to_render_4, 
         function(x) { 
           output[[paste0(x)]] <- renderUI({ 
             rv[[paste0(x)]] %>% round(4) %>% helpText() })
         }
  )
  
# This doesn't work  
# for (r in 2:4) {
# lapply(var_to_render[[r]], 
#               function(x) { 
#                 if (length(var_to_render[[r]])>0) {
#                   output[[paste0(x)]] <- renderUI({ 
#                   rv[[paste0(x)]] %>% round(r) %>% helpText() })
#                 }
#               }
#               )
# }


var_to_render_0_right <- c("inc_rev_herd_size","inc_rev_per_cow", "inc_rev_milk_premium", "inc_rev_cull_sale",
                           "inc_rev_software", 
                           "inc_rev_total",
                           "dec_exp_heat_detection",  "dec_exp_labor",
                           "dec_exp_labor_management", 
                           "dec_exp_total", 
                           # "cash_positive_total",
                           "inc_exp_herd_increase", "inc_exp_repair", "inc_exp_feed", "inc_exp_pellet", 
                           "inc_exp_replacement", "inc_exp_utilities", "inc_exp_record_management", 
                           "inc_exp_total", 
                           "capital_recovery_robot", "capital_recovery_housing",
                           "capital_recovery_total",
                           # "cash_negative_total",
                           # "cash_impact_without_salvage", 
                           "robot_end_PV",  
                           # "cash_impact_with_salvage",
                           "impact_with_inflation")

var_to_render_0_right_sum <- c("cash_positive_total","cash_negative_total",
                           "cash_impact_without_salvage", "cash_impact_with_salvage")
  
inflation_factors <- c("inflation_margin", "inflation_robot",
                 "inflation_labor", "inflation_general")
assign_factors <- c(rep(1,6), rep(3,4), rep(1,8), rep(4,3), 
                     2, NaN) 
# assign_factors <- c(rep(1,6), rep(3,4), NaN, rep(1,8), rep(4,3), NaN,
                    # NaN, 2, NaN, NaN) 
# The order of factor assignments correspond to var_to_render_0_right

inflation <- lapply(assign_factors, function(x) { inflation_factors[x] })

names(inflation) <- var_to_render_0_right 


lapply(var_to_render_0_right, 
       function(x) { 
         output[[paste0(x)]] <- renderUI({
           if (input$cash_flow_on=="ON") {
           if (!is.null(inflation[[paste0(x)]])) {
             i_factor <- (1 + input[[inflation[[paste0(x)]]]]/100)^(input$budget_year - 1)
           } else {
             i_factor <- 1
           } 
             (rv[[paste0(x)]] * i_factor) %>% 
               formatdollar() %>% helpText(align="right") 
           } else {
             rv[[paste0(x)]] %>% 
             formatdollar() %>% helpText(align="right") 
           }
           })
       }
)

lapply( var_to_render_0_right_sum, function(x) {
  output[[paste0(x)]] <- renderUI({  
    rv[[paste0(x)]] %>% 
      formatdollar() %>% helpText(align="right") 
  })
})



# replication of inputs
output$rep_milk_cow_day <- renderUI({
  input$milk_cow_day %>% formatcomma() %>% helpText() 
})

output$rep_milk_change <- renderUI({
  input$milk_change  %>% formatcomma() %>% helpText() 
})             

output$DMI_change_copy <- renderUI({
  rv$DMI_change  %>% round(3) %>% helpText() 
})     

# --- Break-even wage rates ---

output$be_wage_without_salvage  <- renderUI({
  rv$be_wage_without_salvage <- (rv$negative_total + 
                               - rv$inc_rev_total - rv$dec_exp_labor_management)/
    ((rv$dec_exp_heat_detection + rv$dec_exp_labor)/input$labor_rate)
  rv$be_wage_without_salvage %>% formatdollar(2) %>% strong() %>% helpText(align="right")
}) 

output$be_wage_with_salvage  <- renderUI({
  rv$be_wage_with_salvage <- (rv$negative_total - rv$robot_end_PV -
                                rv$inc_rev_total- rv$dec_exp_labor_management )/
    ((rv$dec_exp_heat_detection + rv$dec_exp_labor )/input$labor_rate)
  rv$be_wage_with_salvage %>% formatdollar(2) %>% strong() %>% helpText(align="right")
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
  dash_plot3(rv$capital_recovery_robot,rv$capital_recovery_housing,rv$robot_end_PV,input$NAI)  
})


## Dashboard -- Cash Flow Based Representation

output$cash_IOFC <- renderUI({
  if (input$cash_IOFC=="per cow") {
    dash_IOFC(rv$cash_IOFC, rv$cash_IOFC2, basis=input$IOFC)
  } else {
    dash_IOFC(rv$cash_IOFC_cwt, rv$cash_IOFC2_cwt, basis=input$IOFC)
  }
})  

output$cash_NAI <- renderUI({
  dash_NAI(rv$cash_NAI,cutoff=0)
}) 

output$cash_milk_feed <- renderUI({
  validate(
    need(!is.na(rv$cash_milk_feed),"NA")
  )
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rv$cash_milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      h5("Milk Income - Feed Cost"), h5("under robot"))
})  

output$cash_labor_repair <- renderUI({
  validate(
    need(!is.na(rv$cash_labor_repair ),"NA")
  )
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rv$cash_labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      h5("Labor + Repair Cost"), h5("under robot"))
})  

output$cash_captial_cost <- renderUI({
  validate(
    need(!is.na(rv$cash_capital_cost ),"NA")
  )
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rv$cash_capital_cost %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),  h5("under robot"))
})  

output$cash_misc <- renderUI({
  validate(
    need(!is.na(rv$cash_misc ),"NA")
  )
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rv$cash_misc %>% formatdollar2() %>% strong %>% h4(), 
      h5("The Rest"), h5("under robot"))
})  

output$cash_plot1 <- renderPlot({ 
  dash_plot1(rv$cash_feed_current,rv$cash_feed_robot,rv$cash_milk_current,rv$cash_milk_robot)
})

output$cash_plot2 <- renderPlot({
  dash_plot2(rv$cash_inc_exp_repair,rv$cash_labor_current,rv$cash_labor_robot) 
})

output$cash_plot3 <- renderPlot({ 
  dash_plot3(rv$cash_capital_recovery_robot,rv$cash_capital_recovery_housing,
             rv$cash_robot_end_PV,input$cash_NAI)  
})




