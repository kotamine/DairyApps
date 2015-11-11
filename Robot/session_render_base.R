

## --- Rewritten version of session_variables.R,  session_variables.R, and session_budget.R --- 
## --- This file deals with most of the rendering for the base analysis 



##  Data Entry Level 


var_to_render <- list()               
var_to_render_0 <- c("herd_size2", "robot_invest","cost_housing","total_investment_cow",
                     "total_investment", "housing_years",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_robot_day",
                     "yr_robot2", "yr_robot3", "loan_housing",
                     "loan_robot1","loan_robot2","loan_robot3",
                     "copy_robot_salvage1","copy_robot_salvage2","copy_robot_salvage3")
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
  
  
#   
#   cash_render_0 <- c("yr_robot2", "yr_robot3", "loan_housing",
#                      "loan_robot1","loan_robot2","loan_robot3",
#                      "copy_robot_salvage1","copy_robot_salvage2","copy_robot_salvage3")
#   
  cash_render_0_right <- c("NPV","ANPV", "ANPVr", 
                           "capital_recovery_robot", "capital_recovery_housing",
                           "cost_downpayment2", "robot_end_PV",
                           "capital_cost_total", 
                           "positive_total","negative_total",
                           "positive_minus_negative",
                           "inflation_adjustment",
                           "net_annual_impact_before_tax",
                           "revenue_minus_expense", 
                           "tax_revenue_minus_expense",
                           "tax_interest",
                           "tax_depreciation",
                           "net_annual_impact_after_tax",
                           "cost_downpayment",
                           "adj_WACC_interest",
                           "adj_WACC_hurdle")
  
  rate_render_2_right <- c("IRR","MIRR","WACC")
  
  cash_render_2_right <- c("be_wage_positive_minus_negative", "bw_wage_before_tax", "bw_wage_after_tax")
  
  percent_render_3_right <- c("bw_wage_inflation_before_tax", "bw_wage_inflation_after_tax")
    
#   lapply(cash_render_0, function(x) {
#     output[[x]] <- renderUI({
#       rv[[x]] %>% round() %>% helpText()
#     })
#   })
  
  lapply(cash_render_0_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      rv[[paste0(x)]] %>% formatdollar() %>% helpText(align="right")
    })
  })
  
  lapply(cash_render_2_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      rv[[paste0(x)]] %>% formatdollar(2) %>% helpText(align="right")
    })
  })
  
  lapply(rate_render_2_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      rv[[paste0(x)]] %>% round(2) %>% helpText(align="right")
    })
  })
  
  lapply(percent_render_3_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      a <- (rv[[paste0(x)]]*100) %>% round(3)
      paste0(a,"%") %>% helpText(align="right")
    })
  })
  
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


var_to_render_0_right <- c("inc_rev_herd_size","inc_rev_per_cow", 
                           "inc_rev_milk_premium", "inc_rev_cull_sale",
                           "inc_rev_software", 
                           "inc_rev_total",
                           "dec_exp_heat_detection",  "dec_exp_labor",
                           "dec_exp_labor_management", 
                           "dec_exp_total", 
                           # "cash_positive_total",
                           "inc_exp_herd_increase", "inc_exp_repair", "inc_exp_feed", "inc_exp_pellet", 
                           "inc_exp_replacement", "inc_exp_utilities", "inc_exp_record_management", 
                           "inc_exp_total" ) 
#                            
#                            "capital_recovery_robot", "capital_recovery_housing",
#                            #"capital_recovery_total",
#                            # "cash_negative_total",
#                            # "cash_impact_without_salvage", 
#                            "cost_downpayment2", "robot_end_PV",
#                            "capital_cost_total"  
#                            # "cash_impact_with_salvage", 
#                            #"impact_with_inflation"
                           # )

# var_to_render_0_right_sum <- c("positive_total","negative_total",
#                                "inflation_adjustment",
#                                "net_annual_impact_before_tax",
# #                                "bw_wage_before_tax",
# #                                "bw_wage_inflation_before_tax",
#                                "revenue_minus_expense", 
#                                "tax_revenue_minus_expense",
#                                "tax_interest",
#                                "tax_depreciation",
#                                "net_annual_impact_after_tax")
# #                                "bw_wage_after_tax",
# #                                "bw_wage_inflation_after_tax"
#   

inflation_factors <- c("inflation_margin", "inflation_robot",
                 "inflation_labor", "inflation_general")
assign_factors <- c(rep(1,6), rep(3,4), rep(1,8)) #, rep(4,3), 2, NaN) 
# assign_factors <- c(rep(1,6), rep(3,4), NaN, rep(1,8), rep(4,3), NaN,
                    # NaN, 2, NaN, NaN) 
# The order of factor assignments correspond to var_to_render_0_right

inflation <- lapply(assign_factors, function(x) { inflation_factors[x] })
names(inflation) <- var_to_render_0_right 

lapply(var_to_render_0_right, 
       function(x) { 
         output[[paste0(x)]] <- renderUI({
           if (!is.null(inflation[[paste0(x)]])) {
             i_factor <- (1 + input[[inflation[[paste0(x)]]]]/100)^(input$budget_year - 1)
           } else {
             i_factor <- 1
           } 
             (rv[[paste0(x)]] * i_factor) %>% 
               formatdollar() %>% helpText(align="right") 
           })
       }
)

# lapply(var_to_render_0_right_sum, function(x) {
#   output[[paste0(x)]] <- renderUI({  
#     rv[[paste0(x)]] %>% 
#       formatdollar() %>% helpText(align="right") 
#   })
# })



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



## Dashboard 

output$IOFC <- renderUI({
  if (input$IOFC=="per cow") {
    dash_IOFC(rv$IOFC, rv$IOFC2, basis=input$IOFC)
  } else {
    dash_IOFC(rv$IOFC_cwt, rv$IOFC2_cwt, basis=input$IOFC)
  }
})  

# CREATE DASHBOARD FOR CASH FLOW STATS and Inflation Adjustment 


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
    need(!is.na(rv$capital),"NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      (rv$capital) %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),  h5("under robot"))
})  

output$misc <- renderUI({
  validate(
    need(!is.na(rv$misc ),"NA")
  )
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rv$misc %>% formatdollar2() %>% strong %>% h4(), 
      h5("Others"), h5("under robot"))
}) 

output$inflation <- renderUI({
  validate(
    need(!is.na(rv$inflation ),"NA")
  )
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rv$inflation %>% formatdollar2() %>% strong %>% h4(), 
      h5("Inflation Adjustments"), h5("under robot"))
})  

output$plot1 <- renderPlot({ 
  dash_plot1(rv$feed_current,rv$feed_robot,rv$milk_current,rv$milk_robot)
})

output$plot2 <- renderPlot({
  dash_plot2(rv$inc_exp_repair,rv$labor_current,rv$labor_robot) 
})

output$plot3 <- renderPlot({  
  dash_plot3(rv$capital_recovery_robot2,rv$capital_recovery_housing2,
             rv$cost_downpayment, rv$robot_end_PV)  
})


output$cashflow <- renderUI({
  validate(
    need(!is.na(rv$table_cash_flow ),"NA")
  )
  min <- min(rv$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  avg <- mean(rv$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  max <- max(rv$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
  sd <- sd(rv$table_cash_flow$after_tax_cash_flow) %>% formatdollar()
  pos <- paste0(round(sum(rv$table_cash_flow$after_tax_cash_flow>0)/input$horizon*100),"%")

  div(class="well well-sm", style= "background-color: 	#778899; color:white;", 
      h4("Cash Flow", align="center"),
      h5("Min:", min), 
      h5("Avg:", avg),
      h5("Max:", max),
      h5("S.D.:", sd),
      h5(pos, "stays positive"),
      h5("under robot")
      )
}) 


output$breakeven <- renderUI({ 
  validate(
    need(!is.na(rv$bw_wage_before_tax),"NA")
  )
  if (input$breakeven_option=="wage") {
    option <- "Wage:"
    if (input$NAI=="before tax") { 
      labor_rate <- rv$bw_wage_before_tax
    } else {
      labor_rate <- rv$bw_wage_after_tax
    }
    be_val <- paste0("$", round(labor_rate, 2))
    inflation <-  input$inflation_labor/100
  } else {
    option <- "Inflation:"
    if (input$NAI=="before tax") { 
      inflation <- rv$bw_wage_inflation_before_tax
    } else {
      inflation <- rv$bw_wage_inflation_after_tax
    }
    be_val <- paste0(round(inflation*100,3),"%") 
    labor_rate <- input$labor_rate
  }
  
  yr_one <- paste("Year", round(input$horizon/3), ": ")
  yr_two <- paste("Year", round(input$horizon*(2/3)),": ")
  yr_three <- paste("Year", round(input$horizon),": ")
  wage_zero <- labor_rate  %>% formatdollar(2)
  wage_one <- (labor_rate * (1 + inflation/100)^(round(input$horizon/3)-1))  %>% formatdollar(2)
  wage_two <- (labor_rate * (1 + inflation/100)^(round(input$horizon*2/3)-1))  %>% formatdollar(2)
  wage_three <- (labor_rate * (1 + inflation/100)^(round(input$horizon)-1))  %>% formatdollar(2)
  
  div(class="well well-sm", style= "background-color:	#778899; color:white;", 
      h4("Breakeven", option, be_val, align="center"),
      h5("Year 1: ", wage_zero),
      h5(yr_one, wage_one),
      h5(yr_two, wage_two),
      h5(yr_three, wage_three),
      h5("under robot")
  )
}) 


# 
# ## Dashboard -- Cash Flow Based Representation
# 
# output$cash_IOFC <- renderUI({
#   if (input$cash_IOFC=="per cow") {
#     dash_IOFC(rv$cash_IOFC, rv$cash_IOFC2, basis=input$IOFC)
#   } else {
#     dash_IOFC(rv$cash_IOFC_cwt, rv$cash_IOFC2_cwt, basis=input$IOFC)
#   }
# })  
# 
# output$cash_NAI <- renderUI({
#   dash_NAI(rv$cash_NAI,cutoff=0)
# }) 
# 
# output$cash_milk_feed <- renderUI({
#   validate(
#     need(!is.na(rv$cash_milk_feed),"NA")
#   )
#   div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
#       rv$cash_milk_feed %>% formatdollar2() %>% strong() %>% h4(),
#       h5("Milk Income - Feed Cost"), h5("under robot"))
# })  
# 
# output$cash_labor_repair <- renderUI({
#   validate(
#     need(!is.na(rv$cash_labor_repair ),"NA")
#   )
#   div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
#       rv$cash_labor_repair %>% formatdollar2() %>% strong() %>% h4(),
#       h5("Labor + Repair Cost"), h5("under robot"))
# })  
# 
# output$cash_captial_cost <- renderUI({
#   validate(
#     need(!is.na(rv$cash_capital_cost ),"NA")
#   )
#   div(class="well well-sm", style= "background-color: #64E986; color:white;", 
#       rv$cash_capital_cost %>% formatdollar2() %>% strong() %>% h4(),
#       h5("Cost of Capital"),  h5("under robot"))
# })  
# 
# output$cash_misc <- renderUI({
#   validate(
#     need(!is.na(rv$cash_misc ),"NA")
#   )
#   div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
#       rv$cash_misc %>% formatdollar2() %>% strong %>% h4(), 
#       h5("The Rest"), h5("under robot"))
# })  
# 
# output$cash_plot1 <- renderPlot({ 
#   dash_plot1(rv$cash_feed_current,rv$cash_feed_robot,rv$cash_milk_current,rv$cash_milk_robot)
# })
# 
# output$cash_plot2 <- renderPlot({
#   dash_plot2(rv$cash_inc_exp_repair,rv$cash_labor_current,rv$cash_labor_robot) 
# })
# 
# output$cash_plot3 <- renderPlot({ 
#   dash_plot3(rv$cash_capital_recovery_robot,rv$cash_capital_recovery_housing,
#              rv$cash_robot_end_PV,input$cash_NAI)  
# })


## -----------------------------------------------------------------------


lapply(c("table_cash_flow","table_debt","table_depreciation"), 
       function(x) {
         output[[x]] <- DT::renderDataTable({
           if (length(rv[[x]])==0) return()
           tbl <- round(rv[[x]])
           DT::datatable(tbl,
                         rownames = FALSE,
                         extensions = 'ColVis',
                         # extensions = 'ColReorder',
                         options = list(
                           dom = 'C<"clear">lfrtip',
                           scrollX = TRUE,
                           #                           scrollCollapse = TRUE,
                           #                           scrollY = 500,
                           # scrollCollapse = TRUE,
                           # colVis = list(exclude = c(0, 1,1,0),
                           showNone=TRUE, 
                           activate = 'mouseover'))
           
         })
       })





