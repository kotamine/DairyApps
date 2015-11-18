

## --- Rewritten version of session_variables.R,  session_variables.R, and session_budget.R --- 
## --- This file deals with most of the rendering for the base analysis 



##  Data Entry Level 


var_to_render <- list()               
var_to_render_0 <- c("herd_size2", "cost_milking","cost_housing","total_investment_cow",
                     "total_investment", "housing_years",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_alt_day",
                     "yr_robot2",  "loan_housing",
                     "loan_milking1","loan_milking2", 
                     "copy_cost_housing", "copy_cost_milking1", "copy_cost_milking2", 
                     "copy_salvage_milking1","copy_salvage_milking2",
                     "yr_invest_milking2_pr4", 
                     "copy_robot_years_pr4", "copy_n_robot_pr4", "copy_cost_robot_pr4",
                     "housing_years_pr2", "housing_years_pr3","housing_years_pr4",
                     "copy_cost_housing_pr1","copy_cost_housing_pr2","copy_cost_housing_pr3","copy_cost_housing_pr4",
                     "copy_cost_milking1_pr2",
                     "copy_cost_milking1_pr3",
                     "copy_cost_milking1_pr4", "copy_cost_milking2_pr4",
                     "loan_housing_pr1",
                     "loan_housing_pr2", "loan_milking1_pr2", 
                     "loan_housing_pr3", "loan_milking1_pr3", 
                     "loan_housing_pr4", "loan_milking1_pr4", "loan_milking2_pr4",
                     'copy_r_housing_pr1', 
                     'copy_r_housing_pr2', 'copy_r_milking1_pr2', 
                     'copy_r_housing_pr3', 'copy_r_milking1_pr3', 
                     'copy_r_housing_pr4', 'copy_r_milking1_pr4', 'copy_r_milking2_pr4',
                     "salvage_milking2_pr4" 
                     )
var_to_render_1 <- c("copy_r_housing", "copy_r_milking1","copy_r_milking2")
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
           rv[[paste0(x)]] %>% formatcomma(0) %>% helpText()  })
       }
)

lapply(var_to_render_1, 
       function(x) { 
         output[[paste0(x)]] <- renderUI({ 
           rv[[paste0(x)]] %>% round(1) %>% helpText() })
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
      rv[[paste0(x)]] %>% formatdollar() %>% helpText() %>% div(align="right")
    })
  })
  
  lapply(cash_render_2_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      rv[[paste0(x)]] %>% formatdollar(2) %>% helpText() %>% div(align="right")
    })
  })
  
  lapply(rate_render_2_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      rv[[paste0(x)]] %>% round(2) %>% helpText() %>% div(align="right")
    })
  })
  
  lapply(percent_render_3_right, function(x) {
    output[[paste0(x)]] <- renderUI({
      validate(
        need(!is.na(rv[[paste0(x)]]), "NA")
      )
      a <- (rv[[paste0(x)]]*100) %>% round(3)
      paste0(a,"%") %>% helpText() %>% div(align="right")
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
               formatdollar() %>% helpText() %>% div(align="right")
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



## ------------ Dashboard ------------

robot_or_parlor <- reactive({
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots")
  {   return("Robots")
  } else {
    if (input$profile_choice=="Barn Only") {
      return("New Barn")
    } else if (input$profile_choice=="Retrofit Parlors") {
      return("Retrofit")
  } else {
    return("New Parlors")
  }
  }
})
  
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
      h5("Milk Income - Feed Cost"), h5("under", robot_or_parlor()))
})  

output$labor_repair <- renderUI({
  validate(
    need(!is.na(rv$labor_repair ),"NA")
  )
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rv$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      h5("Labor + Repair Cost"),  h5("under", robot_or_parlor()))
})  

output$captial_cost <- renderUI({
  validate(
    need(!is.na(rv$capital),"NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      (rv$capital) %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),   h5("under", robot_or_parlor()))
})  

output$misc <- renderUI({
  validate(
    need(!is.na(rv$misc ),"NA")
  )
  div(class="well well-sm", style= "background-color: #EDDA74; color:white;", 
      rv$misc %>% formatdollar2() %>% strong %>% h4(), 
      h5("Others"),  h5("under", robot_or_parlor()))
}) 

output$inflation <- renderUI({
  validate(
    need(!is.na(rv$inflation ),"NA")
  )
  div(class="well well-sm", style= "background-color: #7A5DC7; color:white;", 
      rv$inflation %>% formatdollar2() %>% strong %>% h4(), 
      h5("Inflation Adjustments"),  h5("under", robot_or_parlor()))
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
      h5("under", robot_or_parlor())
      )
}) 



output$cashflow2 <- renderGvis({
  if (length(rv[["table_cash_flow"]])==0) return()
  tbl <- round(rv[["table_cash_flow"]])
  tbl$Year <- tbl$year
  tbl$Cashflow <- tbl$after_tax_cash_flow 
  gvisAreaChart(tbl, xvar="Year", 
                yvar=c("Cashflow"),
                options=list(
                             title="After-tax Cash Flow", 
                             vAxis= paste("{title:'Impact under", robot_or_parlor(),"($)'}"),
                             hAxis="{title:'Year'}",
                             legend="none"
                ))
})

# output$cashflow2 <- renderGvis({
#   if (length(rv[["table_cash_flow"]])==0) return()
#   cashflow <- rv$table_cash_flow$after_tax_cash_flow 
#   cut1 <- round(input$horizon/3)
#   cut2 <- round(input$horizon*2/3)
#   list_per <- list(c(1:cut1),c((cut1+1):cut2), c((cut2+1):input$horizon)) 
# 
#   df <- data.frame(period=c(paste0("1-",cut1),paste0(cut1+1,"-",cut2),
#                             paste0(cut2+1,"-",input$horizon)))
#   for (n in 1:3) {
#     df$low[n] <- quantile(cashflow[list_per[[n]]],.01) %>% round() 
#     df$close[n] <- quantile(cashflow[list_per[[n]]],.25) %>% round() 
#     df$open[n] <- quantile(cashflow[list_per[[n]]],.75) %>% round() 
#     df$high[n] <- quantile(cashflow[list_per[[n]]],.99) %>% round() 
#   }
#   gvisCandlestickChart(df,
#                        xvar="period",
#                        low="low", 
#                        close="close",
#                        open="open",
#                        high="high",
#                 options=list(legend="none"                
#                              ))
# })


output$breakeven2 <- renderGvis({
  validate(
    need(!is.na(rv$bw_wage_before_tax),"NA")
  )
  if (input$NAI=="before tax") { 
    labor_rate <- rv$bw_wage_before_tax
    inflation <- rv$bw_wage_inflation_before_tax
    tax <- "(Before Tax)"
  } else {
    labor_rate <- rv$bw_wage_after_tax 
    inflation <- rv$bw_wage_inflation_after_tax
    tax <- "(After Tax)"
  }
  
  df <- data.frame(Year=c(1:input$horizon))
  df$Base <- lapply(c(1:input$horizon), function(t) { 
    input$labor_rate * (1 + input$inflation_labor/100)^(t-1)
  }) %>% unlist() %>% round(2)
  df$Wage <- lapply(c(1:input$horizon), function(t) { 
    labor_rate * (1 + input$inflation_labor/100)^(t-1)
    }) %>% unlist() %>% round(2)
  df$Wage_Inflation <- lapply(c(1:input$horizon), function(t) { 
     input$labor_rate * (1 + inflation)^(t-1)
  }) %>% unlist() %>% round(2)
  
  gvisLineChart(df, xvar="Year", 
                yvar=c("Base", "Wage","Wage_Inflation"),
                options=list(
                  title=paste("Breakeven Wage for",robot_or_parlor(),tax), 
                  vAxis="{title:'Wage Trajectory ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom"
                ))
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
  wage_one <- (labor_rate * (1 + inflation)^(round(input$horizon/3)-1))  %>% formatdollar(2)
  wage_two <- (labor_rate * (1 + inflation)^(round(input$horizon*2/3)-1))  %>% formatdollar(2)
  wage_three <- (labor_rate * (1 + inflation)^(round(input$horizon)-1))  %>% formatdollar(2)
  
  div(class="well well-sm", style= "background-color:	#778899; color:white;", 
      h4("Breakeven", option, be_val, align="center"),
      h5("Year 1: ", wage_zero),
      h5(yr_one, wage_one),
      h5(yr_two, wage_two),
      h5(yr_three, wage_three),
      h5("under", robot_or_parlor())
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


# lapply(c("table_cash_flow","table_debt"), 
#        function(x) {
#          output[[x]] <- DT::renderDataTable({
#            if (length(rv[[x]])==0) return()
#            tbl <- round(rv[[x]])
#            DT::datatable(tbl, 
#                          # colnames = c('Here', 'Are', 'Some', 'New', 'Names')
#                          rownames = FALSE,
#                          extensions = 'ColVis',
#                          # extensions = 'ColReorder',
#                          options = list(
#                            dom = 'C<"clear">lfrtip',
#                            scrollX = TRUE,
#                            pageLength = 30,
#                            lengthMenu = c(10, 20, 30, 40),
#                            searching = FALSE,
#                            #                           scrollCollapse = TRUE,
#                            #                           scrollY = 500,
#                            # scrollCollapse = TRUE,
#                            # colVis = list(exclude = c(0, 1,1,0),
#                            showNone=TRUE, 
#                            activate = 'mouseover'))
#            
#          })
#        })


output$table_cash_flow <- DT::renderDataTable({
  if (length(rv[["table_cash_flow"]])==0) return()
  tbl <- round(rv[["table_cash_flow"]])
  colnames(tbl) <- c('Year', 'Revenue minus Expense', 'Interests on Debt', 'Depreciation',
                     'Operating Income', 'Income Tax','Principal Payments','Adding Back Depreciation',
                     'Down-payments','Salvage Values','After-tax Cashflow')
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = rv$housing_years+1,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(c( 'Revenue minus Expense', 'Interests on Debt', 'Depreciation',
                      'Operating Income', 'Income Tax','Principal Payments','Adding Back Depreciation',
                      'Down-payments','Salvage Values','After-tax Cashflow')) %>%
    formatStyle( 
      'After-tax Cashflow',
      fontWeight = c('bold'),
      color =  styleInterval(0, c('gray', 'white')),
      backgroundColor = styleInterval(0, c('yellow', 'lightblue'))) %>%
    
     formatStyle( 
          'Operating Income',
          fontWeight = c('bold'),
          color =  styleInterval(0.001, c('gray', 'white')),
          backgroundColor = styleInterval(0.001, c('yellow', 'lightblue')))
})  

output$table_debt <- DT::renderDataTable({
  if (length(rv[["table_debt"]])==0) return()
  tbl <- round(rv[["table_debt"]])
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { milk_sys <- 'Robot' }
  else { milk_sys <- 'Parlor'}
  colnames(tbl) <- c('Year', paste(milk_sys,'Payment Year'),paste(milk_sys, 'Interest'), paste(milk_sys,'Principal'), 
                     'Housing Payment Year','Housing Interest', 'Housing Principal',
                     'Interest Total', 'Principal Total') 
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = rv$housing_years,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(c(paste(milk_sys,'Interest'), paste(milk_sys,'Principal'), 
                     'Housing Interest', 'Housing Principal',
                     'Interest Total', 'Principal Total'))
})

output$table_depreciation <- DT::renderDataTable({
  if (length(rv[["table_depreciation"]])==0) return()
  tbl <- round(rv[["table_depreciation"]])
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") { milk_sys <- 'Robot' }
  else { milk_sys <- 'Parlor'}
  colnames(tbl) <- c('Year', milk_sys, 'Housing', 'Total')
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = rv$housing_years,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(c(milk_sys, 'Housing', 'Total'))
})




output$cashflow_chart <- renderGvis({
  if (length(rv[["table_cash_flow"]])==0) return()
  tbl <- round(rv[["table_cash_flow"]])
  tbl$Year <- tbl$year
  tbl$Operating_Income <- tbl$operating_income
  tbl$Cashflow <- tbl$after_tax_cash_flow 
  gvisLineChart(tbl, xvar="Year", 
                         yvar=c("Cashflow","Operating_Income"),
                         options=list(title="Before-tax Operating Income & After-tax Cash Flow", 
                                      vAxis="{title:'Net Annual Impact under Robot ($)'}",
                                      hAxis="{title:'Year'}",
                                      legend="bottom",
                                      width=800, height=400
                                        ))
#       gvisAreaChart(tbl, xvar="Year", 
#                        yvar=c("Cashflow","Operating_Income"),
#                        options=list(isStacked=TRUE,
#                                     title="Before-tax Operating Income & After-tax Cash Flow", 
#                                     vAxis="{title:'Net Annual Impact under Robot ($)'}",
#                                     hAxis="{title:'Year'}",
#                                     legend="bottom",
#                                     width=800, height=400
#                                       ))
})


output$copy_profile_choice1 <- renderUI({ 
    div(h4("Selected Investment Profile:"), 
        h4(input$profile_choice), align="center")
}) 
        
output$copy_profile_choice2 <- renderUI({
  div(h4("Selected Investment Profile:"), 
      h4(input$profile_choice), align="center")
}) 

observeEvent(input$profile_choice, {
  updateSelectInput(session,"copy_profile_choice1",  selected=input$profile_choice,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
  
  updateSelectInput(session,"copy_profile_choice2",  selected=input$profile_choice,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
})

observeEvent(input$copy_profile_choice1, {
  updateSelectInput(session,"profile_choice",  selected=input$copy_profile_choice1,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
  
  updateSelectInput(session,"copy_profile_choice2",  selected=input$copy_profile_choice1,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
})


observeEvent(input$copy_profile_choice2, {
  updateSelectInput(session,"copy_profile_choice1",  selected=input$copy_profile_choice2,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
  
  updateSelectInput(session,"profile_choice",  selected=input$copy_profile_choice2,
                    choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
})

