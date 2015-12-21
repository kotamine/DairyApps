

## rendering UI 



var_to_render_0 <- c("herd_size2", "cost_milking","cost_housing","total_investment_cow",
                     "total_investment", "planning_horizon",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_alt_day",
                     "yr_milking2",  "loan_housing",
                     "loan_milking1","loan_milking2", 
                     "copy_cost_housing", "copy_cost_milking1", "copy_cost_milking2", 
                     "copy_salvage_milking1","copy_salvage_milking2",
                     "yr_system2", 
                     "copy_milking_years", "copy_n_robot", "copy_cost_robot")
var_to_render_1 <- c()
var_to_render_2 <- c("r_milking2","adj_milk_cow_day")
var_to_render_3 <- c("DMI_change","DMI_day","DMI_projected") 
var_to_render_4 <- c("stage_lactation")

var_to_render <- list(var_to_render_1, var_to_render_2, var_to_render_3, var_to_render_4)  


pb_render_right_0 <- c("NPV","ANPV", "ANPVr", 
                       "capital_recovery_milking", "capital_recovery_housing",
                       "cost_downpayment", "salvage_milking_PV",
                       "capital_cost_total", 
                       "tax_revenue_minus_expense",
                       "tax_interest",
                       "tax_depreciation",
                       "net_annual_impact_after_tax",
                       "cost_downpayment",
                       "adj_WACC_interest",
                       "adj_WACC_hurdle")

# These variables are stored in a separate list, e.g., ans[["Robots_pb"]]  etc.   
# This avoids uncessarily triggering other calculations. 
pb_separate <- c("pb_positive_total","pb_negative_total",
                  "pb_positive_minus_negative",
                  "pb_inflation_adjustment",
                  "pb_revenue_minus_expense",
                  "pb_net_annual_impact_before_tax") 

rate_render_right_2 <- c("IRR","MIRR","WACC","ROI")

bw_render_right_1 <- c()
bw_render_right_2 <- c("bw_wage_positive_minus_negative", "bw_wage_before_tax", "bw_wage_after_tax")
bw_render_right_3 <- c("bw_wage_inflation_before_tax", "bw_wage_inflation_after_tax")
bw_render_right <- list(bw_render_right_1, bw_render_right_2, bw_render_right_3)


pb_var_to_render_right <- c("inc_rev_herd_size","inc_rev_per_cow", 
                           "inc_rev_milk_premium", "inc_rev_cull_sale",
                           "inc_rev_software", 
                           "inc_rev_total",
                           "dec_exp_heat_detection",  "dec_exp_labor",
                           "dec_exp_labor_management", 
                           "dec_exp_total", 
                           "inc_exp_herd_increase", "inc_exp_repair", "inc_exp_feed", "inc_exp_pellet", 
                           "inc_exp_replacement", "inc_exp_utilities", "inc_exp_record_management", 
                           "inc_exp_total" ) 

inflation_factors <- c("inflation_margin", "inflation_robot", "inflation_labor")
assign_factors <- c(rep(1,6), rep(3,4), rep(1,8)) 

# The order of factor assignments correspond to var_to_render_0_right
inflation <- lapply(assign_factors, function(z) { inflation_factors[z] })
names(inflation) <- pb_var_to_render_right



"CF_invest_housing"
"CF_loan_housing"
"CF_r_housing"
"CF_down_housing"
"CF_hr_housing"

"yr_milking2",  "loan_housing",
"loan_milking1","loan_milking2", 
"copy_cost_housing", "copy_cost_milking1", "copy_cost_milking2", 
"copy_salvage_milking1","copy_salvage_milking2",


lapply(CF_var_dollar, 
       function(z) { 
         output[[paste0(z,x)]] <- renderUI({ 
           ans[[x]][[paste0(z)]] %>% formatcomma(0,dollar=TRUE) %>% helpText()  })
       }
)


"CF_WACC_formula"

"CF_NPV_formula"
"CF_annuity_formula"
"CF_ROI"


## ---- producing rendering functions ----
lapply(base_profiles, function(x) {
  
lapply(var_to_render_0, 
       function(z) { 
         output[[paste0(z,x)]] <- renderUI({ 
           ans[[x]][[paste0(z)]] %>% formatcomma(0) %>% helpText()  })
       }
)

lapply(c(1:4), function(r) { 
  lapply(var_to_render[[r]], 
         function(z) { 
           output[[paste0(z,x)]] <- renderUI({ 
             ans[[x]][[paste0(z)]] %>% round(r) %>% helpText() })
         }
  )
})

lapply(pb_render_right_0, function(z) {
  output[[paste0(z,x)]] <- renderUI({
    need(!is.null(ans[[x]][[paste0(z)]]), "NA") %>% validate()
    ans[[x]][[paste0(z)]] %>% formatdollar() %>% helpText() %>% div(align="right")
  })
}) 

lapply(rate_render_right_2, function(z) {
  output[[paste0(z,x)]] <- renderUI({
      need(!is.null(ans[[x]][[paste0(z)]]), "NA") %>% validate()
    ans[[x]][[paste0(z)]] %>% round(2) %>% helpText() %>% div(align="right")
  })
})

lapply(percent_render_right_3, function(z) {
  output[[paste0(z,x)]] <- renderUI({
      need(!is.null(ans[[x]][[paste0(z)]]), "NA") %>% validate()
    a <- (ans[[x]][[paste0(z,x)]]*100) %>% round(3)
    paste0(a,"%") %>% helpText() %>% div(align="right")
  })
})


lapply(pb_var_to_render_right, 
       function(z) { 
         output[[paste0(z,x)]] <- renderUI({
           if (!is.null(inflation[[paste0(z)]])) {
             i_factor <- (1 + input[[inflation[[paste0(z)]]]]/100)^(input[[paste0("budget_year",x)]] - 1)
           } else {
             i_factor <- 1
           } 
           (ans[[x]][[paste0(z)]] * i_factor) %>% 
             formatdollar() %>% helpText() %>% div(align="right")
         })
       }
)


lapply(pb_separate, 
       function(z) { 
         output[[paste0(z,x)]] <- renderUI({ 
           ans[[paste0(x,"_pb")]][[paste0(z)]]  %>% 
             formatdollar() %>% helpText() %>% div(align="right")
           })
       }
)

lapply(c(1:3), function(r) {
  lapply(bw_render_right[[r]], function(z) {
    output[[paste0(z,x)]] <- renderUI({
      need(!is.null(ans[[paste0(x,"_bw")]][[paste0(z)]]), "NA") %>% validate()
      a <- (ans[[paste0(x,"_bw")]][[paste0(z)]]*100) %>% round(r)
      paste0(a,"%") %>% helpText() %>% div(align="right")
    })
  })
})


output$DMI_change_copy <- renderUI({
 ans[[x]]$DMI_change  %>% round(3) %>% helpText() 
})  

})

# replicated variables 
output$rep_milk_cow_day <- renderUI({
  input$milk_cow_day %>% formatcomma() %>% helpText() 
})

output$rep_milk_change <- renderUI({
  input[[paste0("milk_change","Robots")]]  %>% formatcomma() %>% helpText() 
})             

 


## ------------ Dashboard ------------
lapply(base_profiles, function(x) { 
  
output[[paste0("IOFC",x)]] <- renderUI({
  if (input$IOFC=="per cow") {
    dash_IOFC(ans[[paste0(x,"_da")]]$IOFC, ans[[paste0(x,"_da")]]$IOFC2, basis=input$IOFC)
  } else {
    dash_IOFC(ans[[paste0(x,"_da")]]$IOFC_cwt, ans[[paste0(x,"_da")]]$IOFC2_cwt, basis=input$IOFC)
  }
})  


output[[paste0("NAI",x)]] <- renderUI({ 
  dash_NAI(ans[[paste0(x,"_da")]]$NAI,cutoff=0)
}) 

output[[paste0("milk_feed",x)]] <- renderUI({
    need(!is.null(ans[[paste0(x,"_da")]]$milk_feed),"NA") %>% validate()
  
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      ans[[paste0(x,"_da")]]$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      h5("Milk Income - Feed Cost"), h5("under", robot_or_parlor()))
})  

output[[paste0("labor_repair",x)]] <- renderUI({
    need(!is.null(ans[[paste0(x,"_da")]]$labor_repair ),"NA")  %>% validate()
  
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      ans[[paste0(x,"_da")]]$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      h5("Labor + Repair Cost"),  h5("under", robot_or_parlor()))
})  

output[[paste0("captial_cost",x)]] <- renderUI({
    need(!is.null(ans[[paste0(x,"_da")]]$capital),"NA") %>% validate()
  
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      (ans[[paste0(x,"_da")]]$capital) %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),   h5("under", robot_or_parlor()))
})  

output[[paste0("misc",x)]] <- renderUI({
    need(!is.null(ans[[paste0(x,"_da")]]$misc ),"NA") %>% validate()
  
  div(class="well well-sm", style= "background-color: #EDDA74; color:white;", 
      ans[[paste0(x,"_da")]]$misc %>% formatdollar2() %>% strong %>% h4(), 
      h5("Others"),  h5("under", robot_or_parlor()))
}) 

output[[paste0("inflation",x)]] <- renderUI({
    need(!is.null(ans[[paste0(x,"_da")]]$inflation ),"NA") %>% validate()

  div(class="well well-sm", style= "background-color: #7A5DC7; color:white;", 
      ans[[paste0(x,"_da")]]$inflation %>% formatdollar2() %>% strong %>% h4(), 
      h5("Inflation Adjustments"),  h5("under", robot_or_parlor()))
})  

output[[paste0("plot1",x)]] <- renderPlot({ 
  dash_plot1(ans[[paste0(x,"_da")]]$feed_current,ans[[paste0(x,"_da")]]$feed_robot,
             ans[[paste0(x,"_da")]]$milk_current,ans[[paste0(x,"_da")]]$milk_robot)
})

output[[paste0("plot2",x)]]<- renderPlot({
  dash_plot2(ans[[paste0(x,"_da")]]$inc_exp_repair,ans[[paste0(x,"_da")]]$labor_current,
             ans[[paste0(x,"_da")]]$labor_robot) 
})

output[[paste0("plot3",x)]] <- renderPlot({  
  dash_plot3(ans[[paste0(x,"_da")]]$capital_recovery_robot2,ans[[paste0(x,"_da")]]$capital_recovery_housing2,
             ans[[paste0(x,"_da")]]$cost_downpayment, ans[[paste0(x,"_da")]]$robot_end_PV)  
})

output[[paste0("cashflow_chart",x)]] <- renderGvis({
  if (length(ans[[x]][["table_cash_flow"]])==0) return()
  tbl <- round(ans[[x]][["table_cash_flow"]])
  n_year1 <- ans[[x]]$planning_horizon + 1
  df <- data.frame(Year=c(1:n_year1))
  if (input$NAI=="before tax") {
    tax_status <- "Before-tax"
    df$Cash_flow <- ans[[x]][["table_cash_flow"]]$before_tax_cash_flow 
  } else {
    tax_status <- "After-tax"
    df$Cash_flow <- ans[[x]][["table_cash_flow"]]$after_tax_cash_flow
  }
  gvisAreaChart(df, xvar="Year", 
                yvar=c("Cash_flow"),
                options=list(
                  title=paste(tax_status, "Cash Flow"), 
                  vAxis= paste("{title:'Impact under", refProfileName(x), "($)'}"),
                  hAxis="{title:'Year'}",
                  legend="none"
                ))
})

})


# 
# output$cashflow <- renderUI({
#   validate(
#     need(!is.null(ans[[x]]$table_cash_flow ),"NA")
#   )
#   min <- min(ans[[x]]$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
#   avg <- mean(ans[[x]]$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
#   max <- max(ans[[x]]$table_cash_flow$after_tax_cash_flow) %>% formatdollar2() 
#   sd <- sd(ans[[x]]$table_cash_flow$after_tax_cash_flow) %>% formatdollar()
#   pos <- paste0(round(sum(ans[[x]]$table_cash_flow$after_tax_cash_flow>0)/ans[[x]]$housing_years*100),"%")
#   
#   div(class="well well-sm", style= "background-color: 	#778899; color:white;", 
#       h4("Cash Flow", align="center"),
#       h5("Min:", min), 
#       h5("Avg:", avg),
#       h5("Max:", max),
#       h5("S.D.:", sd),
#       h5(pos, "stays positive"),
#       h5("under", robot_or_parlor())
#   )
# }) 
# 
# 
# 



# 
# 
# output$breakeven2 <- renderGvis({
#   validate(
#     need(!is.null(ans[[x]]$bw_wage_before_tax),"NA")
#   )
#   if (input$NAI=="before tax") { 
#     labor_rate <- ans[[x]]$bw_wage_before_tax
#     inflation <- ans[[x]]$bw_wage_inflation_before_tax
#     tax <- "(Before Tax)"
#   } else {
#     labor_rate <- ans[[x]]$bw_wage_after_tax 
#     inflation <- ans[[x]]$bw_wage_inflation_after_tax
#     tax <- "(After Tax)"
#   }
#   
#   df <- data.frame(Year=c(1:ans[[x]]$housing_years))
#   df$Base <- lapply(c(1:ans[[x]]$housing_years), function(t) { 
#     input$labor_rate * (1 + input$inflation_labor/100)^(t-1)
#   }) %>% unlist() %>% round(2)
#   df$Wage <- lapply(c(1:ans[[x]]$housing_years), function(t) { 
#     labor_rate * (1 + input$inflation_labor/100)^(t-1)
#   }) %>% unlist() %>% round(2)
#   df$Wage_Inflation <- lapply(c(1:ans[[x]]$housing_years), function(t) { 
#     input$labor_rate * (1 + inflation)^(t-1)
#   }) %>% unlist() %>% round(2)
#   
#   gvisLineChart(df, xvar="Year", 
#                 yvar=c("Base", "Wage","Wage_Inflation"),
#                 options=list(
#                   title=paste("Breakeven Wage for",robot_or_parlor(),tax), 
#                   vAxis="{title:'Wage Trajectory ($)'}",
#                   hAxis="{title:'Year'}",
#                   legend="bottom"
#                 ))
# })
# 
# output$breakeven <- renderUI({ 
#   validate(
#     need(!is.null(ans[[x]]$bw_wage_before_tax),"NA")
#   )
#   if (input$breakeven_option=="wage") {
#     option <- "Wage:"
#     if (input$NAI=="before tax") { 
#       labor_rate <- ans[[x]]$bw_wage_before_tax
#     } else {
#       labor_rate <- ans[[x]]$bw_wage_after_tax
#     }
#     be_val <- paste0("$", round(labor_rate, 2))
#     inflation <-  input$inflation_labor/100
#   } else {
#     option <- "Inflation:"
#     if (input$NAI=="before tax") { 
#       inflation <- ans[[x]]$bw_wage_inflation_before_tax
#     } else {
#       inflation <- ans[[x]]$bw_wage_inflation_after_tax
#     }
#     be_val <- paste0(round(inflation*100,3),"%") 
#     labor_rate <- input$labor_rate
#   }
#   
#   yr_one <- paste("Year", round(ans[[x]]$housing_years/3), ": ")
#   yr_two <- paste("Year", round(ans[[x]]$housing_years*(2/3)),": ")
#   yr_three <- paste("Year", round(ans[[x]]$housing_years),": ")
#   wage_zero <- labor_rate  %>% formatdollar(2)
#   wage_one <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years/3)-1))  %>% formatdollar(2)
#   wage_two <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years*2/3)-1))  %>% formatdollar(2)
#   wage_three <- (labor_rate * (1 + inflation)^(round(ans[[x]]$housing_years)-1))  %>% formatdollar(2)
#   
#   div(class="well well-sm", style= "background-color:	#778899; color:white;", 
#       h4("Breakeven", option, be_val, align="center"),
#       h5("Year 1: ", wage_zero),
#       h5(yr_one, wage_one),
#       h5(yr_two, wage_two),
#       h5(yr_three, wage_three),
#       h5("under", robot_or_parlor())
#   )
# }) 
# 





##  --- cash flow tables ---
lapply(base_profiles, function(x) { 
  
output[[paste0("download_table_cash_flow",x)]] <- downloadHandler( 
  #  --- simpler version with .csv ---
  #   filename = function() { paste('cash_flow.csv') },
  #   content = function(file) {
  #     write.csv(df$table_cash_flow, file)
  #   }
  
  filename = paste0("cash_flow_",x,"_milking_system.xlsx"), 
  content = function(file) { 
    wb <- XLConnect::loadWorkbook(file, create = TRUE)
    XLConnect::createSheet(wb, name = "cashflow")
    XLConnect::createSheet(wb, name = "debt")
    XLConnect::createSheet(wb, name = "depreciation")
    XLConnect::writeWorksheet(wb, ans[[x]]$table_cash_flow, sheet = "cashflow") 
    XLConnect::writeWorksheet(wb, ans[[x]]$table_debt, sheet = "debt") 
    XLConnect::writeWorksheet(wb, ans[[x]]$table_depreciation, sheet = "depreciation") 
    XLConnect::saveWorkbook(wb)
  }
) 

output[[paste0("table_cash_flow",x)]] <- DT::renderDataTable({
  if (length(ans[[x]][["table_cash_flow"]])==0) return()
  tbl <- round(ans[[x]][["table_cash_flow"]])
  colnames(tbl) <- c('Year', 'Revenue minus Expense', 'Interests on Debt', 'Depreciation',
                     'Operating Income', 'Income Tax','Principal Payments','Adding Back Depreciation',
                     'Down-payments','Salvage Values','After-tax Cashflow','Before-tax Cashflow')
  L <- length(tbl[,1])
  df$table_cash_flow <- tbl
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = L,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  activate = 'mouseover')) %>% 
    formatCurrency(c( 'Revenue minus Expense', 'Interests on Debt', 'Depreciation',
                      'Operating Income', 'Income Tax','Principal Payments','Adding Back Depreciation',
                      'Down-payments','Salvage Values','After-tax Cashflow','Before-tax Cashflow')) %>%
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

output[[paste0("table_debt",x)]] <- DT::renderDataTable({
  if (length(ans[[x]][["table_debt"]])==0) return()
  tbl <- round(ans[[x]][["table_debt"]])
  milk_sys <- refProfileName(x)
  colnames(tbl) <- c('Year', paste(milk_sys,'Payment Year'),paste(milk_sys, 'Interest'), paste(milk_sys,'Principal'), 
                     'Housing Payment Year','Housing Interest', 'Housing Principal',
                     'Interest Total', 'Principal Total') 
  L <- length(tbl[,1])
  df$table_debt <- tbl
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = ans[[x]]$housing_years,
                  pageLength = L,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(c(paste(milk_sys,'Interest'), paste(milk_sys,'Principal'), 
                     'Housing Interest', 'Housing Principal',
                     'Interest Total', 'Principal Total'))
})

output[[paste0("table_depreciation",x)]] <- DT::renderDataTable({
  if (length(ans[[x]][["table_depreciation"]])==0) return()
  tbl <- round(ans[[x]][["table_depreciation"]])
  milk_sys <- refProfileName(x)
  
  colnames(tbl) <- c('Year', milk_sys, 'Housing', 'Total')
  L <- length(tbl[,1])
  df$table_depreciation <- tbl
  
  DT::datatable(tbl, 
                # colnames = c('Year', 'Robot', 'Housing', 'Total'), 
                rownames = FALSE,
                extensions = 'ColVis',
                options = list(
                  dom = 'C<"clear">lfrtip',
                  scrollX = TRUE,
                  pageLength = L,
                  lengthMenu = c(10, 20, 30, 40),
                  searching = FALSE,
                  showNone=TRUE, 
                  activate = 'mouseover')) %>% 
    formatCurrency(c(milk_sys, 'Housing', 'Total'))
})


output[[paste0("cashflow_chart",x)]] <- renderGvis({ 
  if (length(ans[[x]][["table_cash_flow"]])==0) return()
  tbl <- round(ans[[x]][["table_cash_flow"]])
  tbl$Year <- tbl$year
  tbl$Operating_Income <- tbl$operating_income
  tbl$Cashflow <- tbl$after_tax_cash_flow 
#   gvisLineChart(tbl, xvar="Year",  
#                 yvar=c("Cashflow","Operating_Income"),
#                 options=list(title="Before-tax Operating Income & After-tax Cash Flow", 
#                              vAxis=paste("{title:'Net Annual Impact under", robot_or_parlor()," ($)'}"),
#                              hAxis="{title:'Year'}",
#                              legend="bottom" #,
#                              # width=800, height=400
#                 ))
        gvisAreaChart(tbl, xvar="Year", 
                         yvar=c("Cashflow","Operating_Income"),
                         options=list(isStacked=TRUE,
                                      title="Before-tax Operating Income & After-tax Cash Flow", 
                                      vAxis="{title:'Net Annual Impact under Robot ($)'}",
                                      hAxis="{title:'Year'}",
                                      legend="bottom" #,
                                      # width=800, height=400
                                        ))
})


})

# 
# ## --- Robots vs Parlors ---
# 
# output$copy_profile_choice1 <- renderUI({ 
#   div(h4("Selected Investment Profile:"), 
#       h4(input$profile_choice), align="center")
# }) 
# 
# output$copy_profile_choice2 <- renderUI({
#   div(h4("Selected Investment Profile:"), 
#       h4(input$profile_choice), align="center")
# }) 
# 
# observeEvent(input$profile_choice, {
#   updateSelectInput(session,"copy_profile_choice1",  selected=input$profile_choice,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
#   
#   updateSelectInput(session,"copy_profile_choice2",  selected=input$profile_choice,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
# })
# 
# observeEvent(input$copy_profile_choice1, {
#   updateSelectInput(session,"profile_choice",  selected=input$copy_profile_choice1,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
#   
#   updateSelectInput(session,"copy_profile_choice2",  selected=input$copy_profile_choice1,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
# })
# 
# 
# observeEvent(input$copy_profile_choice2, {
#   updateSelectInput(session,"copy_profile_choice1",  selected=input$copy_profile_choice2,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
#   
#   updateSelectInput(session,"profile_choice",  selected=input$copy_profile_choice2,
#                     choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
# })

# 
# # ----------- Robustness tables ---------
# output$table_robust_variables <- DT::renderDataTable({
#   if (input$robust=="Sensitivity") {
#     if (dim(rb$table_sensitivity_before_tax)[1]==0) return()
#     tbl <- rb$table_sensitivity_before_tax
#     noncurrency <- c_noncurrency
#     change_var <- c(2:4)
#   }
#   else if (input$robust=="Scenarios") {
#     if (dim(rb$table_scenario_before_tax)[1]==0) return()
#     tbl <- rb$table_scenario_before_tax
#     noncurrency <- s_noncurrency
#     change_var <- c(2:11)
#   } else {
#     return()
#   }
#   L <- length(tbl[,1])
#   vars <- colnames(tbl)
#   tbl <- tbl[,vars[c(1,change_var)]]
#   DT::datatable(tbl,
#                 rownames = FALSE,
#                 extensions = 'ColVis',
#                 options = list(
#                   dom = 'C<"clear">lfrtip',
#                   scrollX = TRUE,
#                   scrollCollapse = TRUE,
#                   scrollY = 500,
#                   pageLength = L,
#                   bLengthChange =FALSE,
#                   searching = FALSE,
#                   activate = 'mouseover')) 
# })
# 
# 
# output$table_robust_before_tax <- DT::renderDataTable({
#   if (input$robust=="Sensitivity") {
#     if (dim(rb$table_sensitivity_before_tax)[1]==0) return()
#     tbl <- rb$table_sensitivity_before_tax
#     noncurrency <- c_noncurrency
#     change_var <- c(2:4)
#   }
#   else if (input$robust=="Scenarios") {
#     if (dim(rb$table_scenario_before_tax)[1]==0) return()
#     tbl <- rb$table_scenario_before_tax
#     noncurrency <- s_noncurrency
#     change_var <- c(2:11)
#   } else {
#     return()
#   }
#   L <- length(tbl[,1])
#   vars <- colnames(tbl)
#   tbl <- tbl[,vars[-c(change_var, grep("diff:",vars))]]
#   DT::datatable(tbl,
#                 rownames = FALSE,
#                 extensions = 'ColVis',
#                 options = list(
#                   dom = 'C<"clear">lfrtip',
#                   scrollX = TRUE,
#                   scrollCollapse = TRUE,
#                   scrollY = 500,
#                   pageLength = L,
#                   bLengthChange =FALSE,
#                   searching = FALSE,
#                   activate = 'mouseover')) %>% 
#     formatCurrency(colnames(tbl)[!(colnames(tbl) %in% noncurrency)])
# })
# 
# 
# output$table_robust_after_tax <- DT::renderDataTable({
#   if (input$robust=="Sensitivity") {
#     if (dim(rb$table_sensitivity_after_tax)[1]==0) return()
#     tbl <- rb$table_sensitivity_after_tax
#     noncurrency <- c_noncurrency
#     change_var <- c(2:4)
#   }
#   else if (input$robust=="Scenarios") {
#     if (dim(rb$table_scenario_after_tax)[1]==0) return()
#     tbl <- rb$table_scenario_after_tax
#     noncurrency <- s_noncurrency
#     change_var <- c(2:11)
#   } else {
#     return()
#   }
#   L <- length(tbl[,1])
#   vars <- colnames(tbl)
#   tbl <- tbl[,vars[-c(change_var, grep("diff:",vars))]]
#   DT::datatable(tbl,
#                 rownames = FALSE,
#                 extensions = 'ColVis',
#                 options = list(
#                   dom = 'C<"clear">lfrtip',
#                   scrollX = TRUE,
#                   scrollCollapse = TRUE,
#                   scrollY = 500,
#                   pageLength = L,
#                   bLengthChange =FALSE,
#                   searching = FALSE,
#                   activate = 'mouseover')) %>% 
#     formatCurrency(colnames(tbl)[!(colnames(tbl) %in% noncurrency)])
# }) 
# 
# output$table_robust_before_tax_diff <- DT::renderDataTable({
#   if (input$robust=="Sensitivity") {
#     if (dim(rb$table_sensitivity_before_tax)[1]==0) return()
#     tbl <- rb$table_sensitivity_before_tax
#     noncurrency <- c_noncurrency
#   }
#   else if (input$robust=="Scenarios") {
#     if (dim(rb$table_scenario_before_tax)[1]==0) return()
#     tbl <- rb$table_scenario_before_tax
#     noncurrency <- s_noncurrency
#   } else {
#     return()
#   }
#   L <- length(tbl[,1])
#   vars <- colnames(tbl)
#   tbl <- tbl[,vars[c(1,grep("diff:",vars))]]
#   DT::datatable(tbl,
#                 rownames = FALSE,
#                 extensions = 'ColVis',
#                 options = list(
#                   dom = 'C<"clear">lfrtip',
#                   scrollX = TRUE,
#                   scrollCollapse = TRUE,
#                   scrollY = 500,
#                   pageLength = L,
#                   bLengthChange =FALSE,
#                   searching = FALSE,
#                   activate = 'mouseover')) %>% 
#     formatCurrency(colnames(tbl)[!(colnames(tbl) %in% noncurrency)])
# })
# 
# 
# output$table_robust_after_tax_diff <- DT::renderDataTable({
#   
#   if (input$robust=="Sensitivity") {
#     if (dim(rb$table_sensitivity_after_tax)[1]==0) return()
#     tbl <- rb$table_sensitivity_after_tax
#     noncurrency <- c_noncurrency
#   }
#   else if (input$robust=="Scenarios") {
#     if (dim(rb$table_scenario_after_tax)[1]==0) return()
#     tbl <- rb$table_scenario_after_tax
#     noncurrency <- s_noncurrency
#   } else {
#     return()
#   }
#   L <- length(tbl[,1])
#   vars <- colnames(tbl)
#   tbl <- tbl[,vars[c(1,grep("diff:",vars))]]
#   DT::datatable(tbl,
#                 rownames = FALSE,
#                 extensions = 'ColVis',
#                 options = list(
#                   dom = 'C<"clear">lfrtip',
#                   scrollX = TRUE,
#                   scrollCollapse = TRUE,
#                   scrollY = 500,
#                   pageLength = L,
#                   bLengthChange =FALSE,
#                   searching = FALSE,
#                   activate = 'mouseover')) %>% 
#     formatCurrency(colnames(tbl)[!(colnames(tbl) %in% noncurrency)])
# }) 
# 
# 
