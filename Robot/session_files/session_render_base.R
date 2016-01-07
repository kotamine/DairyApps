

## rendering UI 



var_to_render_0 <- c("herd_size2", "cost_milking1","cost_housing","total_investment_cow",
                     "total_investment", "planning_horizon",
                     "increased_insurance", "anticipated_hours_milking","milk_lb_alt_day",
                     "yr_milking2",  "loan_housing", "milk_day_cow_alt",
                     "loan_milking1","loan_milking2", 
                     "copy_cost_housing", "copy_cost_milking1", "copy_cost_milking2", 
                     "copy_salvage_milking1","copy_salvage_milking2",
                     "yr_system2", 
                     "copy_milking_years", "copy_n_robot", "copy_cost_robot")
var_to_render_1 <- c()
var_to_render_2 <- c("r_milking2","adj_milk_cow_day")
var_to_render_3 <- c("DMI_change","DMI_day","DMI_projected","copy_DMI_projected") 
var_to_render_4 <- c("stage_lactation")

var_to_render <- list(var_to_render_1, var_to_render_2, var_to_render_3, var_to_render_4)  


pb_render_right_0 <- c("NPV","ANPV", "ANPVr", 
                       "capital_recovery_milking", "capital_recovery_housing",
                       "cost_downpayment", "salvage_milking_PV",
                       "capital_cost_total", 
                       "tax_revenue_minus_expense",
                       "tax_interest",
                       "tax_depreciation",
                       # "net_annual_impact_after_tax",
                       "cost_downpayment",
                       "adj_WACC_interest",
                       "adj_WACC_hurdle")

# These variables are stored in a separate list, e.g., ans[["Robots_pb"]]  etc.   
# This avoids uncessarily triggering other calculations. 
pb_separate <- c("pb_positive_total","pb_negative_total",
                 "pb_positive_minus_negative",
                 "pb_inflation_adjustment",
                 "pb_revenue_minus_expense" #,
                 # "pb_net_annual_impact_before_tax"
) 

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



CF_var_stem <- c("CF_cost", "CF_loan", "CF_down", "CF_r", "CF_hr")
CF_var_ending <- c("housing","milking1","milking2")

CF_vars <- lapply(CF_var_stem, function(stem) {
  lapply(CF_var_ending, function(ending) {
    paste0(stem,"_",ending) 
  })
}) %>% unlist()





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
      ans[[x]][[paste0(z)]] %>% round_pct(2) %>% helpText() %>% div(align="right")
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
             if (input[[paste0("yr_system1",x)]] >= input[[paste0("budget_year",x)]]) { 
               # Use delayed investment revenue or expense when appricable 
               x_pb <- paste0(x,"_delay")
             } else {
               x_pb <- x
             }
             (ans[[x_pb]][[paste0(z)]] * i_factor) %>% 
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
      if (r==3) {
        output[[paste0(z,x)]] <- renderUI({
          need(!is.null(ans[[paste0(x,"_bw")]][[paste0(z)]]), "NA") %>% validate()
          a <- (ans[[paste0(x,"_bw")]][[paste0(z)]]*100) %>% round(r)
          paste0(a,"%") %>% helpText() %>% div(align="right")
        })
      } else {
        output[[paste0(z,x)]] <- renderUI({
          need(!is.null(ans[[paste0(x,"_bw")]][[paste0(z)]]), "NA") %>% validate()
          a <- (ans[[paste0(x,"_bw")]][[paste0(z)]]) %>% round(r)
          paste0("$",a) %>% helpText() %>% div(align="right")
        })
      }
    })
  })
  
  # Cash Flow
  CF_render <- function(fun,...) {
    function(z) {
      output[[paste0(z,x)]] <- renderUI({
        ans[[x]][[paste0(sub("CF_","copy_",z))]] %>% fun(...) %>% 
          helpText() %>% div(align="right")
      })
    }
  }
  
  lapply(CF_vars, function(z) {
    if (grep("CF_r",z) %>% length() >0 | grep("CF_hr",z) %>% length() >0) {
      CF_render(round_pct,3)(z)
    } else {
      CF_render(formatcomma,dollar=TRUE)(z)
    }
  })
  
  
  output[[paste0("CF_WACC_formula",x)]] <- renderUI({
    add_space1 <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp"
    add_space2 <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp"
    
    div( 
      helpText("WACC is the weighted average of the interest rates for housing and milking system loans and the 
                            hurdle rate (opportunity cost) assumed for downpayments. 
               The weights are the proportions of loans and downpayments. 
               For simplicity  we exclude the consideration for the second set of milking system from the WACC calculation."),
      
      helpText("WACC = (housing_loans / total_investment) * housing_interest ", br(), 
               HTML(add_space1), "+ (milking_system_loans / total_investment) * milking_system_interest ", br(), 
               HTML(add_space1), '+ (downpayments / total_investment) * hurdle_rate', br(), 
               HTML(add_space2), '= (housing_loans * housing_interest ' , br(),
               HTML(add_space1), '+ milking_system_loans * milking_system_interest ' , br(),
               HTML(add_space1), '+ downpayments * hurdle_rate ) / total_investment'), 
      
      helpText(HTML(add_space2), "= (", ans[[x]][["loan_housing"]] %>% formatcomma(dollar=TRUE), "*", 
               input[[paste0("r_housing",x)]] %>% round_pct(2), "+", 
               ans[[x]][["loan_milking1"]] %>% formatcomma(dollar=TRUE), "*",
               input[[paste0("r_milking1",x)]] %>% round_pct(2), "+", 
               ans[[x]][["loan_milking1"]]  %>% formatcomma(dollar=TRUE), "*", 
               input$hurdle_rate %>% round_pct(2),") / ",
               ans[[x]][["total_investment"]] %>% formatcomma(dollar=TRUE), br(),
               HTML(add_space2), "=", ans[[x]]$WACC %>% round_pct(2))
    )
  }) 
  
  output[[paste0("CF_NPV_formula",x)]] <- renderUI({ 
    add_space1 <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp"
    add_space2 <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp"
    NPV <- ans[[x]]$NPV %>% formatcomma(0, dollar=TRUE)
    CF <- lapply(ans[[x]]$table_cash_flow$after_tax_cash_flow, formatcomma, 0, dollar=TRUE) %>% unlist() 
    CF_T <- length(CF)
    WACC <- (ans[[x]]$WACC/100) %>% round(4)
    WACC1 <- lapply(c(1:CF_T), function(a) paste0(" / (1+", WACC,")^",a-1))
    WACC1[1] <- ""
    CF_PV <- lapply(c(1:CF_T), function(a) paste(CF[a],WACC1[a])) %>% unlist()
    div( 
      helpText("NPV is a sum of discounted cash flows. 
               Here we calculate NPV of after-tax cash flows (CFs) over the course of 
               investment horizon (T) using WACC as a discount rate."), 
      helpText("NPV= CF_year0 + CF_year1/(1+WACC) + CF_year2/(1+WACC)^2", br(),
               HTML(add_space1), "+ ... + CF_yearT/(1+WACC)^T"),
      helpText(HTML(add_space2),"=", CF_PV[1],"+", CF_PV[2],"+", CF_PV[3], br(),
               HTML(add_space1), "+ ... +", CF_PV[CF_T], br(),
               HTML(add_space2),"=", NPV), br(),
      helpText("Net annual impact (after-tax) is an annualized NPV, obtained by converting NPV into an annuity.
               This represents the value of investment in the form of constant income stream 
               over the investment horizon. The discounted value of annuity incomes is equivalent to NPV;"),
      helpText("annuity + annuity/(1+WACC) + annuity/(1+WACC)^2 + ... + annuity/(1+WACC)^T", br(),
               HTML(add_space2),"= NPV"), br(),
      helpText("This implies; "),
      helpText("annuity = NPV / (1 + (1+WACC) + (1+WACC)^2 + ... + (1+WACC)^T)",br(),
               HTML(add_space2), HTML("&nbsp;&nbsp;&nbsp;&nbsp"),"=", ans[[x]]$ANPV %>% formatcomma(0, dollar=TRUE)), br(),
      
      helpText("The return on investment (ROI) here is the NPV divided by the total investment;"),
      helpText("ROI = NPV/ (initial investment for housing and milking system  +  any additional investment)", br(),
               HTML(add_space2),"= ",  NPV, "/ (",ans[[x]]$total_investment %>% formatcomma(0, dollar=TRUE), "+",
               ans[[x]]$cost_milking2 %>% formatcomma(0, dollar=TRUE) ,")", br(),
               HTML(add_space2),"= ",  ans[[x]]$ROI %>% round_pct(2))
    )
  }) 
  
  
  output[[paste0("CF_ROI",x)]] <- renderUI({ 
    
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
lapply(c(base_profiles,base_profiles_se), function(x) { 
  
  sensitivity <- grepl("_se", x)  # TRUE/FALSE for sensitivity analysis
  
  output[[paste0("IOFC",x)]] <- renderUI({
    if (sensitivity) {
      if (input$IOFC=="per cow") {
        difference <- ans[[paste0(x,"_da")]]()[[input$NAI]]$did_IOFC
      } else  {           
        difference <- ans[[paste0(x,"_da")]]()[[input$NAI]]$did_IOFC_cwt
      }
    } else {
      difference <- NULL
    }
    
    if (input$IOFC=="per cow") {
      dash_IOFC(ans[[paste0(x,"_da")]]()[[input$NAI]]$IOFC, 
                ans[[paste0(x,"_da")]]()[[input$NAI]]$IOFC2, basis=input$IOFC, 
                x, difference=difference)
    } else {
      dash_IOFC(ans[[paste0(x,"_da")]]()[[input$NAI]]$IOFC_cwt,
                ans[[paste0(x,"_da")]]()[[input$NAI]]$IOFC2_cwt, basis=input$IOFC, 
                x, difference=difference)
    }
  })  
  
  
  output[[paste0("NAI",x)]] <- renderUI({
    if (sensitivity) {
      difference <-ans[[paste0(x,"_da")]]()[[input$NAI]]$diff_NAI
    } else {
      difference <-NULL
    }
    dash_NAI(ans[[paste0(x,"_da")]]()[[input$NAI]]$NAI, x,cutoff=0, difference=difference)
  })
  
  dashboard_items <- c("milk_feed","labor_repair","capital","misc","inflation")
  dashboard_colors <- c("#1569C7", "#FF926F", "#64E986", "#EDDA74", "#7A5DC7")
  dashboard_labels <- c("Milk Income - Feed Cost", "Labor + Repair Cost", "Cost of Capital",
                        "Others", "Inflation Adjustments")
  
  lapply(dashboard_items, function(item) {
    
    output[[paste0(item,x)]] <- renderUI({
      need(!is.null(ans[[paste0(x,"_da")]]()[[input$NAI]][[item]]),"NA") %>% validate()
      
      loc <- which(dashboard_items==item)
      
      if (sensitivity) {
        difference <- ans[[paste0(x,"_da")]]()[[input$NAI]][[paste0("diff_",item)]] %>% formatdollar2b()
      } else {
        difference <- paste("under", refProfileName(x))
      }
      
      div(class="well well-sm", 
          style= paste0("background-color: ", dashboard_colors[loc], "; color:white;"), 
          ans[[paste0(x,"_da")]]()[[input$NAI]][[item]] %>% formatdollar2() %>% strong() %>% h4(), 
          h5(dashboard_labels[loc]), h5(difference)) 
    })  
    
  })
  
  
  # Dashboard plots and charts
  output[[paste0("plot1",x)]] <- renderGvis({ 
    
    tbl <- data.frame(
      varnames=c("Milk","Feed"),
      Robot=(c(ans[[paste0(x,"_da")]]()[[input$NAI]]$milk_robot,
               ans[[paste0(x,"_da")]]()[[input$NAI]]$feed_robot)/1000) %>% round,
      color1=rep("#356AE8",2),
      Current=(c(ans[[paste0(x,"_da")]]()[[input$NAI]]$milk_current, 
                 ans[[paste0(x,"_da")]]()[[input$NAI]]$feed_current)/1000) %>% round,
      color2=rep("#99CEFF",2)
    )
    colnames(tbl)[2] <- gsub(" ","",refProfileName(x))
    
    names(tbl)<-c("varnames",
                  colnames(tbl)[2],paste0(colnames(tbl)[2],".style"),
                  "Current","Current.style")
    
    
    gvisBarChart(tbl, xvar=colnames(tbl)[1],
                 yvar=colnames(tbl)[-1],
                 options=list(title="Milk - Feed",
                              titleTextStyle="{fontSize:12}",
                              legend="none",
                              vAxis="{fontSize:12}", 
                              hAxis="{title:'$1,000'}",
                              chartArea ='{width: "50%", height: "50%" }')
    )
  })
  
  
  output[[paste0("plot2",x)]] <- renderGvis({ 
    
    tbl <- data.frame(
      varnames=c("Labor","Repair"),
      Robot=(c(ans[[paste0(x,"_da")]]()[[input$NAI]]$labor_robot,
               ans[[paste0(x)]]$inc_exp_repair)/1000) %>% round,
      color1=rep("#FF7F50",2),
      Current=(c(ans[[paste0(x,"_da")]]()[[input$NAI]]$labor_current, 
                 0)/1000) %>% round,
      color2=rep("#FFBB8C",2)
    )
    colnames(tbl)[2] <- gsub(" ","",refProfileName(x))
    
    names(tbl)<-c("varnames",
                  colnames(tbl)[2],paste0(colnames(tbl)[2],".style"),
                  "Current","Current.style")
    
    gvisBarChart(tbl, xvar=colnames(tbl)[1],
                 yvar=colnames(tbl)[-1],
                 options=list(title="Labor + Repair",
                              titleTextStyle="{fontSize:12}",
                              legend="none",
                              vAxis="{fontSize:12}", 
                              hAxis="{title:'$1,000'}",
                              chartArea ='{width: "50%", height: "50%"}')
    )
  })
  
  
  output[[paste0("plot3",x)]] <- renderGvis({ 
    tbl <- data.frame(
      varnames=c(refProfileName(x),"Housing","Downpayment","Salvage"),
      Robot=(c(ans[[paste0(x,"_da")]]()[[input$NAI]]$capital_recovery_robot2, 
               ans[[paste0(x,"_da")]]()[[input$NAI]]$capital_recovery_housing2, 
               ans[[paste0(x)]]$cost_downpayment, 
               -ans[[x]]$salvage_milking_PV)/1000) %>% round, 
      color1=rep("#64E986",4)  
    )   
    colnames(tbl)[2] <- gsub(" ","",refProfileName(x))
    
    names(tbl)<-c("varnames",
                  colnames(tbl)[2],paste0(colnames(tbl)[2],".style"))
    
    gvisBarChart(tbl, xvar=colnames(tbl)[1],
                 yvar=colnames(tbl)[-1],
                 options=list(title="Capital Cost",
                              titleTextStyle="{fontSize:12}",
                              legend="none",
                              vAxis="{fontSize:12}", 
                              hAxis="{title:'$1,000'}",
                              chartArea ='{width: "50%", height: "50%" }')
    )
  })
  
  output[[paste0("cashflow_small_chart",x)]] <- renderGvis({
    if (length(ans[[x]][["table_cash_flow"]])==0) return()
    tbl <- round(ans[[x]][["table_cash_flow"]])
    n_year <- ans[[x]]$planning_horizon 
    
    df <- data.frame(Year=c(0:n_year))
    if (input$NAI=="before tax") {
      tax_status <- "Before-tax"
      df$Cash_flow <- ans[[x]][["table_cash_flow"]]$before_tax_cash_flow %>% round()
    } else {
      tax_status <- "After-tax"
      df$Cash_flow <- ans[[x]][["table_cash_flow"]]$after_tax_cash_flow %>% round()
    }
    gvisAreaChart(df, xvar="Year", 
                  yvar=c("Cash_flow"),
                  options=list(
                    title=paste(tax_status, "Cash Flow"), 
                    vAxis= paste("{title:'Impact under", refProfileName(x), "($)'}"),
                    hAxis="{title:'Year'}",
                    legend="none"
                  )
    )
  })
  
})



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
    
    gvisAreaChart(tbl, xvar="Year", 
                  yvar=c("Cashflow","Operating_Income"),
                  options=list( 
                    title="Before-tax Operating Income & After-tax Cash Flow", 
                    vAxis="{title:'Net Annual Impact under Robot ($)'}",
                    hAxis="{title:'Year'}",
                    legend="bottom",
                    chartArea ='{width: "50%", height: "65%" }',
                    width=800, height=400
                  ))
  })
  
  
})



