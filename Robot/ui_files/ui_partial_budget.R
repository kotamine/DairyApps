# ---------- Partial Budget Analysis -----------


partialBudget <- function(x) {   
  div(
  fluidRow(column(6, offset=3,
                  div(wellPanel(
                    h4(paste(x),align="center"),                  
                    sliderInput(paste0("budget_year",refProfile(x)), "Select budget year",value=1, min=1,max=30)), align="center")
  )), 
  fluidRow(column(width=6,
                  wellPanel(
                    h5(strong("Positive Impacts:")),
                    h5("Increased Incomes:"),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased income due to herd size increase")),
                             column(width=3, uiOutput(paste0("inc_rev_herd_size",refProfile(x))))            
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased income due to per-cow increase")),
                             column(width=3, uiOutput(paste0("inc_rev_per_cow",refProfile(x))))               
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased milk premiums")),
                             column(width=3, uiOutput(paste0("inc_rev_milk_premium",refProfile(x))))           
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased cull cow sales (minus = decrease)")),
                             column(width=3, uiOutput(paste0("inc_rev_cull_sale",refProfile(x))))                 
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Software value to herd production")),
                             column(width=3, uiOutput(paste0("inc_rev_software",refProfile(x))))                   
                    ),
                    hr(), 
                    fluidRow(column(width=8, offset=1, 
                                    h5("Total increased incomes")),
                             column(width=3, uiOutput(paste0("inc_rev_total",refProfile(x))))             
                    ),
                    br(),
                    h5("Decreased Expenses:"),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Reduced heat detection")),
                             column(width=3, uiOutput(paste0("dec_exp_heat_detection",refProfile(x))))                 
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Reduced labor")),
                             column(width=3, uiOutput(paste0("dec_exp_labor",refProfile(x))))           
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Reduced labor management")),
                             column(width=3, uiOutput(paste0("dec_exp_labor_management",refProfile(x))))
                             
                    ),
                    hr(),
                    fluidRow(column(width=8, offset=1, 
                                    h5("Total decreased expenses")),
                             column(width=3, uiOutput(paste0("dec_exp_total",refProfile(x))))               
                    ), 
                    br(),
                    fluidRow(column(width=8, 
                                    h5("Total positve impacts")),
                             column(width=3, offset=1, uiOutput(paste0("pb_positive_total",refProfile(x))))              
                    )
                  )),
           column(width=6,
                  wellPanel(
                    h5(strong("Negative Impacts:")),
                    h5("Increased Expenses:"),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased expenses due to herd size increase")),
                             column(width=3, uiOutput(paste0("inc_exp_herd_increase",refProfile(x))))            
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased repair and insurance costs")),
                             column(width=3, uiOutput(paste0("inc_exp_repair",refProfile(x))))            
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Change in feed quantity due to DMI change")),
                             column(width=3, uiOutput(paste0("inc_exp_feed",refProfile(x))))                    
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Extra cost to pellet the feed fed in the robot booth")),
                             column(width=3, uiOutput(paste0("inc_exp_pellet",refProfile(x))))                          
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased cow replacement costs (minus = decrease)")),
                             column(width=3, uiOutput(paste0("inc_exp_replacement",refProfile(x))))                           
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased utilities and supplies")),
                             column(width=3, uiOutput(paste0("inc_exp_utilities",refProfile(x))))                           
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Increased records management")),
                             column(width=3, uiOutput(paste0("inc_exp_record_management",refProfile(x))))                           
                    ),
                    hr(), 
                    fluidRow(column(width=8, offset=1, 
                                    h5("Total increased expenses")),
                             column(width=3, uiOutput(paste0("inc_exp_total",refProfile(x))))                      
                    ), 
                    br(),
                    h5("Increased Cost of Capital (annuity payment equivalent):"),
                    fluidRow(column(width=8, offset=1, 
                                    helpText(paste("Capital recovery cost of",x, "(dep. & int.)*"))),
                             column(width=3, uiOutput(paste0("capital_recovery_milking",refProfile(x))))                         
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Capital recovery cost of housing (dep. & int.)*")),
                             column(width=3, uiOutput(paste0("capital_recovery_housing",refProfile(x))))                      
                    ),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Capital cost of downpayment*")),
                             column(width=3, uiOutput(paste0("cost_downpayment",refProfile(x))))                      
                    ),             
                    fluidRow(column(width=8, offset=1, 
                                    helpText(paste("Salvage value of",x, "(minus = income)*"))),
                             column(width=3, uiOutput(paste0("salvage_milking_PV",refProfile(x))))                       
                    ),
                    hr(),
                    fluidRow(column(width=8, offset=1, 
                                    h5("Total cost of capital*")),
                             column(width=3, uiOutput(paste0("capital_cost_total",refProfile(x))))                         
                    ), br(), 
                    fluidRow(column(width=8, 
                                    h5("Total negative impacts*")),
                             column(width=3,  offset=1, uiOutput(paste0("pb_negative_total",refProfile(x))))                        
                    )
                  ), br() 
           )),   
   
  fluidRow(
    column(width=8, offset=2, 
           helpText("Asterisk (*) symbol indicates that the value is annualized
                    into a constant income or payment (called annuity) during the planning horizon."),
           br(),
           hr(),
           # -------- Summary ------- 
           
           fluidRow(column(width=9, offset=0, 
                           h5("Total positive impacts minus total negative impacts*")),
                    column(width=3,  uiOutput(paste0("pb_positive_minus_negative",refProfile(x))))
           ), 
           fluidRow(column(width=8, offset=1, 
                           helpText("Inflation adjustments*")),
                    column(width=3, uiOutput(paste0("pb_inflation_adjustment",refProfile(x))))
           ), 
           hr(),
           fluidRow(column(width=9, offset=0, 
                           h5("Net annual impact (before-tax)*")),
                    column(width=3,  uiOutput(paste0("pb_net_annual_impact_before_tax",refProfile(x))))
           ), 
           br(),
           fluidRow(column(width=9, helpText("Taxes and Tax Deductions:"))),
           fluidRow(column(width=8, offset=1, 
                           helpText("Tax on change in revenue minus expense*")),
                    column(width=3, uiOutput(paste0("tax_revenue_minus_expense",refProfile(x))))
           ), 
           fluidRow(column(width=8, offset=1, 
                           helpText("Tax deduction on interests*")),
                    column(width=3,uiOutput(paste0("tax_interest",refProfile(x))))
           ), 
           fluidRow(column(width=8, offset=1, 
                           helpText("Tax deduction on depreciation*")),
                    column(width=3, uiOutput(paste0("tax_depreciation",refProfile(x))))
           ), 
           fluidRow(column(width=9, helpText("WACC Adjustments to Annualized Cash Flow Values:"))),
           fluidRow(column(width=8, offset=1, 
                           helpText("Adjustments with respect to interest rate*")),
                    column(width=3,uiOutput(paste0("adj_WACC_interest",refProfile(x))))
           ), 
           fluidRow(column(width=8, offset=1, 
                           helpText("Adjustments with respect to hurdle rate*")),
                    column(width=3, uiOutput(paste0("adj_WACC_hurdle",refProfile(x))))
           ),
           hr(),
           fluidRow(column(width=9, offset=0, 
                           h5("Net annual impact (after-tax)*")),
                    column(width=3, uiOutput(paste0("net_annual_impact_after_tax",refProfile(x))))
           )
    )),
#            fluidRow(
#              column(width=3,offset=9, div(helpText("Breakeven Labor"),align="right"))
#            ), 
#            fluidRow(
#              column(width=1,offset=10, div(helpText("Wage"),
#                                            align="center")),
#              column(width=1,offset=0, div(helpText("Inflation"),
#                                           align="center"))
#            ), 
#            hr(),
#            fluidRow(column(width=7, offset=0, 
#                            h5("Total positive impacts minus total negative impacts*")),
#                     column(width=3,  uiOutput("positive_minus_negative")),
#                     column(width=1,  uiOutput("be_wage_positive_minus_negative")),
#                     column(width=1,  div(helpText("-"),align="right"))
#            ), 
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Inflation adjustments*")),
#                     column(width=3, uiOutput("inflation_adjustment"))
#            ), 
#            hr(),
#            fluidRow(column(width=7, offset=0, 
#                            h5("Net annual impact (before-tax)*")),
#                     column(width=3,  uiOutput("net_annual_impact_before_tax")),
#                     column(width=1,  uiOutput("bw_wage_before_tax")),
#                     column(width=1,  uiOutput("bw_wage_inflation_before_tax"))
#            ), 
#            br(),
#            #   fluidRow(column(width=7, offset=0, 
#            #                   helpText("Change in revenue minus expense (= before-tax impact plus cost of capital)")),
#            #            column(width=3, uiOutput("revenue_minus_expense"))
#            #   ), 
#            fluidRow(column(width=7, helpText("Taxes and Tax Deductions:"))),
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Tax on change in revenue minus expense*")),
#                     column(width=3, uiOutput("tax_revenue_minus_expense"))
#            ), 
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Tax deduction on interests*")),
#                     column(width=3,uiOutput("tax_interest"))
#            ), 
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Tax deduction on depreciation*")),
#                     column(width=3, uiOutput("tax_depreciation"))
#            ), 
#            fluidRow(column(width=7, helpText("WACC Adjustments to Annualized Cash Flow Values:"))),
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Adjustments with respect to interest rate*")),
#                     column(width=3,uiOutput("adj_WACC_interest"))
#            ), 
#            fluidRow(column(width=6, offset=1, 
#                            helpText("Adjustments with respect to hurdle rate*")),
#                     column(width=3, uiOutput("adj_WACC_hurdle"))
#            ),
#            hr(),
#            fluidRow(column(width=7, offset=0, 
#                            h5("Net annual impact (after-tax)*")),
#                     column(width=3, uiOutput("net_annual_impact_after_tax")),
#                     column(width=1,  uiOutput("bw_wage_after_tax")),
#                     column(width=1,  uiOutput("bw_wage_inflation_after_tax"))
#            )
    # )),   
  #   fluidRow(
  #     column(width=8,offset=2,
  #            fluidRow(
  #              column(width=3,offset=9, span(helpText("Break-even wage"),
  #                                            align="right"))
  #            ), 
  #            fluidRow(column(width=7, offset=0, 
  #                            h5("Net annual financial impact without salvage")),
  #                     column(width=3, uiOutput("cash_impact_without_salvage")),
  #                     column(width=2,uiOutput("be_wage_without_salvage"))
  #            ), 
  #            br(),
  # #            h5("Robot's salvage value at the end of its useful life:"),
  # #            fluidRow(column(width=6, offset=1, 
  # #                            helpText("Estimated value at end $90,000")),
  # #                     column(width=3, span(helpText("Annualized PV"), align="right"))               
  # #            ),
  #            fluidRow(column(width=6, offset=1, 
  #                            helpText("Estimated salvage value")),
  #                     column(width=3, uiOutput("robot_end_PV"))                       
  #            ),
  #            fluidRow(column(width=7, offset=0, 
  #                            h5("Net annual impact with robot's salvage value")),
  #                     column(width=3, uiOutput("cash_impact_with_salvage")),                                                                          column(width=2,uiOutput("be_wage_with_salvage"))
  #            ), #br(), 
  # #            fluidRow(column(width=7, offset=0, 
  # #                            h5("Net annual impact with inflation, as annualized net present value (NPV)")),
  # #                     column(width=3, uiOutput("impact_with_inflation"))                          
  # #            ),
  # -------- Breakeven wage ------- 
  
  br(),   
  #tags$a(href ="#data_entry",  #"#tab-9037-2",
  ## I haven't been able to set a link to a tab. It seems compliated in Shiny.
  div(id="goData", class="well", style="background-color: gray; color:white;", 
      align="center", 
      h4("See interactive dashboard under the Data Entry tab")
  ), 
  br(),br() 
)   
} 
