# ---------- Partial Budget Analysis -----------

fluidRow(
  column(width=8,offset=2, 
          h4("Partial Budget Analysis",align="center"),
                   div(wellPanel(
                     # h4("Results represented as future values accounting for inflations"),
                   sliderInput("budget_year", "Select budget year",value=1, min=1,max=30)), align="center")
                   ), 
  fluidRow(column(width=6,
           wellPanel(
             h5(strong("Positive Impacts:")),
             h5("Increased Incomes:"),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased income due to herd size increase")),
                      column(width=3, uiOutput("inc_rev_herd_size"))               
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased income due to per-cow increase")),
                      column(width=3, uiOutput("inc_rev_per_cow"))                 
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased milk premiums")),
                      column(width=3, uiOutput("inc_rev_milk_premium"))             
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased cull cow sales (minus = decrease)")),
                      column(width=3, uiOutput("inc_rev_cull_sale"))                   
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Software value to herd production")),
                      column(width=3, uiOutput("inc_rev_software"))                   
             ),
             hr(), 
             fluidRow(column(width=8, offset=1, 
                             h5("Total increased incomes")),
                      column(width=3, uiOutput("inc_rev_total"))               
             ),
             br(),
             h5("Decreased Expenses:"),
             fluidRow(column(width=8, offset=1, 
                             helpText("Reduced heat detection")),
                      column(width=3, uiOutput("dec_exp_heat_detection"))                   
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Reduced labor")),
                      column(width=3, uiOutput("dec_exp_labor"))                       
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Reduced labor management")),
                      column(width=3, uiOutput("dec_exp_labor_management"))                   
             ),
             hr(),
             fluidRow(column(width=8, offset=1, 
                             h5("Total decreased expenses")),
                      column(width=3, uiOutput("dec_exp_total"))               
             ), 
             br(),
             fluidRow(column(width=8, offset=0, 
                             h5("Total positve impacts")),
                      column(width=3, offset=1, uiOutput("positive_total"))              
             )
           )),
    column(width=6,
           wellPanel(
             h5(strong("Negative Impacts:")),
             h5("Increased Expenses:"),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased expenses due to herd size increase")),
                      column(width=3, uiOutput("inc_exp_herd_increase"))            
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased repair and insurance costs")),
                      column(width=3, uiOutput("inc_exp_repair"))            
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Change in feed quantity due to DMI change")),
                      column(width=3, uiOutput("inc_exp_feed"))                    
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Extra cost to pellet the feed fed in the robot booth")),
                      column(width=3, uiOutput("inc_exp_pellet"))                          
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased cow replacement costs (minus = decrease)")),
                      column(width=3, uiOutput("inc_exp_replacement"))                           
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased utilities and supplies")),
                      column(width=3, uiOutput("inc_exp_utilities"))                           
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Increased records management")),
                      column(width=3, uiOutput("inc_exp_record_management"))                           
             ),
             hr(), 
             fluidRow(column(width=8, offset=1, 
                             h5("Total increased expenses")),
                      column(width=3, uiOutput("inc_exp_total"))                          
             ), 
             br(),
             h5("Increased Cost of Capital (annuity payment equivalent):"),
             fluidRow(column(width=8, offset=1, 
                             helpText("Capital recovery cost of robots (dep. & int.)*")),
                      column(width=3, uiOutput("capital_recovery_robot"))                          
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Capital recovery cost of housing (dep. & int.)*")),
                      column(width=3, uiOutput("capital_recovery_housing"))                      
             ),
             fluidRow(column(width=8, offset=1, 
                             helpText("Capital cost of downpayment*")),
                      column(width=3, uiOutput("cost_downpayment"))                      
             ),             
             fluidRow(column(width=8, offset=1, 
                             helpText("Salvage value of robots (minus = income)*")),
                      column(width=3, uiOutput("robot_end_PV"))                       
             ),
             hr(),
             fluidRow(column(width=8, offset=1, 
                             h5("Total cost of capital*")),
                      column(width=3, uiOutput("capital_cost_total"))                         
             ), br(), 
             fluidRow(column(width=8, offset=0, 
                             h5("Total negative impacts*")),
                      column(width=3,  offset=1, uiOutput("negative_total"))                        
             )
           ), br(), br()
    )), 
  fluidRow(
    column(width=8, offset=2, 
                  fluidRow(
                    column(width=3,offset=9, span(helpText("Breakeven wage"),
                                                  align="right"))
                  ), 
           fluidRow(
             column(width=1,offset=9, span(helpText("Wage"),
                                           align="center")),
             column(width=1,offset=0, span(helpText("Inflation"),
                                           align="center"))
           ), 
                  fluidRow(column(width=7, offset=0, 
                                  h5("Total positive impacts minus total negative impacts*")),
                           column(width=3,  uiOutput("positive_minus_negative")),
                           column(width=1,  uiOutput("be_wage_positive_minus_negative")),
                           column(width=1,  helpText("-"))
                  ), 
           fluidRow(column(width=6, offset=1, 
                           helpText("Inflation adjustments*")),
                    column(width=3, uiOutput("inflation_adjustment"))
           ), 
           fluidRow(column(width=7, offset=0, 
                           h5("Net annual impact (before-tax)*")),
                    column(width=3,  uiOutput("net_annual_impact_before_tax")),
                    column(width=1,  uiOutput("bw_wage_before_tax")),
                    column(width=1,  uiOutput("bw_wage_inflation_before_tax"))
           ), 
  br(),
#   fluidRow(column(width=7, offset=0, 
#                   helpText("Change in revenue minus expense (= before-tax impact plus cost of capital)")),
#            column(width=3, uiOutput("revenue_minus_expense"))
#   ), 
  fluidRow(column(width=7, helpText("Taxes:"))),
  fluidRow(column(width=6, offset=1, 
                  helpText("Tax on change in revenue minus expense*")),
           column(width=3, uiOutput("tax_revenue_minus_expense"))
  ), 
  fluidRow(column(width=6, offset=1, 
                  helpText("Tax deduction on interests*")),
           column(width=3,uiOutput("tax_interest"))
  ), 
  fluidRow(column(width=6, offset=1, 
                  helpText("Tax deduction on depreciation*")),
           column(width=3, uiOutput("tax_depreciation"))
  ), 
fluidRow(column(width=7, helpText("Adjustments to Annualized Cash Flow Values at Farm's WACC:"))),
fluidRow(column(width=6, offset=1, 
                helpText("Adjustments of interest rate*")),
         column(width=3,uiOutput("adj_WACC_interest"))
), 
fluidRow(column(width=6, offset=1, 
                helpText("Adjustments of hurdle rate*")),
         column(width=3, uiOutput("adj_WACC_hurdle"))
),
  fluidRow(column(width=7, offset=0, 
                  h5("Net annual impact (after-tax)*")),
           column(width=3, uiOutput("net_annual_impact_after_tax")),
           column(width=1,  uiOutput("bw_wage_after_tax")),
           column(width=1,  uiOutput("bw_wage_inflation_after_tax"))
  )
  )),  
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

           br(),
           #tags$a(href ="#data_entry",  #"#tab-9037-2",
           ## I haven't been able to set a link to a tab. It seems compliated in Shiny.
           div(id="goData", class="well", style="background-color: gray; color:white;", 
               align="center", 
               h4("See interactive dashboard under the Data Entry tab")
           ),
           br(),br()
    # ))
) 


