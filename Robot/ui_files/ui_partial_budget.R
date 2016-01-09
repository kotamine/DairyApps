# ---------- Partial Budget Analysis -----------


uiPartialBudget <- function(x) {   
  div(
    fluidRow(column(6, offset=3,
                    div(wellPanel(
                      h4(paste(refProfileName(x)),align="center"),                  
                      sliderInput(paste0("budget_year",x), "Select budget year",value=1, min=1,max=30)), align="center")
    )), 
    fluidRow(column(width=6,
                    wellPanel(
                      h5(strong("Positive Impacts:")),
                      h5("Increased Incomes:"),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased income due to herd size increase")),
                               column(width=3, uiOutput(paste0("inc_rev_herd_size",x)))            
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased income due to per-cow increase")),
                               column(width=3, uiOutput(paste0("inc_rev_per_cow",x)))               
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased milk premiums")),
                               column(width=3, uiOutput(paste0("inc_rev_milk_premium",x)))           
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased cull cow sales (minus = decrease)")),
                               column(width=3, uiOutput(paste0("inc_rev_cull_sale",x)))                 
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Software value to herd production")),
                               column(width=3, uiOutput(paste0("inc_rev_software",x)))                   
                      ),
                      hr(), 
                      fluidRow(column(width=8, offset=1, 
                                      h5("Total increased incomes")),
                               column(width=3, uiOutput(paste0("inc_rev_total",x)))             
                      ),
                      br(),
                      h5("Decreased Expenses:"),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Reduced heat detection")),
                               column(width=3, uiOutput(paste0("dec_exp_heat_detection",x)))                 
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Reduced labor")),
                               column(width=3, uiOutput(paste0("dec_exp_labor",x)))           
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Reduced labor management")),
                               column(width=3, uiOutput(paste0("dec_exp_labor_management",x)))
                               
                      ),
                      hr(),
                      fluidRow(column(width=8, offset=1, 
                                      h5("Total decreased expenses")),
                               column(width=3, uiOutput(paste0("dec_exp_total",x)))               
                      ), 
                      br(),
                      fluidRow(column(width=8, 
                                      h5("Total positve impacts")),
                               column(width=3, offset=1, uiOutput(paste0("pb_positive_total",x)))              
                      )
                    )),
             column(width=6,
                    wellPanel(
                      h5(strong("Negative Impacts:")),
                      h5("Increased Expenses:"),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased expenses due to herd size increase")),
                               column(width=3, uiOutput(paste0("inc_exp_herd_increase",x)))            
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased repair and insurance costs")),
                               column(width=3, uiOutput(paste0("inc_exp_repair",x)))            
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Change in feed quantity due to DMI change")),
                               column(width=3, uiOutput(paste0("inc_exp_feed",x)))                    
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Extra cost to pellet the feed fed in the robot booth")),
                               column(width=3, uiOutput(paste0("inc_exp_pellet",x)))                          
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased cow replacement costs (minus = decrease)")),
                               column(width=3, uiOutput(paste0("inc_exp_replacement",x)))                           
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased utilities and supplies")),
                               column(width=3, uiOutput(paste0("inc_exp_utilities",x)))                           
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Increased records management")),
                               column(width=3, uiOutput(paste0("inc_exp_record_management",x)))                           
                      ),
                      hr(), 
                      fluidRow(column(width=8, offset=1, 
                                      h5("Total increased expenses")),
                               column(width=3, uiOutput(paste0("inc_exp_total",x)))                      
                      ), 
                      br(),
                      h5("Increased Cost of Capital (annuity payment equivalent):"),
                      fluidRow(column(width=8, offset=1, 
                                      helpText(paste("Capital recovery cost of",refProfileName(x), "(dep. & int.)*"))),
                               column(width=3, uiOutput(paste0("capital_recovery_milking",x)))                         
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Capital recovery cost of housing (dep. & int.)*")),
                               column(width=3, uiOutput(paste0("capital_recovery_housing",x)))                      
                      ),
                      fluidRow(column(width=8, offset=1, 
                                      helpText("Capital cost of downpayment*")),
                               column(width=3, uiOutput(paste0("cost_downpayment",x)))                      
                      ),             
                      fluidRow(column(width=8, offset=1, 
                                      helpText(paste("Salvage value of",refProfileName(x), "(minus = income)*"))),
                               column(width=3, uiOutput(paste0("salvage_milking_PV",x)))                       
                      ),
                      hr(),
                      fluidRow(column(width=8, offset=1, 
                                      h5("Total cost of capital*")),
                               column(width=3, uiOutput(paste0("capital_cost_total",x)))                         
                      ), br(), 
                      fluidRow(column(width=8, 
                                      h5("Total negative impacts")),
                               column(width=3,  offset=1, uiOutput(paste0("pb_negative_total",x)))                        
                      )
                    ), br() 
             )),   
    
    fluidRow(
      column(width=8, offset=2, 
             helpText("Asterisk (*) symbol indicates that the value is annualized
                    into a constant income or payment (called annuity) during the planning horizon."),
             a(id = paste0("PB_plot_show_1",x),"Show/hide more explanation")
      )),
    shinyjs::hidden(
      div(id = paste0("id_PB_plot_show_1",x),
          fluidRow(column(width=8, offset=2, 
                          helpText(em("Increased Revenue Total"),"and", em("Increased Expense Total"), 
                                   "show the projected values for a given budget year, according to the inflation rate of IOFC margin   
                    (from the Data Entry section). Similarly,", em("Decreased Expense Total"), 
                                   "is the projected value according to the inflation rate of labor. 
                    On the other hand,", em("Increased Cost of Capital"), "is presented as a constant, annualized value."), 
                          helpText("The figure below presents the trajectories of these variables over budget years.") 
          )), br(),
          fluidRow(
            column(width=10, offset=1,  
                   htmlOutput(paste0("PB_plot_pos_neg_impact",x))
            )), br(),
          fluidRow(
            column(width=8, offset=2, 
                   helpText("In the following, we summarize how to obtain calculations from the positive and negative impacts above
           to before-tax and after-tax impacts. 
           We present such a summary by using annualized values. The adjustment, labeled", em("Inflation adjustments,"),
                            " is introduced between a (budget year-specific)", em("Total positive impacts minus total negative impacts"),
                            "and a (constant) ", em("Before-tax Net annual impact*."),  
            "Furthermore, the addition of annualized tax and tax deductions, 
            as well as some adjustments to the cost of capital, leads to",  em("After-tax net annual impact*."))
            ))
      )),
    # -------- Inflation adjustments and Taxes ------- 
    fluidRow(column(width=8, offset=2, 
                    hr(),
                    fluidRow(column(width=9, offset=0, 
                                    h5("Total positive impacts minus total negative impacts")),
                             column(width=3,  uiOutput(paste0("pb_positive_minus_negative",x)))
                    ), 
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Inflation adjustments")),
                             column(width=3, uiOutput(paste0("pb_inflation_adjustment",x)))
                    ),
                    div(id=paste0(x,"_PB",2,1),
                        fluidRow(column(width=10, offset=1, 
                                        helpText("This item includes adjustments to delayed",refProfileName(x), "installment. 
                                        Change", em("Budget Year"), "at the top of the page
                                        to see how these adjustments are used to 
                                        link", em("Total positive impacts minus total negative impacts"), 
                                                 "and", em("Before-tax Net annual impact*."))
                        ))
                    ),
                    hr(),
                    fluidRow(column(width=9, offset=0, 
                                    h5("Before-tax net annual impact*")),
                             column(width=3,  uiOutput(paste0("pb_net_annual_impact_before_tax",x)))
                    ), 
                    br(),
                    fluidRow(column(width=9, helpText("Tax (negative) and Tax Deductions (positive):"))),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Tax on change in revenue minus expense*")),
                             column(width=3, uiOutput(paste0("tax_revenue_minus_expense",x)))
                    ), 
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Tax deduction on interests*")),
                             column(width=3,uiOutput(paste0("tax_interest",x)))
                    ), 
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Tax deduction on depreciation*")),
                             column(width=3, uiOutput(paste0("tax_depreciation",x)))
                    ), 
                    fluidRow(column(width=9, helpText("WACC Adjustments to Annualized Cash Flow Values:"))),
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Adjustments with respect to interest rate*")),
                             column(width=3,uiOutput(paste0("adj_WACC_interest",x)))
                    ), 
                    fluidRow(column(width=8, offset=1, 
                                    helpText("Adjustments with respect to hurdle rate*")),
                             column(width=3, uiOutput(paste0("adj_WACC_hurdle",x)))
                    ),
                    hr(),
                    fluidRow(column(width=9, offset=0, 
                                    h5("After-tax net annual impact*")),
                             column(width=3, uiOutput(paste0("net_annual_impact_after_tax",x)))
                    ), br(),
                    a(id = paste0("PB_plot_show_2",x),"Show/hide more explanation")
    )), 
    br(), 
    shinyjs::hidden(
      div(id = paste0("id_PB_plot_show_2",x),
          fluidRow(column(width=8, offset=2, 
                          helpText("The figure below illustrates what we call ", em("Inflation Adjustments,"), " which is the difference between",
                                   em("the (time-variant) Positive-minus-Negative Impacts"), " and",
                                   em("the (constant) Annualized Before-tax Impact."),
                                   "Thus, by definition, the sum of", em("the (time-variant) Positive-minus-Negative Impacts"), "and ", 
                                   em("Inflation Adjustments"),
                                   " is", em("the (constant) Annualized Before-tax Impact.")) 
          )), 
          br(),
          fluidRow(column(width=10, offset=1, 
                          htmlOutput(paste0("PB_plot_before_tax_impact",x))
          )),
          fluidRow(column(width=8, offset=2, 
                          helpText(" Annualized values such as", em("the Annualized Before-tax Impact"),"in the figure represent a form of 
              valuation that converts a stream of cash flows into
              a stream of constant payments (called annuity) that has the same value under a given discount rate.  
                             This calculation is straightforward; we first convert a stream of cash flow into a discounted sum, 
                             or the Net Present Value (NPV), and then convert the NPV into a constant annual payment, or the annuity, 
                             of the equivalent value."), br(), 
                          
                          helpText("The two figures below present the cash flows and their annualized values.
               It illustrates why it can be useful to annualize cash flows for evaluating the relative importance of 
               each variable that fluctuates over time. In the figures,", 
                                   em("Before-tax cashflow"), "accounts for", em("Total Positive Impact"), em("Increased Expense Total"), "and",
                                   em("the Cost of Capital"), "(interest payments, principal payments, downpayments,
                and salvage sales revenue). 
               Adding taxes, tax deductions, and minor capital cost adjustments to", em("Before-tax Cashflow"), 
                                   "yields", em("After-tax Cashflow."))  
          )), br(),
          fluidRow(column(width=10, offset=1, 
                          htmlOutput(paste0("PB_plot_after_tax_impact",x)),
                          htmlOutput(paste0("PB_plot_after_tax_impact_const",x))
          )), br(),
          fluidRow(column(width=8, offset=2,
                          helpText("The annualization above depends on the discount rate.
                             The Partial Budget calculation here primarily uses a mix of interest rates 
                              for milking system and housing loans and
                             the hurdle rate for downpayments. 
                             The Cashflow Analysis (in the next tab) calculation uses a weighted average
                             of these rates, or the Weighted Average Cost of Capital (WACC). The WACC adjustments above refer to
                             the diffences attributable to the difference in the discount rate used for annualization.")
          ))
      )), br(), 
    # -------- Breakeven wage ------- 
    
    fluidRow(column(width=8, offset=2,
                    hr(),
                    h5(paste("Breakeven Wage:", refProfileName(x))),
                    helpText("The primary benefit of installing a new milking system is the reduction of labor requirement, and its
                         importance depends on wage rate. Here, we ask under what wage rate it is sensible to invest in", x, ".",
                             "Holding everything else constant, any wage rate higher than the breakeven wage 
                         implies profits from the investment, while any wage rate lower than the breakeven wage implies 
                         a loss."),
                    helpText("Given the long-term nature of the investment at hand, 
                         we define the breakeven wage as a sequence of labor wage rates over the planning horizon   
                        that sets ", em("After-tax net annual impact"), "approximately zero. 
                         Here are the two cases of such a wage sequence we obtained:"), 
                    fluidRow(column(9,offset=1, helpText("(a) The starting wage rate, while keeping the wage inflation fixed:
                                                     (any starting wage rage higher than the calculated value implies profits)")),
                             column(2, uiOutput(paste0("bw_wage_after_tax",x)))
                    ),br(), 
                    fluidRow(column(9,offset=1, helpText( "(b) The wage inflation/deflation rate, while keeping the starting wage rate fixed: 
                                                      (any wage inflation rate higher than the calculated value implies profits)")),
                             column(2, uiOutput(paste0("bw_wage_inflation_after_tax",x)))
                    ),
                    helpText("The figure below illustrates trajectories of wage rates for the baseline and the two cases of
                         breakeven wage calculations. If the baseline wage trajectory stays above the breakeven trajectories,
                         the investment is profitable, holding everything else constant.") 
    )),
    fluidRow(column(width=10, offset=1, 
                    htmlOutput(paste0("breakeven_chart",x)) 
    )), br(), br() 
    # #tags$a(href ="#data_entry",  #"#tab-9037-2",
    # ## I haven't been able to set a link to a tab. It seems compliated in Shiny.
    # div(id="goData", class="well", style="background-color: gray; color:white;", 
    #     align="center", 
    #     h4("See interactive dashboard under the Data Entry tab")
    # ), 
    # br(),br() 
  )   
} 
