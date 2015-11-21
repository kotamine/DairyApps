div(      
  fluidRow(column(width=6, offset=3,
                         radioButtons("robot_parlor","Robots vs Parlors Comparison",choices=c("OFF","ON"), inline=TRUE), 
                           helpText("The default investment profile is 'Robots'. 
                           When 'Robots vs Parlors Comparison' is activated, 
                           the user can adopt a different profile in Data Entry tab.") 
            )),
         fluidRow(
           column(
             width=1),
           column(
             width=10, 
             conditionalPanel('input.robot_parlor=="ON"',
             div(h4("Robots vs Parlors Analysis (Alternative Investment Profiles)"), align="center"),
             div(a(id = "readProfile","Show/hide explanations about the profiles"),align="center"),
             shinyjs::hidden(
               div(id = "ref_readProfile",
                   includeMarkdown(file.path("text","profiles.md"))
                   )), br(),  
            fluidRow(column(width=2, offset=5, bsButton("robot_parlor_calculate", "Calculate",style="primary")),
            column(4,bsAlert("p_input_change"))), br(),
            conditionalPanel('input.robot_parlor_calculate>=1', 
            h4("Analysis Results: Summary"),
            div(htmlOutput("profile_impacts"),
                 tabsetPanel(
                   tabPanel("Operating Income",
                            htmlOutput("profile_operating_income_chart")),
                   tabPanel("Cashflow",
                            htmlOutput("profile_cashflow_chart")),
                 selected="Cashflow"),
                 align="center"),
            a(id = "tableProfile","Show/hide tables of summary results"),
            shinyjs::hidden(
              div(id = "tableProfileSummary",
            tabsetPanel(
              tabPanel("Before Tax",
                       DT::dataTableOutput("table_before_tax")
              ), 
              tabPanel("After Tax",
                       DT::dataTableOutput("table_after_tax")
              ),
              tabPanel("Operating Income",
                       DT::dataTableOutput("table_operating_income")
              ),
              tabPanel("Cash Flow",
                       DT::dataTableOutput("table_after_tax_cash_flow")
              ))
            )), 
            br(), br(),     
             h4("Analysis Data and Assumptions"),
             helpText("Data entered below are linked with the investment profile that can be selected in Data Entry tab.
                      The details of the results under the selected profile will appear under Partial Budget and Cash Flow tabs.
                      The underlying calculations are the identical across profiles, but different data inputs lead 
                      to different results for projected net impacts."), 
             br(),
             hr(),
             h5("Section A. Anticipated Changes in Production"),
             div(style="background-color: #4863A0; color:white;",
                           fluidRow(
                             column(width=8,offset=4, 
                                           h4(strong("Investment Profiles"), align="center")
                                           )),
                           fluidRow(column(width=4,  h5(strong("Item"),align="center")),
                                    column(width=2,  h5(strong("Barn Only"),  align="center")),
                                    column(width=2,  h5(strong("Retrofit Parlors"),  align="center")),
                                    column(width=2,  h5(strong("New Parlors"), align="center")),
                                    column(width=2,  h5(strong("Robots"), align="center"))
                           )), br(),
            fluidRow(column(width=4, h5("Herd"))),
            fluidRow(column(width=4, helpText("Anticipated increase in milking herd (Animals)")),
                     column(width=2, numericInput("herd_increase_pr1",NULL,value=0,min=0,step=10)),
                     column(width=2, numericInput("herd_increase_pr2",NULL,value=0,min=0,step=10)),
                     column(width=2, numericInput("herd_increase_pr3",NULL,value=0,min=0,step=10)),
                     column(width=2, numericInput("herd_increase_pr4",NULL,value=0,min=0,step=10))
            ),
             fluidRow(column(width=4, h5("Maintenance"))),
             fluidRow(column(width=4, helpText("Estimated annual change in milking system repair ($)")),
                      column(width=2, numericInput("repair_pr1",NULL,value=0,min=0,step=500)),
                      column(width=2, numericInput("repair_pr2",NULL,value=1000,min=0,step=500)),
                      column(width=2, numericInput("repair_pr3",NULL,value=1000,min=0,step=500)),
                      column(width=2, numericInput("repair_pr4",NULL,value=7000,min=0,step=500))
             ),
             fluidRow(column(width=4, helpText("Insurance rate per $1000 value (%)")),
                      column(width=2, numericInput("insurance_rate_pr1",NULL,value=0.5,min=0,step=0.1)),
                      column(width=2, numericInput("insurance_rate_pr2",NULL,value=0.5,min=0,step=0.1)),
                      column(width=2, numericInput("insurance_rate_pr3",NULL,value=0.5,min=0,step=0.1)),
                      column(width=2, numericInput("insurance_rate_pr4",NULL,value=0.5,min=0,step=0.1))
             ),
             fluidRow(column(width=4, h5("Labor"))),
             fluidRow(column(width=4, helpText("Anticipated savings in milking & chore labor (hrs/day)")),
                      column(width=2, numericInput("hr_sv_milking_pr1",NULL,value=4.1,step=0.2)),
                      column(width=2, numericInput("hr_sv_milking_pr2",NULL,value=8.1,step=0.2)),
                      column(width=2, numericInput("hr_sv_milking_pr3",NULL,value=8.9,step=0.2)),
                      column(width=2, numericInput("hr_sv_milking_pr4",NULL,value=10.82,step=0.2))
             ),
             fluidRow(column(width=4, helpText("Anticipated hours of heat detection (hrs/day)")),
                      column(width=2, numericInput("anticipated_hours_heat_pr1",NULL,value=0.65,step=0.05)),
                      column(width=2, numericInput("anticipated_hours_heat_pr2",NULL,value=0.65,step=0.05)),
                      column(width=2, numericInput("anticipated_hours_heat_pr3",NULL,value=0.65,step=0.05)),
                      column(width=2, numericInput("anticipated_hours_heat_pr4",NULL,value=0.25,step=0.05))
             ),
             fluidRow(column(width=4, helpText("Increased hours of records management (hrs/day)")),
                      column(width=2, numericInput("increase_rc_mgt_pr1",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("increase_rc_mgt_pr2",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("increase_rc_mgt_pr3",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("increase_rc_mgt_pr4",NULL,value=0.6,step=0.1))
             ),
             fluidRow(column(width=4, helpText("Reduced hours of labor management (hrs/day)")),
                      column(width=2, numericInput("decrease_lab_mgt_pr1",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("decrease_lab_mgt_pr2",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("decrease_lab_mgt_pr3",NULL,value=0,step=0.1)),
                      column(width=2, numericInput("decrease_lab_mgt_pr4",NULL,value=0.6,step=0.1))
             ),
             fluidRow(column(width=4, h5("Milk Outputs"))),
             fluidRow(column(width=4, helpText("Projected change in milk production (lb/cow/day)")),
                      column(width=2, numericInput("milk_change_pr1",NULL,value=6,step=2)),
                      column(width=2, numericInput("milk_change_pr2",NULL,value=6,step=2)),
                      column(width=2, numericInput("milk_change_pr3",NULL,value=6,step=2)),
                      column(width=2, numericInput("milk_change_pr4",NULL,value=10,step=2))
             ),
             fluidRow(column(width=4, helpText("Estimated percent change in SCC (%)")),
                      column(width=2, numericInput("scc_change_pr1",NULL,value=-5,step=0.25)),
                      column(width=2, numericInput("scc_change_pr2",NULL,value=-5,step=0.25)),
                      column(width=2, numericInput("scc_change_pr3",NULL,value=-5,step=0.25)),
                      column(width=2, numericInput("scc_change_pr4",NULL,value=-5,step=0.25))
             ),
             fluidRow(column(width=4, helpText("Reproduction and herd health value of software ($/cow/year)")),
                      column(width=2, numericInput("software_pr1",NULL,value=0,step=5)),
                      column(width=2, numericInput("software_pr2",NULL,value=0,step=5)),
                      column(width=2, numericInput("software_pr3",NULL,value=0,step=5)),
                      column(width=2, numericInput("software_pr4",NULL,value=35,step=5))
             ),
             fluidRow(column(width=4, h5("Feed"))),
             fluidRow(column(width=4, helpText("Pellets fed in robot booth (lb/cow/day)")),
                      column(width=2, numericInput("pellets_pr1",NULL,value=0,step=2)),
                      column(width=2, numericInput("pellets_pr2",NULL,value=0,step=2)),
                      column(width=2, numericInput("pellets_pr3",NULL,value=0,step=2)),
                      column(width=2, numericInput("pellets_pr4",NULL,value=11,step=2))
             ),
             fluidRow(column(width=4, helpText("Extra cost for pellets fed in robot booth ($/ton)")),
                      column(width=2, numericInput("cost_pellets_pr1",NULL,value=0,step=2)),
                      column(width=2, numericInput("cost_pellets_pr2",NULL,value=0,step=2)),
                      column(width=2, numericInput("cost_pellets_pr3",NULL,value=0,step=2)),
                      column(width=2, numericInput("cost_pellets_pr4",NULL,value=20,step=2))
             ),
             fluidRow(column(width=4, h5("Replacement"))),
             fluidRow(column(width=4, helpText("Anticipated change in annual turnover rate (%)")),
                      column(width=2, numericInput("change_turnover_pr1",NULL,value=-1,step=0.25)),
                      column(width=2, numericInput("change_turnover_pr2",NULL,value=-1,step=0.25)),
                      column(width=2, numericInput("change_turnover_pr3",NULL,value=-1,step=0.25)),
                      column(width=2, numericInput("change_turnover_pr4",NULL,value=-1,step=0.25))
             ),
             fluidRow(column(width=4, h5("Utilities"))),
             fluidRow(column(width=4, helpText("Anticipated change in electricity cost ($/cow/year)")),
                      column(width=2, numericInput("change_electricity_pr1",NULL,value=8.25,step=0.25)),
                      column(width=2, numericInput("change_electricity_pr2",NULL,value=8.25,step=0.25)),
                      column(width=2, numericInput("change_electricity_pr3",NULL,value=8.25,step=0.25)),
                      column(width=2, numericInput("change_electricity_pr4",NULL,value=8.25,step=0.25))
             ),
             fluidRow(column(width=4, helpText("Anticipated change in water cost ($/cow/year)")),
                      column(width=2, numericInput("change_water_pr1",NULL,value=-3.00,step=0.25)),
                      column(width=2, numericInput("change_water_pr2",NULL,value=-3.00,step=0.25)),
                      column(width=2, numericInput("change_water_pr3",NULL,value=-3.00,step=0.25)),
                      column(width=2, numericInput("change_water_pr4",NULL,value=-3.00,step=0.25))
             ),
             fluidRow(column(width=4, helpText("Anticipated change in chemical cost ($/cow/year) ")),
                      column(width=2, numericInput("change_chemical_pr1",NULL,value=1.50,step=0.25)),
                      column(width=2, numericInput("change_chemical_pr2",NULL,value=1.50,step=0.25)),
                      column(width=2, numericInput("change_chemical_pr3",NULL,value=1.50,step=0.25)),
                      column(width=2, numericInput("change_chemical_pr4",NULL,value=1.50,step=0.25))
             ),
            helpText("*Note: all other production variables are shared across investment profiles."), 
         # ----- Financing ------
             hr(),
             h5("Section B. Financing Schedule by Profile") , 
             tabsetPanel(
               tabPanel("Barn Only",
             div(style="background-color:#4863A0; color:white;",
                 fluidRow(column(width=6, offset=4, h4(strong("Investment Assets"),align="center"))),
                 fluidRow(column(width=4,  h5(strong("Item"), align="center")),
                          column(width=2,  h5(strong("Housing"), align="center"))
                 )), br(),
             fluidRow(column(width=4, helpText("Year of investment")),
                      column(width=2, helpText("0"))
                      ), 
             fluidRow(column(width=4,  helpText("Housing Investment per cow ($)")), 
                      column(width=2, numericInput("cost_housing_cow_pr1",NULL,value=3000,min=0,step=500))
             ),
             fluidRow(column(width=4,  helpText("Investment amount ($)")), 
                      column(width=2, uiOutput("copy_cost_housing_pr1")), 
                      shinyjs::hidden(column(width=2,  numericInput("cost_parlors_pr1",NULL,value=0)))
                             ), 
             fluidRow(column(width=4, helpText("Years of Useful Life")),
                      column(width=2, numericInput("milking_years_pr1",NULL,value=30,min=0,step=1)) 
                      # This becomes the planning horizon
             ), 
             fluidRow(column(width=4, helpText("Down payment ($)")),
                      column(width=2,  
                             numericInput("down_housing_pr1",NULL,value=100000, min=0,step=20000)),
                      shinyjs::hidden(column(width=2, numericInput("down_milking1_pr1",NULL,value=0)))
             ), 
             fluidRow(column(width=4,  helpText("Loan amount ($)")),
                      column(width=2,  uiOutput("loan_housing_pr1")),
                      shinyjs::hidden(column(width=2, numericInput("loan_milking1_pr1", NULL,value=0)))
             ),
             fluidRow(column(width=4,  helpText("Interest rate (%)")),
                      column(width=2,  uiOutput('copy_r_housing_pr1'))
             ),
             shinyjs::hidden(  # Currently unused. I don't know if we want to use separate interest rates across assets 
               fluidRow(column(width=4,  helpText("Interest rate (%)")),
                        column(width=2,  numericInput("r_housing_pr1",NULL,value=4, min=0, step=.25)),
                        column(width=2, numericInput("r_milking1_pr1", NULL,value=0))
               )), 
             fluidRow(column(width=4,  helpText("Loan period (years)")),
                      column(width=2,  numericInput("n_yr_housing_pr1",NULL,value=24, min=0, step=1)),
                      shinyjs::hidden(column(width=2, numericInput("n_yr_milking1_pr1", NULL,value=1)))
                      ),
             fluidRow(column(width=4,  helpText("Salvage value ($)")),
                      column(width=2,  helpText("0")),
                      shinyjs::hidden(column(width=2, numericInput("salvage_milking1_pr1", NULL,value=0)))
                      )
               ),
             tabPanel("Retro Parlor",
               div(style="background-color:#4863A0; color:white;",
                   fluidRow(column(width=6, offset=4, h4(strong("Investment Assets"),align="center"))),
                   fluidRow(column(width=4,  h5(strong("Item"), align="center")),
                            column(width=2,  h5(strong("Housing"), align="center")),
                            column(width=2,  h5(strong("Rertrofit"), align="center"))
                   )), br(),
               fluidRow(column(width=4, helpText("Year of investment")),
                        column(width=2, helpText("0")),
                        column(width=2, helpText("0"))
                      #  column(width=2, numericInput("yr_invest_milking1_pr2",NULL,value=0,min=0,step=1))
               ),
               fluidRow(column(width=4, helpText("Years of Useful Life")),
                        column(width=2, uiOutput("housing_years_pr2")), # This becomes the planning horizon
                        column(width=2, numericInput("milking_years_pr2",NULL,value=30,min=0,step=1))
               ), 
               fluidRow(column(width=4,  helpText("Housing Investment per cow ($)")), 
                        column(width=2, numericInput("cost_housing_cow_pr2",NULL,value=3000,min=0,step=500))
               ), 
               fluidRow(column(width=4,  helpText("Investment amount ($)")), 
                        column(width=2, uiOutput("copy_cost_housing_pr2")),
                               column(width=2, numericInput("cost_parlors_pr2",NULL,value=75000,min=0,step=20000))
                        ), 
               fluidRow(column(width=4, helpText("Down payment ($)")),
                        column(width=2,  
                               numericInput("down_housing_pr2",NULL,value=100000, min=0,step=20000)),
                        column(width=2,  
                               numericInput("down_milking1_pr2",NULL,value=0, min=0,step=20000))
               ), 
               fluidRow(column(width=4,  helpText("Loan amount ($)")),
                        column(width=2,  uiOutput("loan_housing_pr2")),
                        column(width=2,  uiOutput("loan_milking1_pr2"))
               ),
               fluidRow(column(width=4,  helpText("Interest rate (%)")),
                        column(width=2,  uiOutput('copy_r_housing_pr2')),
                        column(width=2,  uiOutput('copy_r_milking1_pr2'))
               ),
               shinyjs::hidden(  # Currently unused. I don't know if we want to use separate interest rates across assets 
                 fluidRow(column(width=4,  helpText("Interest rate (%)")),
                          column(width=2,  numericInput("r_housing_pr2",NULL,value=4, min=0, step=.25)),
                          column(width=2,  numericInput("r_milking1_pr2",NULL,value=4, min=0, step=.25))
                 )), 
               
               fluidRow(column(width=4,  helpText("Loan period (years)")),
                        column(width=2,  numericInput("n_yr_housing_pr2",NULL,value=24, min=0, step=1)),
                        column(width=2,  numericInput("n_yr_milking1_pr2",NULL,value=12, min=0, step=1))
               ),
               fluidRow(column(width=4,  helpText("Salvage value ($)")),
                        column(width=2,  helpText("0")),
                        column(width=2,  numericInput("salvage_milking1_pr2",NULL,value=25000, min=0, step=5000))
               )
             ),
             tabPanel("New Parlor",
               div(style="background-color:#4863A0; color:white;",
                   fluidRow(column(width=6, offset=4, h4(strong("Investment Assets"),align="center"))),
                   fluidRow(column(width=4,  h5(strong("Item"), align="center")),
                            column(width=2,  h5(strong("Housing"), align="center")),
                            column(width=2,  h5(strong("Parlor"), align="center"))
                   )), br(),
               fluidRow(column(width=4, helpText("Year of investment")),
                        column(width=2, helpText("0")),
                        column(width=2, helpText("0"))
                        #column(width=2, numericInput("yr_invest_milking1_pr3",NULL,value=0,min=0,step=1))
               ), 
               fluidRow(column(width=4,  helpText("Housing Investment per cow ($)")), 
                        column(width=2, numericInput("cost_housing_cow_pr3",NULL,value=3000,min=0,step=500))
               ), 
               fluidRow(column(width=4,  helpText("Investment amount ($)")), 
                        column(width=2, uiOutput("copy_cost_housing_pr3")),
                        column(width=2, numericInput("cost_parlors_pr3",NULL,value=400000,min=0,step=20000))
                ), 
               fluidRow(column(width=4, helpText("Years of Useful Life")),
                        column(width=2, uiOutput("housing_years_pr3")), # This becomes the planning horizon
                        column(width=2, numericInput("milking_years_pr3",NULL,value=30,min=0,step=1))
                        ), 
               fluidRow(column(width=4, helpText("Down payment ($)")),
                        column(width=2,  
                               numericInput("down_housing_pr3",NULL,value=100000, min=0,step=20000)),
                        column(width=2,  
                               numericInput("down_milking1_pr3",NULL,value=0, min=0,step=20000))
               ), 
               fluidRow(column(width=4,  helpText("Loan amount ($)")),
                        column(width=2,  uiOutput("loan_housing_pr3")),
                        column(width=2,  uiOutput("loan_milking1_pr3"))
               ),
               fluidRow(column(width=4,  helpText("Interest rate (%)")),
                        column(width=2,  uiOutput('copy_r_housing_pr3')),
                        column(width=2,  uiOutput('copy_r_milking1_pr3'))
               ),
               shinyjs::hidden(  # Currently unused. I don't know if we want to use separate interest rates across assets 
                 fluidRow(column(width=4,  helpText("Interest rate (%)")),
                          column(width=2,  numericInput("r_housing_pr3",NULL,value=4, min=0, step=.25)),
                          column(width=2,  numericInput("r_milking1_pr3",NULL,value=4, min=0, step=.25))
                 )), 
               fluidRow(column(width=4,  helpText("Loan period (years)")),
                        column(width=2,  numericInput("n_yr_housing_pr3",NULL,value=24, min=0, step=1)),
                        column(width=2,  numericInput("n_yr_milking1_pr3",NULL,value=12, min=0, step=1))
               ),
               fluidRow(column(width=4,  helpText("Salvage value ($)")),
                        column(width=2, helpText("0")),
                        column(width=2, numericInput("salvage_milking1_pr3",NULL,value=25000, min=0, step=5000))
               )
             ),
             tabPanel("Robots",
                      div(style="background-color:#4863A0; color:white;",
                   fluidRow(column(width=6, offset=4, h4(strong("Investment Assets"),align="center"))),
                   fluidRow(column(width=4,  h5(strong("Item"), align="center")),
                            column(width=2,  h5(strong("Housing"), align="center")),
                            column(width=2,  h5(strong("Robot 1"), align="center")),
                            conditionalPanel("input.n_robot_life>=2", 
                                             column(width=2,  h5(strong("Robot 2"), align="center")))
                   )), br(),
#                fluidRow(column(width=4, helpText("Year of investment")),
#                         column(width=2, helpText("0")),
#                         column(width=2, numericInput("yr_invest_milking1_pr4",NULL,value=0,min=0,step=1)),
#                         column(width=2, uiOutput("yr_invest_milking2_pr4"))
#                ), 
                fluidRow(column(width=4, helpText("Year of investment")),
                         column(width=2, helpText("0")),
                         column(width=2, helpText("0")),
                         conditionalPanel("input.n_robot_life>=2", 
                                          column(width=2, uiOutput("yr_invest_milking2_pr4")))
                ), 
               fluidRow(column(width=4, helpText("Years of Useful Life")),
                        column(width=2, uiOutput("housing_years_pr4")), # This becomes the planning horizon
                        column(width=2, numericInput("robot_years_pr4",NULL,value=15,min=0,step=1)),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2, uiOutput("copy_robot_years_pr4")))
               ), 
               fluidRow(column(width=4,  helpText("Housing Investment per cow ($)")), 
                        column(width=2, numericInput("cost_housing_cow_pr4",NULL,value=9500,min=0,step=500))
               ), 
              fluidRow(column(width=4, helpText("Number of robots")),
                       column(width=2, offset=2, numericInput("n_robot_pr4",NULL,value=2,min=0,step=1)),
                       column(width=2, uiOutput("copy_n_robot_pr4"))
              ),
              fluidRow(column(width=4, helpText("Estimated cost per robot ($)")),
                       column(width=2, offset=2, numericInput("cost_robot_pr4",NULL,value=180000,min=50000,step=10000)),
                       column(width=2, uiOutput("copy_cost_robot_pr4"))
              ),
               fluidRow(column(width=4,  helpText("Investment amount ($)")), 
                        column(width=2, uiOutput("copy_cost_housing_pr4")),
                        column(width=2, uiOutput("copy_cost_milking1_pr4")),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2, uiOutput("copy_cost_milking2_pr4")))
               ), 
               fluidRow(column(width=4, helpText("Down payment ($)")),
                        column(width=2,  
                               numericInput("down_housing_pr4",NULL,value=100000, min=0,step=20000)),
                        column(width=2,  
                               numericInput("down_milking1_pr4",NULL,value=0, min=0,step=20000)),
                        conditionalPanel("input.n_robot_life>=2",  
                              column(width=2,  
                               numericInput("down_milking2_pr4",NULL,value=0, min=0,step=20000)))
               ),
               fluidRow(column(width=4,  helpText("Loan amount ($)")),
                        column(width=2,  uiOutput("loan_housing_pr4")),
                        column(width=2,  uiOutput("loan_milking1_pr4")),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2,  uiOutput("loan_milking2_pr4")))
               ),
                fluidRow(column(width=4,  helpText("Interest rate (%)")),
                         column(width=2,  uiOutput('copy_r_housing_pr4')),
                         column(width=2,  uiOutput('copy_r_milking1_pr4')),
                         conditionalPanel("input.n_robot_life>=2", 
                                          column(width=2, uiOutput('copy_r_milking2_pr4')))
                ),
                shinyjs::hidden(  # Currently unused. I don't know if we want to use separate interest rates across assets 
               fluidRow(column(width=4,  helpText("Interest rate (%)")),
                        column(width=2,  numericInput("r_housing_pr4",NULL,value=4, min=0, step=.25)),
                        column(width=2,  numericInput("r_milking1_pr4",NULL,value=4, min=0, step=.25)),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2,  numericInput("r_milking2_pr4",NULL,value=4, min=0, step=.25)))
               )), 
               fluidRow(column(width=4,  helpText("Loan period (years)")),
                        column(width=2,  numericInput("n_yr_housing_pr4",NULL,value=24, min=0, step=1)),
                        column(width=2,  numericInput("n_yr_milking1_pr4",NULL,value=12, min=0, step=1)),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2,  numericInput("n_yr_milking2_pr4",NULL,value=12, min=0, step=1)))
               ),
               fluidRow(column(width=4,  helpText("Salvage value ($)")),
                        column(width=2,  helpText("0")),
                        column(width=2,  numericInput("salvage_milking1_pr4",NULL,value=45000, min=0, step=5000)),
                        conditionalPanel("input.n_robot_life>=2", 
                                         column(width=2,  uiOutput("salvage_milking2_pr4")))
               )
             ),
             selected="Robots"
           )
         )
         ))), br(), br() 
) 
