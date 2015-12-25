
# ----- Date Entry Tabs ------
div( 
  navlistPanel("Current Operation",  id="navlistPanel",
               ## ---------------  Baseline --------------- 
               tabPanel("Operation", value="Operation",
                        #                         herdsize, milk output, average scc, 
                        #                         milking labor, heat detection, feed calc.
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            h4("Shared across All Investment Profiles", align="center")
                          )),
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            div(style="background-color:#4863A0; color:white;",
                                          fluidRow(column(width=8, 
                                                          h5(strong("Item"),align="center")),
                                                   column(width=4, h5(strong("User Data"), 
                                                                      align="center"))
                                          )
                                ), br(),
                            fluidRow(column(width=8, 
                                            helpText("Current herd size (milking & dry animals)")),
                                     column(width=4, 
                                            numericInput("herd_size",NULL,value=120,min=30,step=10))
                                     ),
                            fluidRow(column(width=8, 
                                            helpText("Milk per cow per day, past year (lbs/cow/day)")),
                                     column(width=4, 
                                            numericInput("milk_cow_day",NULL,value=75,min=0,step=5))
                            ),
                            fluidRow(column(width=8, 
                                            helpText("Current annual bulk tank average SCC (SCC/ml)")),
                                     column(width=4, 
                                            numericInput("scc_average",NULL,value=240000,min=0,step=10000))
                                     ), 
                            fluidRow(column(width=8, helpText("Current hours of milking & chore labor (hours/day)")),
                                     column(width=4, numericInput("hours_milking",NULL,value=17,min=0,step=1))
                            ),
                            fluidRow(column(width=8, helpText("Current hours of heat detection (hours per day)")),
                                     column(width=4, numericInput("hr_heat_detection",NULL,value=0.65,min=0,step=.05))
                            )
#                             fluidRow(column(width=8, helpText("Projected change in dry matter intake (DMI) per day (lbs DM/day)")),
#                                      column(width=4, uiOutput("DMI_change"))
#                             ),
                           
                            )),  
                        icon=icon("home")), 
               tabPanel("Markets", value="Markets",
                        #                         milk price, heifer price, cull price,  
                        #                         labor rate milking, labor rate management
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            h4("Shared across All Investment Profiles", align="center")
                          )),
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            div(style="background-color:#4863A0; color:white;",
                                fluidRow(column(width=8, 
                                                h5(strong("Item"),align="center")),
                                         column(width=4, h5(strong("User Data"), 
                                                            align="center"))
                                )
                            ), br(),
                            fluidRow(column(width=8, helpText("Mailbox milk price ($/cwt)")),
                                     column(width=4, numericInput("price_milk",NULL,value=17.50,min=0,step=0.25))
                            ),
                            fluidRow(column(width=8, helpText("SCC premium per 1,000 SCC (SCC/ml)")),
                                     column(width=4, numericInput("scc_premium",NULL,value=0.003,min=0,step=.001))
                            ),
                            fluidRow(column(width=8, helpText("Cost per lb of TMR dry matter ($ per lb DM)")),
                                     column(width=4, numericInput("cost_DM",NULL,value=0.115,min=0,step=0.005))
                            ),
                            fluidRow(column(width=8, helpText("Cost of replacement heifer ($)")),
                                     column(width=4,  numericInput("cost_heifer",NULL,value=1600,min=0,step=100))
                            ),
                            fluidRow(column(width=8, helpText("Cull price per cow ($)")),
                                     column(width=4,  numericInput("cull_price",NULL,value=750,min=0,step=50))
                            ), 
                            fluidRow(column(width=8, helpText("Labor rate for milking and heat detection ($/hour)")),
                                     column(width=4, numericInput("labor_rate",NULL,value=15.00,min=0,step=0.25))
                            ),
                            fluidRow(column(width=8, helpText("Labor rate for records and labor management ($/hour)")),
                                     column(width=4, numericInput("labor_rate_rc_mgt",NULL,value=18.00,min=0,step=0.25))
                            )
                          )),
                        icon=icon("bell-o")),
               tabPanel("Inflations", value="Inflations",
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            h4("Shared across All Investment Profiles", align="center")
                          )),
                        fluidRow(
                          column(
                            width=10, offset=1, 
                            div(style="background-color:#4863A0; color:white;",
                                fluidRow(column(width=8, 
                                                h5(strong("Item"),align="center")),
                                         column(width=4, h5(strong("User Data"), 
                                                            align="center"))
                                )
                            ), br(),
                            fluidRow(column(width=8, helpText("Robot/parlor & related-housing prices (%)")),
                                     column(width=4, numericInput("inflation_robot",NULL,value=1.5,step=0.25))
                            ),
                            fluidRow(column(width=8, helpText("Margin milk over feed & operation per cow with robots (%)")),
                                     column(width=4, numericInput("inflation_margin",NULL,value=0.2,step=0.25))
                            ),
                            fluidRow(column(width=8, helpText("Milking & chore labor rate per hour (%)")),
                                     column(width=4, numericInput("inflation_labor",NULL,value=1.5,step=0.25))
                            ),
                            fluidRow(column(width=8, helpText("Hurdle rate for equity (%)")),   
                                     column(width=4, numericInput("hurdle_rate",NULL,value=4, min=0, step=.1))
                            ),
                            fluidRow(column(width=8, helpText("Marginal (federal + state) income tax rate (%)")),   
                                     column(width=4, numericInput("tax_rate",NULL,value=40, min=0, step=2))
                            ), br(),
                            radioButtons("dep_method","Depreciation accounting method:",
                                         choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2"))
                            )),
                        icon=icon("money")),
               ## --------------- Profile-specific Inpiuts  --------------- 
               "Change",
                tabPanel("Capital", value="Capital",
                         fluidRow(column(width=10, offset=1,
                         tabsetPanel(id="prCapital", 
                                     tabPanel("Robots", value=base_profiles[1],
                                              changeVariablesCapital(base_profiles[1])),
                                     tabPanel("Retrofit Parlors", value=base_profiles[2],
                                              changeVariablesCapital(base_profiles[2])),
                                     tabPanel("New Parlors", value=base_profiles[3],
                                              changeVariablesCapital(base_profiles[3]))
                                     # tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                     #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                     #          helpText("* Values are taken from the two profiles.")),
                                     # tabPanel("Retrofit/New",  value=combo_profiles[2],
                                     #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                     #          helpText("* Values are taken from the two profiles."))
                        )
                        )),
                        icon=icon("gears")
                ), 
               tabPanel("Maintenance", value="Maintenance",
                        fluidRow(column(width=10, offset=1,
                                        tabsetPanel(id="prMaintenance", 
                                                    tabPanel("Robots", value=base_profiles[1],
                                                             changeVariablesMaintenace(base_profiles[1])),
                                                    tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                             changeVariablesMaintenace(base_profiles[2])),
                                                    tabPanel("New Parlors", value=base_profiles[3],
                                                             changeVariablesMaintenace(base_profiles[3]))
                                        )
                        )),
                        icon=icon("wrench")
               ), 
                tabPanel("Milk & Feed", value="Milk",
                         fluidRow(column(width=10, offset=1,
                        tabsetPanel(id="prMilk", 
                                     tabPanel("Robots", value=base_profiles[1],
                                              changeVariablesMilkfeed(base_profiles[1]),
                                              source(file.path("ui_files","ui_feed_calculation.R"), local=TRUE)$value
                                              ),
                                     tabPanel("Retrofit Parlors", value=base_profiles[2],
                                              changeVariablesMilkfeed(base_profiles[2])),
                                     tabPanel("New Parlors", value=base_profiles[3],
                                              changeVariablesMilkfeed(base_profiles[3]))
                        )
                          )),
                        icon=icon("truck")
                ),
                 tabPanel("Labor & Energy", value="Labor",
              fluidRow(column(width=10, offset=1,
                              tabsetPanel(id="prLabor", 
                                          tabPanel("Robots", value=base_profiles[1],
                                                   changeVariablesLaborenergy(base_profiles[1])),
                                          tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                   changeVariablesLaborenergy(base_profiles[2])),
                                          tabPanel("New Parlors", value=base_profiles[3],
                                                   changeVariablesLaborenergy(base_profiles[3]))
                              )
              )),
              icon=icon("male")
                ),
              tabPanel("Finance", value="Finance",
              fluidRow(column(width=10, offset=1,
                              tabsetPanel(id="prFinance", 
                                          tabPanel("Robots", value=base_profiles[1],
                                                   changeVariablesFinance(base_profiles[1])),
                                          tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                   changeVariablesFinance(base_profiles[2])),
                                          tabPanel("New Parlors", value=base_profiles[3],
                                                   changeVariablesFinance(base_profiles[3]))
                                          # tabPanel("Retrofit/Robots",   value=combo_profiles[1],
                                          #          changeVariablesCombo("Retrofit/Robots","Retrofit Parlors","Robots"),
                                          #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                          #          helpText("* The second set of Parlors are excluded from the calculation."),
                                          #          helpText("* The rest of the values are taken from the two profiles.")
                                          #          ),
                                          # tabPanel("Retrofit/New",    value=combo_profiles[2],
                                          #          changeVariablesCombo("Retrofit/New","Retrofit Parlors","New Parlors"),
                                          #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                          #          helpText("* The second set of Parlors are excluded from the calculation."),
                                          #          helpText("* The rest of the values are taken from the two profiles.")
                                          # )
                              )
              )),
               icon=icon("bank")
               ), 
                               widths=c(3,9)
  )    
) 

