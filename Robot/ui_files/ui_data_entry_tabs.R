
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
                                              changeVariablesCapital("Robots")),
                                     tabPanel("Retrofit Parlors", value=base_profiles[2],
                                              changeVariablesCapital("Retrofit Parlors")),
                                     tabPanel("New Parlors", value=base_profiles[3],
                                              changeVariablesCapital("New Parlors"))
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
                                                             changeVariablesMaintenace("Robots")),
                                                    tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                             changeVariablesMaintenace("Retrofit Parlors")),
                                                    tabPanel("New Parlors", value=base_profiles[3],
                                                             changeVariablesMaintenace("New Parlors"))
                                                    # tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                                    #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                                    #          helpText("* Values are taken from the two profiles.")),
                                                    # tabPanel("Retrofit/New",  value=combo_profiles[2],
                                                    #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                                    #          helpText("* Values are taken from the two profiles."))
                                        )
                        )),
                        icon=icon("wrench")
               ), 
                tabPanel("Milk & Feed", value="Milk",
                         fluidRow(column(width=10, offset=1,
                        tabsetPanel(id="prMilk", 
                                     tabPanel("Robots", value=base_profiles[1],
                                              changeVariablesMilkfeed("Robots"),
                                              source(file.path("ui_files","ui_feed_calculation.R"), local=TRUE)$value
                                              ),
                                     tabPanel("Retrofit Parlors", value=base_profiles[2],
                                              changeVariablesMilkfeed("Retrofit Parlors")),
                                     tabPanel("New Parlors", value=base_profiles[3],
                                              changeVariablesMilkfeed("New Parlors"))
                                    # tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                    #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                    #          helpText("* Values are taken from the two profiles.")),
                                    # tabPanel("Retrofit/New",  value=combo_profiles[2],
                                    #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                    #          helpText("* Values are taken from the two profiles."))
                        )
                          )),
                        icon=icon("truck")
                ),
                 tabPanel("Labor & Energy", value="Labor",
              fluidRow(column(width=10, offset=1,
                              tabsetPanel(id="prLabor", 
                                          tabPanel("Robots", value=base_profiles[1],
                                                   changeVariablesLaborenergy("Robots")),
                                          tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                   changeVariablesLaborenergy("Retrofit Parlors")),
                                          tabPanel("New Parlors", value=base_profiles[3],
                                                   changeVariablesLaborenergy("New Parlors"))
                                          # tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                          #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                          #          helpText("* Values are taken from the two profiles.")),
                                          # tabPanel("Retrofit/New",  value=combo_profiles[2],
                                          #          helpText("* This assumes first Retrofit Parlors and then New Parlors."),
                                          #          helpText("* Values are taken from the two profiles."))
                              )
              )),
              icon=icon("male")
                ),
              tabPanel("Finance", value="Finance",
              fluidRow(column(width=10, offset=1,
                              tabsetPanel(id="prFinance", 
                                          tabPanel("Robots", value=base_profiles[1],
                                                   changeVariablesFinance("Robots")),
                                          tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                   changeVariablesFinance("Retrofit Parlors")),
                                          tabPanel("New Parlors", value=base_profiles[3],
                                                   changeVariablesFinance("New Parlors"))
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

#                
#                
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=8, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=8, helpText("Current herd size (milking & dry)")),
#                                      column(width=3, numericInput("herd_size",NULL,value=120,min=30,step=10)),
#                                      column(width=3, helpText("animals", align="center"))
#                             ),
#                             
#                             )
#                             fluidRow(column(width=6, 
#                                             conditionalPanel('input.robot_parlor=="OFF"',
#                                                              helpText("Anticipated increase in milking herd with robots")),
#                                             conditionalPanel('input.robot_parlor=="ON"',
#                                                              helpText("Anticipated increase in milking herd with robots/parlors"))),
#                                      column(width=3, numericInput("herd_increase",NULL,value=0,step=10)),
#                                      column(width=3, helpText("animals", align="center"))
#                             ), 
#                             conditionalPanel("input.herd_increase!=0", 
#                                              fluidRow(column(width=6, helpText("Additional labor expense with herd expansion")),
#                                                       column(width=3,  numericInput("additional_labor",NULL,value=450,step=50,min=0)),
#                                                       column(width=3, helpText("dollars/additional cow/year", align="center"))
#                                              ), 
#                                              fluidRow(column(width=6, helpText("Other expense with herd expansion")),
#                                                       column(width=3,  numericInput("additional_cost",NULL,value=200,step=50,min=0)),
#                                                       column(width=3, helpText("dollars/additional cow/year", align="center"))
#                                              )
#                             ),
#                             fluidRow(column(width=6, 
#                                             conditionalPanel('input.robot_parlor=="OFF"',helpText("Herd size with robots")),
#                                             conditionalPanel('input.robot_parlor=="ON"',helpText("Herd size with robots/parlors"))),
#                                      column(width=3, uiOutput("herd_size2")),
#                                      column(width=3, helpText("animals", align="center"))
#                             ), 
#                             conditionalPanel( 'input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
#                                               fluidRow(column(width=6, helpText("Number of robots")),
#                                                        column(width=3, numericInput("n_robot",NULL,value=2,min=0,step=1)),
#                                                        column(width=3, helpText("units", align="center"))
#                                               ),
#                                               fluidRow(column(width=6, helpText("Estimated cost per robot")),
#                                                        column(width=3, numericInput("cost_robot",NULL,value=180000,min=50000,step=10000)),
#                                                        column(width=3, helpText("dollars", align="center"))
#                                               ),
#                                               fluidRow(column(width=6, helpText("Total investment for the robots alone")),
#                                                        column(width=3, uiOutput("cost_milking")),
#                                                        column(width=3, helpText("dollars", align="center"))
#                                               )),
#                             conditionalPanel('input.robot_parlor=="ON" & input.profile_choice!="Robots"',
#                                              fluidRow(column(width=6, helpText("Investment in Parlors")),
#                                                       column(width=3, numericInput("cost_parlors",NULL,value=0,min=0,step=10000)),
#                                                       column(width=3, helpText("dollars", align="center"))
#                                              )
#                             ),
#                             fluidRow(column(width=6, helpText("Related housing changes needed per cow")),
#                                      column(width=3, numericInput("cost_housing_cow",NULL,value=9500,min=0,step=500)),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Related housing changes needed")),
#                                      column(width=3,   uiOutput("cost_housing")),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, 
#                                             conditionalPanel('input.robot_parlor=="OFF"',helpText("Total investment for the robots and housing")),
#                                             conditionalPanel('input.robot_parlor=="ON"',helpText("Total investment for the robots/parlors and housing"))),
#                                      column(width=3,   uiOutput("total_investment")),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Toal investment per cow")),
#                                      column(width=3,   uiOutput("total_investment_cow")),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             br(), br() 
#                           )),
#                         icon=icon("home")),
#                tabPanel("Maintenance", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Estimated annual change in milking system repair")),
#                                      column(width=3, numericInput("repair",NULL,value=7000,min=0,step=500)),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             conditionalPanel( 'input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
#                                               fluidRow(column(width=6, helpText("Robots: years of useful life")),
#                                                        column(width=3, numericInput("robot_years",NULL,value=15, min=0, step=1)),
#                                                        column(width=3, helpText("years", align="center"))
#                                               ),
#                                               fluidRow(column(width=6, helpText("Related housing: useful life, multiple of robot life")),
#                                                        column(width=3, numericInput("n_robot_life",NULL, value=2,min=0,step=1, max=2)),
#                                                        column(width=3, helpText("times robot life", align="center"))
#                                               )),
#                             conditionalPanel( 'input.robot_parlor=="ON" & input.profile_choice!="Robots"',
#                                               fluidRow(column(width=6, helpText("Parlors: years of useful life")),
#                                                        column(width=3, numericInput("milking_years",NULL,value=30, min=0, step=1)),
#                                                        column(width=3, helpText("years", align="center"))
#                                               )),
#                             fluidRow(column(width=6, helpText("Related housing: years of useful life")),
#                                      column(width=3, uiOutput("housing_years")),
#                                      column(width=3, helpText("years", align="center"))
#                             ),
#                             #                           shinyjs::hidden( fluidRow(column(width=6, helpText("Planning horizon")),   
#                             #                                    column(width=3, numericInput("horizon",NULL,value=30, min=1, step=5)),
#                             #                                    column(width=3, helpText("years", align="center"))
#                             #                           )),
#                             fluidRow(column(width=6, 
#                                             conditionalPanel('input.robot_parlor=="OFF"',helpText("Value of the robots after useful life")),
#                                             conditionalPanel('input.robot_parlor=="ON"',helpText("Value of the robots/parlors after useful life"))),
#                                      column(width=3, numericInput("salvage_milking1",NULL,value=45000,min=0,step=1000)),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, 
#                                             conditionalPanel('input.robot_parlor=="OFF"',helpText("Increased insurance value of robot & housing vs current")),
#                                             conditionalPanel('input.robot_parlor=="ON"',helpText("Increased insurance value of robot/parlor & housing vs current"))),
#                                      column(width=3,   uiOutput("increased_insurance")),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Insurance rate per $1000 value")),
#                                      column(width=3, numericInput("insurance_rate",NULL,value=0.5,min=0,step=0.1)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ))),
#                         icon=icon("wrench")), 
#                tabPanel("Labor Savings",
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Current hours of milking & chore labor")),
#                                      column(width=3, numericInput("hours_milking",NULL,value=17,min=0,step=1)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Anticipated savings in milking & chore labor")),
#                                      column(width=3, numericInput("hr_sv_milking",NULL,value=10.82, min=0, step=.2)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Anticipated hours of milking & chore labor")),
#                                      column(width=3, uiOutput("anticipated_hours_milking")),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Current hours of heat detection")),
#                                      column(width=3, numericInput("hr_heat_detection",NULL,value=0.65,min=0,step=.05)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Anticipated hours of heat detection")),
#                                      column(width=3, numericInput("anticipated_hours_heat",NULL,value=0.25,min=0,step=0.05)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Labor rate for milking and heat detection")),
#                                      column(width=3, numericInput("labor_rate",NULL,value=15.00,min=0,step=0.25)),
#                                      column(width=3, helpText("dollars per hour", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Increased hours of records management")),
#                                      column(width=3, numericInput("increase_rc_mgt",NULL,value=0.6,step=0.1)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Reduced hours of labor management")),
#                                      column(width=3, numericInput("decrease_lab_mgt",NULL,value=0.6,min=0,step=0.1)),
#                                      column(width=3, helpText("hours per day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Labor rate for records and labor management")),
#                                      column(width=3, numericInput("labor_rate_rc_mgt",NULL,value=18.00,min=0,step=0.25)),
#                                      column(width=3, helpText("dollars per hour", align="center"))
#                             ))),
#                         icon=icon("male")),
#                tabPanel("Milk Outputs", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Mailbox milk price")),
#                                      column(width=3, numericInput("price_milk",NULL,value=17.50,min=0,step=0.25)),
#                                      column(width=3, helpText("dollars per cwt", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Milk per cow per day, past year")),
#                                      column(width=3, numericInput("milk_cow_day",NULL,value=75,min=0,step=5)),
#                                      column(width=3, helpText("lbs/cow/day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Projected change in milk production")),
#                                      column(width=3, numericInput("milk_change",NULL,value=10,step=2)),
#                                      column(width=3, helpText("lbs/cow/day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("SCC premium per 1,000 SCC")),
#                                      column(width=3, numericInput("scc_premium",NULL,value=0.003,min=0,step=.001)),
#                                      column(width=3, helpText("SCC per ml", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Current annual bulk tank average SCC")),
#                                      column(width=3, numericInput("scc_average",NULL,value=240000,min=0,step=10000)),
#                                      column(width=3, helpText("SCC per ml", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Estimated percent change in SCC")),
#                                      column(width=3, numericInput("scc_change",NULL,value=-5,step=0.25)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Reproduction and herd health value of software")),
#                                      column(width=3, numericInput("software",NULL,value=35,min=0, step=1)),
#                                      column(width=3, helpText("dollars per cow/year", align="center"))
#                             ),
#                             conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
#                                              fluidRow(column(width=6, helpText("Milk lbs/robot/day")),
#                                                       column(width=3, uiOutput("milk_lb_alt_day")),
#                                                       column(width=3, helpText("lbs/robot/day", align="center"))
#                                              )),
#                             conditionalPanel('input.robot_parlor=="ON" & input.profile_choice!="Robots"',
#                                              fluidRow(column(width=6, helpText("Projected milk lbs/day")),
#                                                       column(width=3, uiOutput("milk_alt_day")),
#                                                       column(width=3, helpText("lbs/day", align="center"))
#                                              ))
#                           )),
#                         icon=icon("bell-o")),
#                tabPanel("Feed", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Projected change in dry matter intake (DMI) per day")),
#                                      column(width=3, uiOutput("DMI_change")),
#                                      column(width=3, helpText("lbs DM/day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Cost per lb of TMR dry matter")),
#                                      column(width=3, numericInput("cost_DM",NULL,value=0.115,min=0,step=0.005)),
#                                      column(width=3, helpText("dollars per lb DM", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Pellets fed in robot booth")),
#                                      column(width=3, numericInput("pellets",NULL,value=11,min=0,step=1)),
#                                      column(width=3, helpText("lb/cow/day", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Extra cost for pellets fed in robot booth")),
#                                      column(width=3, numericInput("cost_pellets",NULL,value=20,min=0,step=2)),
#                                      column(width=3, helpText("dollars per ton", align="center"))
#                             ), br(),
#                             ## This is just an alternative way to show/hide a section 
#                             # checkboxInput("customDMI","Show calculations of projected DMI change",value=FALSE),
#                             # conditionalPanel("input.customDMI",
#                             a(id = "customDMI","Show/hide calculations of projected DMI change"),
#                             shinyjs::hidden(
#                               div(id = "DMI_inputs",
#                                   div(style="background-color: #616D7E; color:white;",
#                                       fluidRow(column(width=6, h5(strong("Item"), align="center")),
#                                                column(width=3, h5(strong("Value"), align="center")), 
#                                                column(width=3, h5(strong("Coefficient"), align="center"))
#                                       )), br(),
#                                   fluidRow(column(width=6, helpText("Milk per cow per day")),
#                                            column(width=3, uiOutput("rep_milk_cow_day")),
#                                            column(width=3, numericInput("milk_cow_coeff",NULL,value=0.4,min=0,step=0.1))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Milk fat content (%)")),
#                                            column(width=3, numericInput("milk_fat",NULL,value=3.65,min=0,step=0.2)),
#                                            column(width=3, numericInput("milk_fat_coeff",NULL,value=15,min=0,step=0.5))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Milk/cow/day adjusted to 4% fat ")),
#                                            column(width=3, uiOutput("adj_milk_cow_day")),
#                                            column(width=3,numericInput("adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.1))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Milking herd avg body weight (lb)")),
#                                            column(width=2, numericInput("body_weight",NULL,value=1500,min=1000,step=50)),
#                                            column(width=2,numericInput("body_weight_coeff1",NULL,value=0.0968,min=0,step=0.005)),
#                                            column(width=2,numericInput("body_weight_coeff2",NULL,value=0.75,min=0,step=0.05))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Lactation weeks")),
#                                            column(width=2, numericInput("lcatation_week",NULL,value=24,min=0,step=1)),
#                                            column(width=2,numericInput("lactation_coeff1",NULL,value=-0.192,min=0,step=0.01)),
#                                            column(width=2,numericInput("lactation_coeff2",NULL,value=3.67,min=0,step=0.05))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Stage of lactation adjustment")),
#                                            column(width=3, uiOutput("stage_lactation"))
#                                            
#                                   ),
#                                   fluidRow(column(width=6, helpText("Current DMI per day")),
#                                            column(width=3, uiOutput("DMI_day"))
#                                   ), br(),
#                                   fluidRow(column(width=6, helpText("Projected change in milk production (lbs/cow/day)")),
#                                            column(width=3, uiOutput("rep_milk_change"))
#                                   ),
#                                   fluidRow(column(width=6, 
#                                                   conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
#                                                                    helpText("Projected DMI per day with robots")),
#                                                   conditionalPanel('input.robot_parlor=="ON" & input.profile_choice!="Robots"',
#                                                                    helpText("Projected DMI per day with parlor investment"))),
#                                            column(width=3, uiOutput("DMI_projected"))
#                                   ),
#                                   fluidRow(column(width=6, helpText("Projected change in DMI per day")),
#                                            column(width=3, uiOutput("DMI_change_copy"))
#                                   ), 
#                                   fluidRow(column(width=3, offset=9,
#                                                   span(actionButton("coeff_reset","reset"),align="center"))
#                                   ),
#                                   br(), br()
#                               ))
#                           )
#                         ), 
#                         icon=icon("truck")),
#                tabPanel("Replacement", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Current culling percentage/year")),
#                                      column(width=3, numericInput("culling_rate",NULL,value=30.0,min=0,step=0.1)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Current cow death loss percentage/year")),
#                                      column(width=3,  numericInput("death_rate",NULL,value=6.5,min=0,step=0.1)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Cost of replacement heifer")),
#                                      column(width=3,  numericInput("cost_heifer",NULL,value=1600,min=0,step=100)),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Cull price per cow")),
#                                      column(width=3,  numericInput("cull_price",NULL,value=750,min=0,step=50)),
#                                      column(width=3, helpText("dollars", align="center"))
#                             ), 
#                             fluidRow(column(width=6, helpText("Anticipated change in annual turnover rate")),
#                                      column(width=3,  numericInput("change_turnover",NULL,value=-1.0,step=0.25)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ))),
#                         icon=icon("eyedropper")),
#                tabPanel("Utilties",
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Anticipated change in electricity cost")),
#                                      column(width=3, numericInput("change_electricity",NULL,value=8.25,step=0.25)),
#                                      column(width=3, helpText("dollars/cow/year", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Anticipated change in water cost")),
#                                      column(width=3, numericInput("change_water",NULL,value=-3.00,step=0.25)),
#                                      column(width=3, helpText("dollars/cow/year", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Anticipated change in chemical cost")),
#                                      column(width=3, numericInput("change_chemical",NULL,value=1.50,step=0.25)),
#                                      column(width=3, helpText("dollars/cow/year", align="center"))
#                             )
#                           )), 
#                         icon=icon("lightbulb-o")),
#                # "Cash Flow Factors" ,
#                tabPanel("Inflations & Interests", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, div(style="background-color: #616D7E; color:white;",
#                                           fluidRow(column(width=6, 
#                                                           h5(strong("Item"),align="center")),
#                                                    column(width=3, h5(strong("User Data"), 
#                                                                       align="center")),
#                                                    column(width=3,  h5(strong("Unit"), 
#                                                                        align="center"))
#                                           )), br(),
#                             fluidRow(column(width=6, helpText("Robot/parlor & related-housing prices")),
#                                      column(width=3, numericInput("inflation_robot",NULL,value=1.5,step=0.25)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             #                                                     fluidRow(column(width=6, helpText("Expected robot salvage value at 15 years old")),
#                             #                                                              column(width=3, numericInput("inflation_salvage",NULL,value=1.5,step=0.25)),
#                             #                                                              column(width=3, helpText("percent", align="center"))
#                             #                                                     ),
#                             fluidRow(column(width=6, helpText("Margin milk over feed & operation per cow with robots")),
#                                      column(width=3, numericInput("inflation_margin",NULL,value=0.2,step=0.25)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Milking & chore labor rate per hour")),
#                                      column(width=3, numericInput("inflation_labor",NULL,value=1.5,step=0.25)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(width=6, helpText("Interest rate required on the overall investment")),
#                                      column(width=3,  numericInput("interest",NULL,value=4.0,min=0.0,step=0.1)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(6, helpText("Hurdle rate")),   
#                                      column(3, numericInput("hurdle_rate",NULL,value=4, min=0, step=.1)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ),
#                             fluidRow(column(6, helpText("Marginal (federal + state) income tax rate")),   
#                                      column(3, numericInput("tax_rate",NULL,value=40, min=0, step=2)),
#                                      column(width=3, helpText("percent", align="center"))
#                             ), br(),
#                             radioButtons("dep_method","Depreciation accounting method:",
#                                          choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2"))
#                             
#                           )),
#                         icon=icon("money")
#                ),   
#                tabPanel("Financing", 
#                         fluidRow(
#                           column(
#                             width=1),
#                           column(
#                             width=10, 
#                             div(style="background-color: #616D7E; color:white;",
#                                 fluidRow(column(width=4,  h5(strong("Item"), align="center")),
#                                          column(width=2,  h5(strong("Housing"), align="center")),
#                                          column(width=2,  
#                                                 conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
#                                                                  h5(strong("Robot 1"), align="center")), 
#                                                 conditionalPanel('input.robot_parlor=="ON" & input.profile_choice=="Retrofit Parlors"',
#                                                                  h5(strong("Retrofit"), align="center")),
#                                                 conditionalPanel('input.robot_parlor=="ON" & input.profile_choice=="New Parlors"',
#                                                                  h5(strong("Parlor"), align="center"))),
#                                          column(width=2, 
#                                                 conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                  h5(strong("Robot 2"), align="center")))
#                                 )), br(), 
#                             fluidRow(column(width=4,  helpText("Year of investment")),
#                                      column(width=2,  helpText("0")),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        helpText("0"))),
#                                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                       uiOutput("yr_robot2"))) 
#                             ), 
#                             fluidRow(column(width=4,  helpText("Investment amount ($)")),
#                                      column(width=2,  uiOutput("copy_cost_housing")),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        uiOutput("copy_cost_milking1"))),
#                                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                       uiOutput("copy_cost_milking2")))
#                             ), 
#                             fluidRow(column(width=4,  helpText("Down payment ($)")),
#                                      column(width=2,  
#                                             numericInput("down_housing",NULL,value=100000, min=0,step=20000)),
#                                      column(width=2,  
#                                             conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                              numericInput("down_milking1",NULL,value=0, min=0,step=20000))),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                        numericInput("down_milking2",NULL,value=50000, min=0, step=20000)))
#                             ), 
#                             fluidRow(column(width=4,  helpText("Loan amount ($)")),
#                                      column(width=2,  uiOutput("loan_housing")),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        uiOutput("loan_milking1"))),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                        uiOutput("loan_milking2")))
#                             ),
#                             fluidRow(column(width=4,  helpText("Interest rate (%)")),
#                                      column(width=2,  uiOutput('copy_r_housing')),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        uiOutput('copy_r_milking1'))),
#                                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                       uiOutput('copy_r_milking2')))
#                             ),
#                             shinyjs::hidden( 
#                               fluidRow(column(width=4,  helpText("Interest rate (%)")),
#                                        column(width=2,  numericInput("r_housing",NULL,value=4, min=0, step=.25)),
#                                        column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                          numericInput("r_milking1",NULL,value=4, min=0, step=.25))),
#                                        column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                         numericInput("r_milking2",NULL,value=4, min=0, step=.25)))
#                               )) , 
#                             fluidRow(column(width=4,  helpText("Loan period (years)")),
#                                      column(width=2,  numericInput("n_yr_housing",NULL,value=24, min=0, step=1)),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        numericInput("n_yr_milking1",NULL,value=12, min=0, step=1))),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                        numericInput("n_yr_milking2",NULL,value=12, min=0, step=1)))
#                             ),
#                             fluidRow(column(width=4,  helpText("Salvage value ($)")),
#                                      column(width=2,  helpText("0")),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
#                                                                        uiOutput("copy_salvage_milking1"))),
#                                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
#                                                                                 & input.n_robot_life>=2",
#                                                                        uiOutput("copy_salvage_milking2")))
#                                      
#                             )
#                           )), icon=icon("bank")), 
#                widths=c(3,9)
#   )     
# )  


