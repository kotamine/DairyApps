library(shiny)
library(shinyBS)
library(shinyjs)
suppressPackageStartupMessages(library(dplyr))

#source("helpers.R")

shinyUI(
  navbarPage(
    "Robotic Milking System",
    # ---------- Introduction  -----------
    tabPanel("Introduction",
             fluidRow(column(width=1),
                      column(width=10,
                             h4("Why robots?"),
                             p("..."),
                             h4("Where to start?"),
                             p("..."),
                             h4("How will this tool help me?"),
                             p("..."),
                             helpText("* Do we want to put some picture here?")
                      ))
    ),
    # ---------- Data Entry -----------
    tabPanel("Data Entry",
             navlistPanel("Data and Assumptions",
                          tabPanel("Farm Finance", 
                                    fluidRow(
                                      column(
                                        width=1),
                                      column(
                                        width=10,
                                        fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                 column(width=3, h5(strong("User Data"), align="center")), 
                                                 column(width=3, h5(strong("Unit"), align="center"))
                                        ),
                                        
                                        fluidRow(column(width=6, helpText("Current herd size (milking & dry)")),
                                                 column(width=3, numericInput("herd_size",NULL,value=120,min=30,step=10)),
                                                 column(width=3, helpText("animals", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Anticipated increase in milking herd with robots")),
                                                 column(width=3, numericInput("herd_increase",NULL,value=0,step=10)),
                                                 column(width=3, helpText("animals", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Herd size with robots")),
                                                 column(width=3, uiOutput("herd_size2")),
                                                 column(width=3, helpText("animals", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Number of robots")),
                                                 column(width=3, numericInput("n_robot",NULL,value=2,min=0,step=1)),
                                                 column(width=3, helpText("units", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Estimated cost per robot")),
                                                 column(width=3, numericInput("cost_robot",NULL,value=180000,min=50000,step=10000)),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Total investment for the robots alone")),
                                                 column(width=3, uiOutput("robot_invest")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Related housing changes needed per cow")),
                                                 column(width=3, numericInput("cost_housing_cow",NULL,value=9500,min=0,step=500)),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Related housing changes needed")),
                                                 column(width=3,   uiOutput("cost_housing")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Total investment for the robots and housing")),
                                                 column(width=3,   uiOutput("total_investment")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Toal investment per cow")),
                                                 column(width=3,   uiOutput("total_investment_cow")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Interest rate required on the overall investment")),
                                                 column(width=3,  numericInput("interest",NULL,value=3.0,min=0.0,step=0.1)),
                                                 column(width=3, helpText("percent", align="center"))
                                        ), br(), br() 
                                      )),
                                    icon=icon("dollar")),
                          tabPanel("Maintenance", 
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Estimated annual change in milking system repair")),
                                                column(width=3, numericInput("repair",NULL,value=7000,min=0,step=500)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Robots: years of useful life")),
                                                column(width=3, numericInput("robot_years",NULL,value=15,step=1)),
                                                column(width=3, helpText("years", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Related housing: useful life, multiple of robot life")),
                                                column(width=3, numericInput("n_robot_life",NULL, value=2,min=0,step=1)),
                                                column(width=3, helpText("times robot life", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Related housing: years of useful life")),
                                                column(width=3, uiOutput("housing_years")),
                                                column(width=3, helpText("years", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Value of the robots after useful life")),
                                                column(width=3, numericInput("salvage_robot",NULL,value=45000,min=0,step=1000)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Increased insurance value of robot & housing vs current")),
                                                column(width=3,   uiOutput("increased_insurance")),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Insurance rate per $1000 value")),
                                                column(width=3, numericInput("insurance_rate",NULL,value=0.5,min=0,step=0.1)),
                                                column(width=3, helpText("percent", align="center"))
                                       ))),
                                   icon=icon("wrench")), 
                          tabPanel("Labor Savings",
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Current hours of milking & chore labor")),
                                                column(width=3, numericInput("hours_milking",NULL,value=17,min=0,step=1)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated savings in milking & chore labor")),
                                                column(width=3, numericInput("hr_sv_milking",NULL,value=10.82,step=.2)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated hours of milking & chore labor")),
                                                column(width=3, uiOutput("anticipated_hours_milking")),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Current hours of heat detection")),
                                                column(width=3, numericInput("hr_heat_detection",NULL,value=0.65,min=0,step=.05)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated hours of heat detection")),
                                                column(width=3, numericInput("anticipated_hours_heat",NULL,value=0.25,min=0,step=0.05)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Labor rate for milking and heat detection")),
                                                column(width=3, numericInput("labor_rate",NULL,value=15.00,min=0,step=0.25)),
                                                column(width=3, helpText("dollars per hour", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Increased hours of records management")),
                                                column(width=3, numericInput("increase_rc_mgt",NULL,value=0.6,step=0.1)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Reduced hours of labor management")),
                                                column(width=3, numericInput("decrease_lab_mgt",NULL,value=0.6,min=0,step=0.1)),
                                                column(width=3, helpText("hours per day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Labor rate for records and labor management")),
                                                column(width=3, numericInput("labor_rate_rc_mgt",NULL,value=18.00,min=0,step=0.25)),
                                                column(width=3, helpText("dollars per hour", align="center"))
                                       ))),
                                   icon=icon("male")),
                          tabPanel("Milk Outputs", 
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Mailbox milk price")),
                                                column(width=3, numericInput("price_milk",NULL,value=17.50,min=0,step=0.25)),
                                                column(width=3, helpText("dollars per cwt", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Milk per cow per day, past year")),
                                                column(width=3, numericInput("milk_cow_day",NULL,value=75,min=0,step=5)),
                                                column(width=3, helpText("lbs/cow/day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Projected change in milk production")),
                                                column(width=3, numericInput("milk_change",NULL,value=10,step=2)),
                                                column(width=3, helpText("lbs/cow/day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("SCC premium per 1,000 SCC change")),
                                                column(width=3, numericInput("scc_premium",NULL,value=0.003,min=0,step=.001)),
                                                column(width=3, helpText("SCC per ml", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Current annual bulk tank average SCC")),
                                                column(width=3, numericInput("scc_average",NULL,value=240000,min=0,step=10000)),
                                                column(width=3, helpText("SCC per ml", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Estimated percent change in SCC")),
                                                column(width=3, numericInput("scc_change",NULL,value=-5,step=0.25)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Reproduction and herd health value of software")),
                                                column(width=3, numericInput("software",NULL,value=35,min=0, step=1)),
                                                column(width=3, helpText("dollars per cow/year", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Milk lbs/robot/day")),
                                                column(width=3, uiOutput("milk_lb_robot_day")),
                                                column(width=3, helpText("lbs/robot/day", align="center"))
                                       ))),
                                   icon=icon("bell-o")),
                          tabPanel("Feed", 
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                        fluidRow(column(width=6, helpText("Projected change in dry matter intake (DMI) per day")),
                                                      column(width=3, uiOutput("DMI_change")),
                                                      column(width=3, helpText("lbs DM/day", align="center"))
                                             ),
                                       fluidRow(column(width=6, helpText("Cost per lb of TMR dry matter")),
                                                column(width=3, numericInput("cost_DM",NULL,value=0.115,min=0,step=0.005)),
                                                column(width=3, helpText("dollars per lb DM", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Pellets fed in robot booth")),
                                                column(width=3, numericInput("pellets",NULL,value=11,min=0,step=0.5)),
                                                column(width=3, helpText("lb/day", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Extra cost for pellets fed in robot booth")),
                                                column(width=3, numericInput("cost_pellets",NULL,value=20,min=0,step=1)),
                                                column(width=3, helpText("dollars per ton", align="center"))
                                       ), br(),
                                       # checkboxInput("customDMI","Show DMI calculations",value=FALSE),
                                       # conditionalPanel("input.customDMI",
                                       a(id = "customDMI","Show/hide calculations of projected DMI change"),
                                       shinyjs::hidden(
                                         div(id = "DMI_inputs",
                                             fluidRow(column(width=6, h5(strong("Item"), align="center")),
                                                      column(width=3, h5(strong("Value"), align="center")), 
                                                      column(width=3, h5(strong("Coefficient"), align="center"))
                                             ),
                                             fluidRow(column(width=6, helpText("Milk per cow per day")),
                                                      column(width=3, uiOutput("rep_milk_cow_day")),
                                                      column(width=3, numericInput("milk_cow_coeff",NULL,value=0.4,min=0,step=0.05))
                                             ),
                                             fluidRow(column(width=6, helpText("Milk fat content (%)")),
                                                      column(width=3, numericInput("milk_fat",NULL,value=3.65,min=0,step=0.1)),
                                                      column(width=3,numericInput("milk_fat_coeff",NULL,value=15,min=0,step=0.05))
                                             ),
                                             fluidRow(column(width=6, helpText("Milk/cow/day adjusted to 4% fat ")),
                                                      column(width=3, uiOutput("adj_milk_cow_day")),
                                                      column(width=3,numericInput("adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.01))
                                             ),
                                             fluidRow(column(width=6, helpText("Milking herd avg body weight (lb)")),
                                                      column(width=2, numericInput("body_weight",NULL,value=1500,min=1000,step=50)),
                                                      column(width=2,numericInput("body_weight_coeff1",NULL,value=0.0968,min=0,step=0.001)),
                                                      column(width=2,numericInput("body_weight_coeff2",NULL,value=0.75,min=0,step=0.05))
                                             ),
                                             fluidRow(column(width=6, helpText("Lactation weeks")),
                                                      column(width=2, numericInput("lcatation_week",NULL,value=24,min=0,step=1)),
                                                      column(width=2,numericInput("lactation_coeff1",NULL,value=-0.192,step=0.01)),
                                                      column(width=2,numericInput("lactation_coeff2",NULL,value=3.67,min=0,step=0.05))
                                             ),
                                             fluidRow(column(width=6, helpText("Stage of lactation adjustment")),
                                                      column(width=3, uiOutput("stage_lactation"))
                                                      
                                             ),
                                             fluidRow(column(width=6, helpText("Current DMI per day")),
                                                      column(width=3, uiOutput("DMI_day"))
                                             ), br(),
                                             fluidRow(column(width=6, helpText("Projected change in milk production (lbs/cow/day)")),
                                                      column(width=3, uiOutput("rep_milk_change"))
                                             ),
                                             fluidRow(column(width=6, helpText("Projected DMI per day with robots")),
                                                      column(width=3, uiOutput("DMI_projected"))
                                             ),
                                             fluidRow(column(width=6, helpText("Projected change in DMI per day")),
                                                      column(width=3, uiOutput("DMI_change_copy"))
                                             ), br(), br()
                                         ))
                                     )
                                   ), 
                                   icon=icon("truck")),
                          tabPanel("Replacement", 
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, offset=, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Current culling percentage/year")),
                                                column(width=3, numericInput("culling_rate",NULL,value=30.0,min=0,step=0.1)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Current cow death loss percentage/year")),
                                                column(width=3,  numericInput("death_rate",NULL,value=6.5,min=0,step=0.1)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Cost of replacement heifer")),
                                                column(width=3,  numericInput("cost_heifer",NULL,value=1600,min=0,step=100)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Cull price per cow")),
                                                column(width=3,  numericInput("cull_price",NULL,value=750,min=0,step=50)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ), 
                                       fluidRow(column(width=6, helpText("Anticipated change in annual turnover rate")),
                                                column(width=3,  numericInput("change_turnover",NULL,value=-1.0,step=0.1)),
                                                column(width=3, helpText("percent", align="center"))
                                       ))),
                                   icon=icon("eyedropper")),
                          tabPanel("Utilties",
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, offset=, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated change in electricity cost")),
                                                column(width=3, numericInput("change_electricity",NULL,value=8.25,step=0.25)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated change in water cost")),
                                                column(width=3, numericInput("change_water",NULL,value=-3.00,step=0.25)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Anticipated change in chemical cost")),
                                                column(width=3, numericInput("change_chemical",NULL,value=1.50,step=0.25)),
                                                column(width=3, helpText("dollars", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Additional labor with herd expansion")),
                                                column(width=3,  numericInput("additional_labor",NULL,value=450,step=50)),
                                                column(width=3, helpText("dollars/cow/year", align="center"))
                                       ), 
                                       fluidRow(column(width=6, helpText("Other expense with herd expansion")),
                                                column(width=3,  numericInput("additional_cost",NULL,value=200,step=50)),
                                                column(width=3, helpText("dollars/cow/year", align="center"))
                                       ))), 
                                   icon=icon("lightbulb-o")),
                          tabPanel("Inflations", 
                                   fluidRow(
                                     column(
                                       width=1),
                                     column(
                                       width=10,
                                       fluidRow(column(width=6, offset=, h5(strong("Item"), align="center")),
                                                column(width=3, h5(strong("User Data"), align="center")), 
                                                column(width=3, h5(strong("Unit"), align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Robot, parlor & housing purchase prices")),
                                                column(width=3, numericInput("inflation_robot",NULL,value=1.5,step=0.25)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Expected robot salvage value at 15 years old")),
                                                column(width=3, numericInput("inflation_salvage",NULL,value=1.5,step=0.25)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Margin milk over feed & operation per cow with robots")),
                                                column(width=3, numericInput("inflation_margin",NULL,value=1.5,step=0.25)),
                                                column(width=3, helpText("percent", align="center"))
                                       ),
                                       fluidRow(column(width=6, helpText("Milking & chore labor rate per hour")),
                                                column(width=3, numericInput("inflation_robot",NULL,value=1.5,step=0.25)),
                                                column(width=3, helpText("percent", align="center"))
                                       ))),
                                   icon=icon("money"))         
             ),
             conditionalPanel("input.budget>0",
                              hr(),
                              fluidRow(
                                column(4,
                                       fluidRow(
                                         column(6,
                                                h5("Income Over Feed+ Cost ($/cow/year)"),
                                                uiOutput("IOFC")),
                                         column(6,
                                                h5("Net Annual Impact"),
                                                uiOutput("NAI"))
                                       )),
                                column(4,
                                       helpText("bar chart positives")),
                                column(4,
                                       helpText("bar chart negatives"))
                              ))
    ),
    # ---------- Partial Budget Analysis -----------
    tabPanel("Economic Analysis",
             conditionalPanel("input.budget==0",
                              div(bsButton("budget","Calculate",disabled = TRUE, icon = icon("ban")),align="center"),
                              span(helpText("Please review all tabs in Data Entry."),align="center")
                              ),
             fluidRow(
               conditionalPanel("input.budget>0",
                                column(8,offset=2, h4("Partial Budget Analysis (before-tax)",align="center")),
                              fluidRow(
                                column(6,
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
                                         fluidRow(column(width=8, offset=1, 
                                                         h5("Total positve impacts")),
                                                  column(width=3, uiOutput("positive_total"))              
                                         )
                                       )),
                                column(6,
                                       wellPanel(
                                         h5(strong("Negative Impacts:")),
                                         h5("Increased Expenses:"),
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
                                         fluidRow(column(width=8, offset=1, 
                                                         helpText("Capital recovery cost of robots (dep. & int.)")),
                                                  column(width=3, uiOutput("inc_exp_capital_recovery"))                          
                                         ),
                                         hr(), 
                                         fluidRow(column(width=8, offset=1, 
                                                         h5("Total increased expenses")),
                                                  column(width=3, uiOutput("inc_exp_total"))                          
                                         ), br(),
                                         fluidRow(column(width=8, offset=1, 
                                                         h5("Total negative impacts")),
                                                  column(width=3, uiOutput("negative_total"))                        
                                         )
                                       ), br(), br()
                                )),
                              fluidRow(
                                column(8,offset=2,
                                       fluidRow(
                                                column(width=3,offset=9, span(helpText("Break-even wage"),
                                                                              align="right"))
                                       ), 
                                       fluidRow(column(width=7, offset=0, 
                                                       h5("Net annual financial impact w/o housing")),
                                                column(width=3, uiOutput("impact_without_housing")),    
                                                column(width=2,uiOutput("be_wage_without_housing"))
                                       ), 
                                       fluidRow(column(width=6, offset=1, 
                                                       helpText("Capital recovery cost of housing (dep. & int.)")),
                                                column(width=3, uiOutput("capital_recovery_housing"))                      
                                       ),
                                       fluidRow(column(width=6, offset=1, 
                                                       helpText("Total capital recovery cost of robots & housing")),
                                                column(width=3, uiOutput("capital_recovery_total"))                         
                                       ),
                                       fluidRow(column(width=7, offset=0, 
                                                       h5("Net annual financial impact with housing")),
                                                column(width=3, uiOutput("impact_with_housing")),
                                                column(width=2,uiOutput("be_wage_with_housing"))
                                       ), 
                                       br(),
                                       h5("Robot's salvage value at the end of its useful life:"),
                                       fluidRow(column(width=6, offset=1, 
                                                       helpText("Estimated value at end $90,000")),
                                                column(width=3, span(helpText("Annualized PV"), align="right"))               
                                                ),
                                                fluidRow(column(width=6, offset=1, 
                                                                helpText("Estimated value at end, present value (PV) at 3%/year, $57,768")),
                                                         column(width=3, uiOutput("robot_end_PV"))                       
                                                ),
                                                fluidRow(column(width=7, offset=0, 
                                                                h5("Net annual impact with robot's salvage value")),
                                                         column(width=3, uiOutput("impact_with_robot_salvage")),                                                                          column(width=2,uiOutput("be_wage_with_salvage"))
                                                ),br(), 
                                                fluidRow(column(width=7, offset=0, 
                                                                h5("Net annual impact with inflation, as annualized net present value (NPV)")),
                                                         column(width=3, uiOutput("impact_with_inflation"))                          
                                                ),
                                                
                                                br(),br()
                                       ))
                              )
             )),
    # ---------- Additional Analyses -----------
             navbarMenu("More",
                        tabPanel("Sensitivity"),
                        tabPanel("Scenarios"),
                        tabPanel("Cash Flow")),
    # ---------- About -----------
             tabPanel("About",
                      fluidRow(column(width=1),
                               column(width=10,
                                      h4("Credits"),
                                      p("..."),
                                      h4("Contacts"),
                                      p("..."),
                                      h4("More resources"),
                                      p("...")
                               ))
             ),
             useShinyjs()
    ))
  
  
  
  
  