
tabPanel("Investment", 
         fluidRow(
           column(
             width=1),
           column(
             width=10, 
             div(style="background-color: #616D7E; color:white;",
                 fluidRow(column(4,  helpText("Variable")),
                          column(2,  helpText("Housing")),
                          column(2,  helpText("Robot 1")),
                          conditionalPanel("input.n_robot_life>=2", column(2,  helpText("Robot 2"))),
                          conditionalPanel("input.n_robot_life>=3", column(2,  helpText("Robot 3")))
                 )), 
             fluidRow(column(4,  helpText("Year of investment")),
                      column(2,  helpText("1")),
                      column(2,  helpText("1")),
                      conditionalPanel("input.n_robot_life>=2", column(2,  uiOutput("yr_robot2"))),
                      conditionalPanel("input.n_robot_life>=3", column(2,  uiOutput("yr_robot3")))
             ), 
             fluidRow(column(4,  helpText("Investment amount ($)")),
                      column(2,  uiOutput("loan_housing")),
                      column(2,  uiOutput("loan_robot1")),
                      conditionalPanel("input.n_robot_life>=2", column(2,  uiOutput("loan_robot2"))),
                      conditionalPanel("input.n_robot_life>=3", column(2,  uiOutput("loan_robot3")))
             ), 
             fluidRow(column(4,  helpText("Down payment ($)")),
                      column(2,  numericInput("down_housing",NULL,value=0, min=0,step=5000)),
                      column(2,  numericInput("down_robot1",NULL,value=100000, min=0,step=5000)),
                      conditionalPanel("input.n_robot_life>=2", 
                                       column(2,  numericInput("down_robot2",NULL,value=100000, min=0, step=5000))),
                      conditionalPanel("input.n_robot_life>=3", 
                                       column(2,  numericInput("down_robot3",NULL,value=100000, min=0, step=5000)))
             ), 
             fluidRow(column(4,  helpText("Loan amount ($)")),
                      column(2,  uiOutput("loan_housing")),
                      column(2,  uiOutput("loan_robot1")),
                      conditionalPanel("input.n_robot_life>=2", column(2,  uiOutput("loan_robot2"))),
                      conditionalPanel("input.n_robot_life>=3", column(2,  uiOutput("loan_robot3")))
             ),
             fluidRow(column(4,  helpText("Interest rate (%)")),
                      column(2,  numericInput("r_housing",NULL,value=5, min=0, step=.25)),
                      column(2,  numericInput("r_robot1",NULL,value=5, min=0, step=.25)),
                      conditionalPanel("input.n_robot_life>=2", 
                                       column(2,  numericInput("r_robot2",NULL,value=5, min=0, step=.25))),
                      conditionalPanel("input.n_robot_life>=3", 
                                       column(2,  numericInput("r_robot3",NULL,value=5, min=0, step=.25)))
             ), 
             fluidRow(column(4,  helpText("Loan period (years)")),
                      column(2,  numericInput("n_yr_housing",NULL,value=24, min=0, step=1)),
                      column(2,  numericInput("n_yr_robot1",NULL,value=12, min=0, step=1)),
                      conditionalPanel("input.n_robot_life>=2", 
                                       column(2,  numericInput("n_yr_robot2",NULL,value=12, min=0, step=1))),
                      conditionalPanel("input.n_robot_life>=3", 
                                       column(2,  numericInput("n_yr_robot3",NULL,value=12, min=0, step=1)))
             ),
             fluidRow(column(4,  helpText("Salvage value ($)")),
                      column(2,  numericInput("salvage_housing",NULL,value=0, min=0, step=5000)),
                      column(2,  uiOutput("copy_robot_salvage1")),
                      conditionalPanel("input.n_robot_life>=2", 
                                       uiOutput("copy_robot_salvage2")),
                      conditionalPanel("input.n_robot_life>=3", 
                                       column(2,  uiOutput("copy_robot_salvage3")))
             ))
         )),
tabPanel("Planning", 
         fluidRow(
           column(
             width=1),
           column(
             width=10,            
             div(style="background-color: #616D7E; color:white;",
                 fluidRow(column(width=6, 
                                 h5(strong("Item"),align="center")),
                          column(width=3, h5(strong("User Data"), 
                                             align="center")),
                          column(width=3,  h5(strong("Unit"), 
                                              align="center"))
                 )), br(),
             fluidRow(column(width=6, helpText("Planning horizon")),   
                      column(width=3, numericInput("horizon",NULL,value=30, min=1, step=5)),
                      column(width=3, helpText("years", align="center"))
             ),
             fluidRow(column(6, helpText("Hurdle rate")),   
                      column(3, numericInput("hurdle_rate",NULL,value=3, min=0, step=.25)),
                      column(width=3, helpText("percent", align="center"))
             ),
             fluidRow(column(5, helpText("Marginal (federal + state) income tax rate")),   
                      column(3, numericInput("tax_rate",NULL,value=40, min=0, step=2)),
                      column(width=3, helpText("percent", align="center"))
             ), 
             radioButtons("dep_method","Depreciation method",
                          choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2"))
           )
         )),
tabPanel("Inflations", 
         fluidRow(
           column(
             width=1),
           column(
             width=10, div(style="background-color: #616D7E; color:white;",
                           fluidRow(column(width=6, 
                                           h5(strong("Item"),align="center")),
                                    column(width=3, h5(strong("User Data"), 
                                                       align="center")),
                                    column(width=3,  h5(strong("Unit"), 
                                                        align="center"))
                           )), br(),
             fluidRow(column(width=6, helpText("Robot, parlor & related-housing prices")),
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
                      column(width=3, numericInput("inflation_labor",NULL,value=1.5,step=0.25)),
                      column(width=3, helpText("percent", align="center"))
             ),
             fluidRow(column(width=6, helpText("General inflation in economy")),
                      column(width=3, numericInput("inflation_general",NULL,value=1.5,step=0.25)),
                      column(width=3, helpText("percent", align="center"))
             )
           )),
         icon=icon("money"))



