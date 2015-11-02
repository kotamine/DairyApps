
# ---------- Cash Flow Analysis -----------

fluidRow(
  column(8,offset=2,
         h4("Cash Flow Analysis (after-tax)",align="center"),
#   wellPanel(
#     fluidRow(column(4,  helpText("Variable")),
#              column(2,  helpText("Housing")),
#              column(2,  helpText("Robot 1")),
#              conditionaPanel("input.n_robot_life>=2", column(2,  helpText("Robot 2"))),
#              conditionaPanel("input.n_robot_life>=3", column(2,  helpText("Robot 3")))
#     ), 
#     fluidRow(column(4,  helpText("Year of investment")),
#              column(2,  helpText("1")),
#              column(2,  helpText("1")),
#              conditionaPanel("input.n_robot_life>=2", column(2,  uiOutput("yr_robot2"))),
#              conditionaPanel("input.n_robot_life>=3", column(2,  uiOutput("yr_robot3")))
#     ), 
#     fluidRow(column(4,  helpText("Investment amount ($)")),
#              column(2,  uiOutput("loan_housing")),
#              column(2,  uiOutput("loan_robot1")),
#              conditionaPanel("input.n_robot_life>=2", column(2,  uiOutput("loan_robot2"))),
#              conditionaPanel("input.n_robot_life>=3", column(2,  uiOutput("loan_robot3")))
#     ), 
#     fluidRow(column(4,  helpText("Down payment ($)")),
#              column(2,  numericInout("down_housing",value=0, min=0,step=5000)),
#              column(2,  numericInput("down_robot1",value=100000, min=0,step=5000)),
#              conditionaPanel("input.n_robot_life>=2", 
#                              column(2,  numericInput("down_robot2",value=100000, min=0, step=5000))),
#              conditionaPanel("input.n_robot_life>=3", 
#                              column(2,  numericInput("down_robot3",value=100000, min=0, step=5000)))
#     ), 
#     fluidRow(column(4,  helpText("Loan amount ($)")),
#              column(2,  uiOutput("loan_housing")),
#              column(2,  uiOutput("loan_robot1")),
#              conditionaPanel("input.n_robot_life>=2", column(2,  uiOutput("loan_robot2"))),
#              conditionaPanel("input.n_robot_life>=3", column(2,  uiOutput("loan_robot3")))
#     ),
#     fluidRow(column(4,  helpText("Interest rate (%)")),
#              column(2,  unmericInput("r_housing",value=5, min=0, step=.25)),
#              column(2,  numericInput("r_robot1",value=5, min=0, step=.25)),
#              conditionaPanel("input.n_robot_life>=2", 
#                              column(2,  numericInput("r_robot2",value=5, min=0, step=.25))),
#              conditionaPanel("input.n_robot_life>=3", 
#                              column(2,  numericInput("r_robot3",value=5, min=0, step=.25)))
#     ), 
#     fluidRow(column(4,  helpText("Loan period (years)")),
#              column(2,  unmericInput("n_yr_housing",value=24, min=0, step=1)),
#              column(2,  numericInput("n_yr_robot1",value=12, min=0, step=1)),
#              conditionaPanel("input.n_robot_life>=2", 
#                              column(2,  numericInput("n_yr_robot2",value=12, min=0, step=1))),
#              conditionaPanel("input.n_robot_life>=3", 
#                              column(2,  numericInput("n_yr_robot3",value=12, min=0, step=1)))
#     ),
#     fluidRow(column(4,  helpText("Salvage value ($)")),
#              column(2,  unmericInput("salvage_housing",value=0, min=0, step=5000)),
#              column(2,  uiOutput("copy_robot_salvage1")),
#              conditionaPanel("input.n_robot_life>=2", 
#                              uiOutput("copy_robot_salvage2")),
#              conditionaPanel("input.n_robot_life>=3", 
#                              column(2,  uiOutput("copy_robot_salvage3")))
#     )
#   ),
#   radioButton("dep_method","Depreciation method",
#               choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2")),
#   fluidRow(column(5, helpText("Marginal (federal + state) income tax rate (%)")),   
#            column(2, unmericInput("tax_rate",value=40, min=0, step=2))
#   ), 
#   fluidRow(column(5, helpText("Hurdle rate (%)")),   
#            column(2, unmericInput("hurdle_rate",value=3, min=0, step=.25))
#   ),
br(), 
div(bsButton("calculate_cash_flow","Calculate",disabled = FALSE, icon = icon("ban")),align="center"),
tags$hr(), 
br(),   
fluidRow(column(5, helpText("Net present value (NPV)")),   
         column(2, uiOutput("NPV")),
         column(2, helpText("dollars"))
), 
fluidRow(column(5, helpText("Annualized NPV in nominal terms (ANPV)")),   
         column(2, uiOutput("ANPV")),
         column(2, helpText("dollars"))
), 
fluidRow(column(5, helpText("Annualized NPV in real terms (ANPV)")),   
         column(2, uiOutput("ANPVr")),
         column(2, helpText("dollars"))
), 
fluidRow(column(5, helpText("Internal rate of return (IRR)")),   
         column(2, uiOutput("IRR")),
         column(2, helpText("percent"))
), 
fluidRow(column(5, helpText("Modified internal rate of return (MIRR)")),   
         column(2, uiOutput("MIRR")),
         column(2, helpText("percent"))
), 
fluidRow(column(5, helpText("Weighted average cost of capital (WACC)")),   
         column(2, uiOutput("WACC")),
         column(2, helpText("percent"))
)))


 









