# ---------- Dashboard for Sensitivity and Scenarios ---------- 
conditionalPanel('input.robust=="Sensitivity" | input.robust=="Scenarios"', 
  shinyjs::hidden(
  div(id = "dashboard_robust",
      fluidRow(
        column(4,
               fluidRow(
                 column(6,
                        uiOutput("c_IOFC")),
                 column(6,
                        uiOutput("c_NAI")) 
               ),
               fluidRow(
                 conditionalPanel("input.dash_option!='chart'", 
                                  column(6,
                                         div(uiOutput("c_breakeven"),align="center")),
                                  column(6,
                                         div( uiOutput("c_cashflow"), align="center")
                                  )
                 ),
                 conditionalPanel("input.dash_option=='chart'", 
                                  tabsetPanel(
                                    tabPanel("Breakeven Wage",
                                             htmlOutput("c_breakeven2")),
                                    tabPanel("Cash Flow",
                                             htmlOutput("c_cashflow2"))
                                  ) 
                 )
               )),
        column(8,
               div(fluidRow(
                 column(4,
                        plotOutput("c_plot1", height = 200),
                        uiOutput("c_milk_feed")),
                 column(4,
                        plotOutput("c_plot2", height = 200),
                        uiOutput("c_labor_repair")),
                 column(4,
                        plotOutput("c_plot3", height = 200),
                        uiOutput("c_captial_cost"))
               ),
               fluidRow(column(6,uiOutput("c_misc")),
                        column(6,uiOutput("c_inflation"))
               ), align="center")
        )), br(),
        conditionalPanel('input.robust=="Sensitivity"', 
                        div(htmlOutput("plot_robust"), align="center")
        )
      )), br(),
  conditionalPanel('(input.robust=="Sensitivity" & input.sensitivity_calculate>0) |
                   (input.robust=="Scenarios" & input.scenario_calculate>0)',
  tabsetPanel(
    tabPanel("Change in Variables",
             DT::dataTableOutput("table_robust_variables")),
    tabPanel("Before Tax",
             DT::dataTableOutput("table_robust_before_tax")),
    tabPanel("Before Tax: Difference",
             DT::dataTableOutput("table_robust_before_tax_diff")),
    tabPanel("After tax",
             DT::dataTableOutput("table_robust_after_tax")),
    tabPanel("After Tax: Difference",
             DT::dataTableOutput("table_robust_after_tax_diff"))
  ))
)