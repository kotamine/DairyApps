# ----- Dashboard -----
fluidRow(
  column(4,
         fluidRow(
           column(6,
                  uiOutput("IOFC"),
                  radioButtons("IOFC",NULL,choices=c("per cow","per cwt"), selected="per cwt")),
           column(6,
                  uiOutput("NAI"),
                  radioButtons("NAI",NULL,
                               choices=c("before tax",
                                         "after tax"),
                               selected="after tax")) 
         ),
         fluidRow(
           conditionalPanel("input.dash_option!='chart'",
                            column(6,
                                   div(uiOutput("breakeven"),align="center"),
                                   radioButtons("breakeven_option",NULL,
                                                choices=c("wage",
                                                          "wage inflation"),
                                                selected="wage")),
                            column(6,
                                   div( uiOutput("cashflow"), align="center")
                            )
           ),
           conditionalPanel("input.dash_option=='chart'",
                            tabsetPanel(
                              tabPanel("Cash Flow",
                                       htmlOutput("cashflow2")),
                              tabPanel("Breakeven Wage",
                                       htmlOutput("breakeven2"))
                            ) 
           ),
           br(), div( radioButtons("dash_option",NULL, inline=TRUE,
                                   choices=c("chart",
                                             "numbers"),
                                   selected="chart"), align="center")
         )),
  column(8,
         div(fluidRow(
           column(4,
                  plotOutput("plot1", height = 200),
                  uiOutput("milk_feed")),
           column(4,
                  plotOutput("plot2", height = 200),
                  uiOutput("labor_repair")),
           column(4,
                  plotOutput("plot3", height = 200),
                  uiOutput("captial_cost"))
         ),
         fluidRow(column(6,uiOutput("misc")),
                  column(6,uiOutput("inflation")))
         ), align="center")
)