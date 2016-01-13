div(      
  fluidRow(
    column(
      width=10, offset=1, 
      h4("Net Annual Impact:"),
      div(htmlOutput("profile_impacts"),align="center"),
      h4("Cash Flow:"),
        tabsetPanel(
          tabPanel("Operating Income",
                   htmlOutput("profile_operating_income_chart")),
          tabPanel("Cashflow",
                   htmlOutput("profile_cashflow_chart")),
          selected="Cashflow"),
      h4("Table View:"),
      tabsetPanel(
        tabPanel("Before Tax",
                 DT::dataTableOutput("summary_table_before_tax")
        ), 
        tabPanel("After Tax",
                 DT::dataTableOutput("summary_table_after_tax")
        ),
        tabPanel("Operating Income",
                 DT::dataTableOutput("summary_table_operating_income")
        ),
        tabPanel("Cash Flow",
                 DT::dataTableOutput("summary_table_after_tax_cash_flow")
        ),selected="After Tax")
    )),br(),
  div(downloadButton("download_table_summary"),align="center"),
  br()
)

