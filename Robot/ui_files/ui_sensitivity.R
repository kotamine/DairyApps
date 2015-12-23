
uiVarX <- function(str, x) paste0(str, refProfile(x))

uiSensitivity <- function(x) { 
div( 
  fluidRow(column(1), 
    lapply(c(1:5), function(i) { column(2, htmlOutput(uiVarX(paste0("sensitivity_vars",i),x)))})
  ), 
  fluidRow(
    column(
      width=10, offset=1, 
      h4("Net Annual Impact:"),
      div(htmlOutput(paste0("sensitivity_impacts",refProfile(x))),align="center"),
      h4("Cash Flow:"),
      div(
        tabsetPanel(
          tabPanel("Operating Income",
                   htmlOutput(paste0("sensitivity_operating_income_chart",refProfile(x)))
                   ),
          tabPanel("Cashflow",
                   htmlOutput(paste0("sensitivity_cashflow_chart",refProfile(x)))
                   ),
          selected="Cashflow"),
        align="center"),
      h4("Table View:"),
      tabsetPanel(
        tabPanel("Before Tax",
                 DT::dataTableOutput(paste0("sensitivity_table_before_tax",refProfile(x)))
        ), 
        tabPanel("After Tax",
                 DT::dataTableOutput(paste0("sensitivity_table_after_tax",refProfile(x)))
        ),
        tabPanel("Operating Income",
                 DT::dataTableOutput(paste0("sensitivity_table_operating_income",refProfile(x)))
        ),
        tabPanel("Cash Flow",
                 DT::dataTableOutput(paste0("sensitivity_table_after_tax_cash_flow",refProfile(x)))
        ))
    )),br(),br() 
)
} 


