

uiSensitivity <- function(x) {   
div(    
  fluidRow(column(10, offset=1, h4("Sensitivity to:"))),   
  fluidRow(column(1), 
    lapply(c(1:5), function(i) { column(2, htmlOutput(paste0("sensitivity_vars",x,i)))})
  ), 
  shinyjs::hidden(
    div(id = paste0("sensitivity_details",x),
        hr(),
        fluidRow(column(10, offset=1, h4("Detials:"))),
        htmlOutput(paste0("sensitivity_dashboard",x)), 
        div(a(id = paste0("sensitivity_details_close",x),"Close Details"),align="center"),
        hr() 
    )),
  br(),
  fluidRow(
    column(
      width=10, offset=1, 
      h4("Net Annual Impact:"))),
      div(htmlOutput(paste0("sensitivity_impacts",x)),align="center"),
  fluidRow(column(width=10, offset=1, h4("Cash Flow:"))),
       div(
        tabsetPanel(
          tabPanel("Operating Income",
                   htmlOutput(paste0("sensitivity_operating_income_chart",x))
                   ),
          tabPanel("Cashflow",
                   htmlOutput(paste0("sensitivity_cashflow_chart",x))
                   ),
          selected="Cashflow"),
         align="center"),
      fluidRow(column(width=10, offset=1, 
      h4("Table View:"),
      tabsetPanel(
        tabPanel("Before Tax",
                 DT::dataTableOutput(paste0("sensitivity_table_before_tax",x))
        ), 
        tabPanel("After Tax",
                 DT::dataTableOutput(paste0("sensitivity_table_after_tax",x))
        ),
        tabPanel("Operating Income",
                 DT::dataTableOutput(paste0("sensitivity_table_operating_income",x))
        ),
        tabPanel("Cash Flow",
                 DT::dataTableOutput(paste0("sensitivity_table_after_tax_cash_flow",x)) 
        ), selected="After Tax")
    )),br(),br() 
)
} 


