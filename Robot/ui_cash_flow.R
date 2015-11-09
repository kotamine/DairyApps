
# ---------- Cash Flow Analysis -----------
div(
fluidRow(
  column(8,offset=2,
         h4("Cash Flow Analysis (after-tax)",align="center"),
br(), 
# div(bsButton("calculate_cash_flow","Calculate",disabled = FALSE),align="center"),
# tags$hr(), 
# br(),   
fluidRow(column(5, helpText("Net present value (NPV)")),   
         column(2, uiOutput("NPV")),
         column(2, helpText("dollars"))
), 
fluidRow(column(5, helpText("Annualized NPV in nominal terms (ANPV)")),   
         column(2, uiOutput("ANPV")),
         column(2, helpText("dollars"))
), 
fluidRow(column(5, helpText("Annualized NPV in real terms (rANPV)")),   
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
)
  )),
fluidRow(
  column(width=10,offset=1, 
         tabsetPanel(
           tabPanel("Cash Flow",
                    DT::dataTableOutput("table_cash_flow")
           ), 
           tabPanel("Debt Calculation",
                    DT::dataTableOutput("table_debt")
           ), 
           tabPanel("Depreciation",
                    DT::dataTableOutput("table_depreciation")
           )
         ) 
  ))
)



 









