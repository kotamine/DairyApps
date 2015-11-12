
# ---------- Cash Flow Analysis -----------
div(
fluidRow(
  column(8,offset=2,
         h4("Cash Flow Analysis",align="center"),
br(), 
# div(bsButton("calculate_cash_flow","Calculate",disabled = FALSE),align="center"),
# tags$hr(), 
# br(),  
fluidRow(column(6, offset=1, helpText("Weighted average cost of capital (WACC)")),   
         column(2, uiOutput("WACC")),
         column(2, helpText("percent"))
), 
fluidRow(column(6, offset=1, helpText("Net present value (NPV)")),   
         column(2, uiOutput("NPV")),
         column(2, helpText("dollars"))
), 
fluidRow(column(6,  offset=1,helpText("Net annual impact (after-tax)")),   
         column(2, uiOutput("ANPV")),
         column(2, helpText("dollars"))
), 
# fluidRow(column(6,  offset=1, helpText("Annualized NPV in real terms (rANPV)")),   
#          column(2, uiOutput("ANPVr")),
#          column(2, helpText("dollars"))
# ), 
fluidRow(column(6,  offset=1, helpText("Internal rate of return (IRR)")),   
         column(2, div(uiOutput("IRR"), align="right")),
         column(2, helpText("percent"))
), 
fluidRow(column(6,  offset=1,helpText("Modified internal rate of return (MIRR)")),   
         column(2, uiOutput("MIRR")),
         column(2, helpText("percent"))
)
  )),
br(), 
fluidRow(
  column(width=10,offset=1, 
         div(htmlOutput("cashflow_chart"),align="center"),
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



 









