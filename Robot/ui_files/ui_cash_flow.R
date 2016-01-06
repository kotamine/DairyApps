
# ---------- Cash Flow Analysis -----------
uiCashFlow <- function(x) { 
  div( 
    fluidRow(column(6, offset=3,
                    div(wellPanel(h4(paste(refProfileName(x))),align="center"))
    )),
    br(),
    fluidRow(
      column(8,offset=2,
             fluidRow(column(8, offset=1, helpText("Weighted average cost of capital after-tax (WACC)")),   
                      column(2, uiOutput(paste0("WACC",x)))
             ), 
             fluidRow(column(10, offset=1, a(id = paste0("CF_formula_show_1",x),"Show/hide formula"))),
             shinyjs::hidden(
               div(id = paste0("id_CF_formula_show_1",x),
                   hr(),
                   div(style="background-color:gray; color:white;",
                   fluidRow(column(6, h5("Item")),
                            column(2, h5("Housing")),
                            column(2, h5(refProfileName(x),1)),
                            column(2, div(id=paste0(x,2,10),h5(refProfileName(x),2)))
                   )
                   ),
                   fluidRow(column(6, helpText("Investment")),
                            column(2, uiOutput(paste0("CF_cost_housing",x))),
                            column(2, uiOutput(paste0("CF_cost_milking1",x))),
                            column(2, div(id=paste0(x,2,11),uiOutput(paste0("CF_cost_milking2",x))))
                   ),
                   fluidRow(column(6, helpText("Loan")),
                            column(2, uiOutput(paste0("CF_loan_housing",x))),
                            column(2, uiOutput(paste0("CF_loan_milking1",x))),
                            column(2, div(id=paste0(x,2,12),uiOutput(paste0("CF_loan_milking2",x))))
                   ),
                   fluidRow(column(6, helpText("Interest rate for the loan")),
                            column(2, uiOutput(paste0("CF_r_housing",x))),
                            column(2, uiOutput(paste0("CF_r_milking1",x))),
                            column(2, div(id=paste0(x,2,13),uiOutput(paste0("CF_r_milking2",x))))
                   ),
                   fluidRow(column(6, helpText("Downpayment")),
                            column(2, uiOutput(paste0("CF_down_housing",x))),
                            column(2, uiOutput(paste0("CF_down_milking1",x))),
                            column(2, div(id=paste0(x,2,14),uiOutput(paste0("CF_down_milking2",x))))
                   ),
                   fluidRow(column(6, helpText("Hurdle rate for the downpayment")),
                            column(2, uiOutput(paste0("CF_hr_housing",x))),
                            column(2, uiOutput(paste0("CF_hr_milking1",x))),
                            column(2, div(id=paste0(x,2,15),uiOutput(paste0("CF_hr_milking2",x))))
                   ),
                   uiOutput(paste0("CF_WACC_formula",x)),
                   hr()
               )),
             fluidRow(column(8, offset=1, helpText("Net present value (NPV)")),   
                      column(2, uiOutput(paste0("NPV",x)))
             ), 
             fluidRow(column(8,  offset=1,helpText("Net annual impact (after-tax)")),   
                      column(2, uiOutput(paste0("ANPV",x)))
             ),
             #            fluidRow(column(6,  offset=1, helpText("Internal rate of return (IRR)")),   
             #                     column(2, div(uiOutput("IRR"), align="right")),
             #                     column(2, helpText("percent"))
             #            ), 
             #            fluidRow(column(6,  offset=1,helpText("Modified internal rate of return (MIRR)")),   
             #                     column(2, uiOutput("MIRR")),
             #                     column(2, helpText("percent"))
             #            ),
             fluidRow(column(8,  offset=1,helpText("Return on Investment (ROI)")),   
                      column(2, uiOutput(paste0("ROI",x)))
             ),
             fluidRow(column(10, offset=1, a(id = paste0("CF_formula_show_2",x),"Show/hide formula"))),
             shinyjs::hidden(
               div(id = paste0("id_CF_formula_show_2",x),
                   hr(), 
                   uiOutput(paste0("CF_NPV_formula",x)), 
                   hr()
               ))
      )),
    br(),
    fluidRow(
      column(width=10,offset=1,
             htmlOutput(paste0("cashflow_chart",x)), br(), br(),
             tabsetPanel(
               tabPanel("Cash Flow",
                        DT::dataTableOutput(paste0("table_cash_flow",x))
               ),
               tabPanel("Debt Calculation",
                        DT::dataTableOutput(paste0("table_debt",x))
               ),
               tabPanel("Depreciation",
                        DT::dataTableOutput(paste0("table_depreciation",x))
               )
             )
      )),
    br(),
    div(downloadButton(paste0("download_table_cash_flow",x)),align="center"),
    br(),br()
  )
}  














