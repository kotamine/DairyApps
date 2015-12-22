
# ---------- Cash Flow Analysis -----------
uiCashFlow <- function(x) {
  div(
    fluidRow(column(6, offset=3,
                    div(wellPanel(h4(paste(x)),align="center"))
    )),
    br(),
    fluidRow(
      column(8,offset=2,
             fluidRow(column(8, offset=1, helpText("Weighted average cost of capital after-tax (WACC)")),   
                      column(2, uiOutput(paste0("WACC",refProfile(x))))
                      ), 
             fluidRow(column(10, offset=1, a(id = "CF_formula_show_1","Show/hide formula"))),
                        shinyjs::hidden(
                        div(id = "id_CF_formula_show_1",
                      fluidRow(column(6, h5("Item")),
                               column(2, h5("Housing")),
                               column(2, h5("Robot 1")),
                               column(2, h5("Robot 2"))
                      ),
                       fluidRow(column(6, helpText("Investment")),
                                column(2, uiOutput(paste0("CF_cost_housing",refProfile(x)))),
                                column(2, uiOutput(paste0("CF_cost_milking1",refProfile(x)))),
                                column(2, uiOutput(paste0("CF_cost_milking2",refProfile(x))))
                       ),
                      fluidRow(column(6, helpText("Loan")),
                               column(2, uiOutput(paste0("CF_loan_housing",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_loan_milking1",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_loan_milking2",refProfile(x))))
                      ),
                      fluidRow(column(6, helpText("Interest rate for the loan")),
                               column(2, uiOutput(paste0("CF_r_housing",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_r_milking1",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_r_milking2",refProfile(x))))
                      ),
                      fluidRow(column(6, helpText("Downpayment")),
                               column(2, uiOutput(paste0("CF_down_housing",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_down_milking1",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_down_milking2",refProfile(x))))
                      ),
                      fluidRow(column(6, helpText("Hurdle rate for the downpayment")),
                               column(2, uiOutput(paste0("CF_hr_housing",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_hr_milking1",refProfile(x)))),
                               column(2, uiOutput(paste0("CF_hr_milking2",refProfile(x))))
                      ),
                      uiOutput(paste0("CF_WACC_formula",refProfile(x)))
                        )),
                      fluidRow(column(8, offset=1, helpText("Net present value (NPV)")),   
                               column(2, uiOutput(paste0("NPV",refProfile(x))))
                      ), 
                      fluidRow(column(8,  offset=1,helpText("Net annual impact (after-tax)")),   
                               column(2, uiOutput(paste0("ANPV",refProfile(x))))
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
                               column(2, uiOutput(paste0("ROI",refProfile(x))))
                      ),
             fluidRow(column(10, offset=1, a(id = "CF_formula_show_2","Show/hide formula"))),
             shinyjs::hidden(
                 div(id = "id_CF_formula_show_2",
                     uiOutput(paste0("CF_NPV_formula",refProfile(x))),
                     uiOutput(paste0("CF_annuity_formula",refProfile(x))),
                     uiOutput(paste0("CF_ROI",refProfile(x)))
             ))
      )),
    br(),
    fluidRow(
      column(width=10,offset=1,
             htmlOutput(paste0("cashflow_chart",refProfile(x))), br(),
             div(uiOutput(paste0("dl_button_cash_flow",refProfile(x))),align="center"),
             tabsetPanel(
               tabPanel("Cash Flow",
                        DT::dataTableOutput(paste0("table_cash_flow",refProfile(x)))
               ),
               tabPanel("Debt Calculation",
                        DT::dataTableOutput(paste0("table_debt",refProfile(x)))
               ),
               tabPanel("Depreciation",
                        DT::dataTableOutput(paste0("table_depreciation",refProfile(x)))
               )
             )
      )),
    br(),br()
  )
} 














