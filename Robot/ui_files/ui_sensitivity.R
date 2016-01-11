

uiSensitivity <- function(x) {   
  div(    
    fluidRow(column(6, offset=3,
                    div(wellPanel(h4(paste(refProfileName(x))),align="center"))
    )),
    fluidRow(column(10, offset=1, h4("Sensitivity with respect to:"))),   
    fluidRow(column(1), 
             lapply(c(1:5), function(i) { column(2, htmlOutput(paste0("sensitivity_vars",x,i)))})
    ), 
    fluidRow(column(1), 
             lapply(c(6:10), function(i) { column(2, htmlOutput(paste0("sensitivity_vars",x,i)))})
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
    div(htmlOutput(paste0("sensitivity_impacts1",x)),
        htmlOutput(paste0("sensitivity_impacts2",x)), align="center"),
    fluidRow(column(width=10, offset=1, h4("Cash Flow:"))),
    div(
      tabsetPanel(
        tabPanel("Operating Income",
                 htmlOutput(paste0("sensitivity_operating_income_chart1",x)),
                 htmlOutput(paste0("sensitivity_operating_income_chart2",x))
        ),
        tabPanel("Cashflow",
                 htmlOutput(paste0("sensitivity_cashflow_chart1",x)),
                 htmlOutput(paste0("sensitivity_cashflow_chart2",x))
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
    )),br(),
    div(downloadButton(paste0("download_table_sensitivity",x)),align="center"),
    br(),
    fluidRow(column(width=10, offset=1,   
                    h4("Plot over a Range of Percentage Change:"),
                    fluidRow(
                      #                     column(6, numericInput(paste0("sensitivity_range",x),"Select a Range",
                      #                                            min=50, max=100, step=50, value=100)), 
                      column(3, offset=2, bsButton(paste0("sensitivity_plot_button",x),"Plot", style="primary")),
                      column(3, radioButtons(paste0("sensitivity_plot_NAI",x), NULL, 
                                             choices=c("before tax", "after tax"),
                                             selected=c("after tax"), inline=TRUE))),
                    shinyjs::hidden(div(id=paste0("sensitivity_plot_message",x),
                                        h4("Calculating... Please wait"), align="center"))
                    
                    # tags$style(type='text/css', paste0("#sensitivity_plot_button",x,
                    #                                    "  { width:100%; margin-top: 25px;}"))
    )), 
    htmlOutput(paste0("sensitivity_plot1",x)),
    htmlOutput(paste0("sensitivity_plot2",x)),
    br(), br(),br()
  ) 
}   


