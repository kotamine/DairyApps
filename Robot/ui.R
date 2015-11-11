library(shiny)
library(shinyBS)
library(markdown)
library(ggplot2)
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(XLConnect))

shinyUI(
   navbarPage(
    "Robotic Milking System",
    # ---------- Introduction  -----------
    tabPanel("Introduction",
             fluidRow(column(width=1),
                      column(width=10,
                             includeMarkdown(file.path("text","introduction.md"))
#                              div(class="well", style="background-color: #616D7E; color:white;",
#                                  h4("Why robots?"),
#                                  p("..."))
                      ))
    ),
    # ---------- Data Entry -----------
    tabPanel("Data Entry", id="data_entry",
             # Need to add "$value" for including source in UI: 
             # otherwise "TRUE" will show up at the end of file
             source("ui_data_entry_tabs.R", local=TRUE)$value,    
             
             # ----- Dashboard -----
             conditionalPanel("input.budget>0",
                              ## Basic Annual Impact Representation 
                              fluidRow(
                                column(4,
                                       fluidRow(
                                         column(6,
                                                uiOutput("IOFC"),
                                                radioButtons("IOFC",NULL,choices=c("per cow","per cwt"))),
                                         column(6,
                                                uiOutput("NAI"),
                                                radioButtons("NAI",NULL,
                                                             choices=c("before tax",
                                                                       "after tax"),
                                                             selected="before tax")) 
                                       ),
                                       fluidRow(
                                         column(6,
                                                div(uiOutput("cashflow")),align="center"),
                                         column(6,
                                                div(uiOutput("breakeven"),align="center"),
                                                radioButtons("breakeven_option",NULL,
                                                             choices=c("wage",
                                                                       "wage inflation"),
                                                             selected="wage"))
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
                              ), 
#                               ## Cash Flow Based Representation 
#                               conditionalPanel("input.cash_flow_on=='ON'",
#                                                fluidRow(
#                                                  column(4,
#                                                         fluidRow(
#                                                           column(6,
#                                                                  uiOutput("cash_IOFC"),
#                                                                  radioButtons("cash_IOFC",NULL,choices=c("per cow","per cwt"))),
#                                                           column(6,
#                                                                  uiOutput("cash_NAI"),
#                                                                  radioButtons("cash_NAI",NULL,
#                                                                               choices=c("w/o salvage",
#                                                                                         "w/ salvage"),
#                                                                               selected="w/ salvage")) 
#                                                         )),
#                                                  column(8,
#                                                         div(align="center", fluidRow(
#                                                           column(4,
#                                                                  plotOutput("cash_plot1", height = 200),
#                                                                  uiOutput("cash_milk_feed")),
#                                                           column(4,
#                                                                  plotOutput("cash_plot2", height = 200),
#                                                                  uiOutput("cash_labor_repair")),
#                                                           column(4,
#                                                                  plotOutput("cash_plot3", height = 200),
#                                                                  uiOutput("cash_captial_cost"))
#                                                         ),
#                                                         uiOutput("cash_misc")))
#                                                  
#                                                )), 
                              # ---------- Sensitivity Analysis -----------   
                              conditionalPanel('input.robust=="Sensitivity"', 
                                               tags$hr(), 
                                               fluidRow(column(3,h4("Sensitivity Analysis")),
                                                        column(2,bsButton("sensitivity_calculate","Calculate", style="primary")),
                                                        column(5,bsAlert("c_input_change"))),
                                               a(id = "sensitivity_show","Show/hide sensitivity control"),
                                               shinyjs::hidden(
                                                 div(id = "sensitivity_control",
                                              fluidRow(column(2,offset=5, h5("% Change")),
                                                       column(5,bsAlert("c_toggle"))),
                                               fluidRow(column(5,
                                                               selectInput("c_choice",NULL, width="100%",
                                                                           c("Estimated cost per robot"="c1",
                                                                             "Related housing changes needed per cow"="c2",
                                                                             "Estimated annual change in milking system repair"="c3",
                                                                             "Robots: years of useful life"="c4",
                                                                             "Value of the robots after useful life"="c5",
                                                                             "Anticipated savings in milking & chore labor"="c6",
                                                                             "Projected change in milk production"="c7"
                                                                           ))),
                                                        column(2,
                                                               numericInput("c_val",NULL, value=20, step=10)
                                                        ),
                                                        column(3,
                                                               uiOutput("c_text")
                                                        ) 
                                              )
                              ))
             ),
             # ---------- Scenarios Analysis -----------         
             conditionalPanel('input.robust=="Scenarios"', 
                              tags$hr(), 
                              fluidRow(column(3,h4("Scenario Analysis")),
                                       column(2,bsButton("scenario_calculate","Calculate", style="primary")),
                                       column(5,bsAlert("s_input_change"))),
                              a(id = "scenario_show","Show/hide scenario control"),
                              shinyjs::hidden(
                                div(id = "scenario_control",
                                    fluidRow(column(5,
                                                    selectInput("s_choice","Scenario", width="100%",
                                                                choices=c("Increased investment"="s1",
                                                                  "Use less pellets"="s2",
                                                                  "New barn ($120k/stall)"="s3"
                                                                ))),
                                             column(5,bsAlert("s_toggle"))), 
                                    fluidRow(
                                      column(5,offset=1,
                                             h5("Variable")),
                                      column(2,
                                             h5("% Change")),
                                      column(3,
                                             fluidRow(column(4,h5("Base")),
                                                      column(4,h5("New")),
                                                      column(4,h5("Unit"))))),
                                    conditionalPanel("input.s_choice=='s1'",
                                    fluidRow(
                                            column(5,offset=1,
                                                   helpText("Estimated cost per robot")),
                                            column(2, 
                                                   numericInput("s_cost_robot", NULL, value=25, step=10)),
                                            column(3,
                                                   uiOutput("s_txt_cost_robot"))
                                            )
                                      ),
                                                     fluidRow(
                                      column(5,offset=1,
                                             helpText("Related housing changes needed per cow")),
                                      column(2, 
                                             numericInput("s_cost_housing_cow", NULL, value=-95, step=10)),
                                      column(3,
                                             uiOutput("s_txt_cost_housing_cow"))
                                      )
                                    ,
                                    conditionalPanel("input.s_choice=='s2' | input.s_choice=='s3'",
                                                     fluidRow(
                                      column(5,offset=1,
                                             helpText("Projected change in milk production (%)")),
                                      column(2, 
                                             numericInput("s_milk_change", NULL, value=0, step=10)),
                                      column(3,
                                             uiOutput("s_txt_milk_change"))
                                                     )
                                    ),
                                    conditionalPanel("input.s_choice=='s3' ",
                                    fluidRow(
                                      column(5,offset=1,
                                             helpText("Estimated percent change in SCC")),
                                      column(2, 
                                             numericInput("s_scc_change", NULL, value=0, step=10)),
                                      column(3,
                                             uiOutput("s_txt_scc_change"))
                                    )
                                    ),
                                    conditionalPanel("input.s_choice=='s2'",
                                                     fluidRow(
                                      column(5,offset=1,
                                             helpText("Pellets fed in robot booth")),
                                      column(2, 
                                             numericInput("s_pellets", NULL, value=0, step=10)),
                                      column(3,
                                             uiOutput("s_txt_pellets"))
                                                     )
                                    )
                                ))
                              ),
             # ---------- Dashboard for Sensitivity and Scenarios ---------- 
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
               column(8,
                      div(align="center", fluidRow(
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
                      uiOutput("c_misc")))
             ))),
             DT::dataTableOutput("table_robust")
#              # ---------- Cash Flow Analysis -----------         
#              conditionalPanel('input.robust=="Cash Flow"', 
#                               helpText("Additional Controls and Displays for Cash Flow Analysis")
#              )
            ),
             # --------- Data Table ---------
             fluidRow(column(2,offset=3,
                             downloadButton("c_download","Download")),
                      column(3, 
             # fileInput() is passessed from the server
             uiOutput('resettableInput'),
             bsAlert("upload_alert")),   
             column(1, actionButton("remove", "Remove Data"))),
             #
             tabsetPanel(
               tabPanel(
             tableOutput('sheet1')),
             tabPanel(
             tableOutput('sheet2'))
             ),
             br(), br()
    )),
    # ---------- Partial Budget Analysis -----------
    tabPanel("Partial Budget",
             conditionalPanel("input.budget==0",
                              div(bsButton("budget","Calculate",disabled = TRUE, icon = icon("ban")),
                                  helpText("Please review all tabs in Data Entry."),align="center")
             ),
             conditionalPanel("input.budget>0",
                source("ui_partial_budget.R", local=TRUE)$value
             )
             ),
tabPanel("Cash Flow",
         conditionalPanel("input.budget==0",
                          div(helpText("Please review all tabs in Data Entry."),align="center")
         ),
         conditionalPanel("input.budget>0",
#                           div(
#                             radioButtons("cash_flow_on","Cash Flow Based Representation", choices=c("OFF","ON")), 
#                             align="center"), 
                          conditionalPanel("input.budget>0",
                          source("ui_cash_flow.R", local=TRUE)$value
                          )
         )
),
    # ---------- Additional Analyses -----------
    navbarMenu("More",
               tabPanel("Robustness Check Tools",
                        fluidRow(
                          column(10,offset=1, 
                                        fluidRow(column(6, offset=3,
                                                        radioButtons("robust", "Robustness analysis options", 
                                                                     choices=c("Off","Sensitivity","Scenarios")))),
                                        conditionalPanel('input.robust=="Sensitivity"', 
                                                         helpText("Explanation about Sensitivity Analysis")),
                                        conditionalPanel('input.robust=="Scenarios"', 
                                                         helpText("Explanation about Scenario Analysis")))
                        )),

               tabPanel("Robots vs Parlors")
    ),
    # ---------- About -----------
    tabPanel("About",
             fluidRow(column(width=1),
                      column(width=10,
                             includeMarkdown(file.path("text","about.md"))
#                              h4("Credits"),
#                              p("..."),
#                              h4("Contacts"),
#                              p("..."),
#                              h4("More resources"),
#                              p("...")
                      ))
    ),
# HTML("<script>$('#goData').click(function() {
# 						tabs = $('.tabbable .nav.nav-tabs li')
# 						tabs.each(function() {
# 							$(this).removeClass('active')
# 						})
# 						$(tabs[1]).addClass('active')
# 						
# 						tabsContents = $('.tabbable .tab-content .tab-pane')
# 						tabsContents.each(function() {
# 							$(this).removeClass('active')
# 						})
# 						$(tabsContents[1]).addClass('active')
# 
# 						$('#about').trigger('change').trigger('shown');
# 						 
# 					})</script>
# 			"),
    useShinyjs(),
collapsible = TRUE
  ))




