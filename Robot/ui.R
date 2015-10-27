library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(markdown)

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
    tabPanel("Data Entry",
             source("ui_data_entry_tabs.R", local=TRUE), 
             # ----- Dashboard -----
             conditionalPanel("input.budget>0",
                              fluidRow(
                                column(4,
                                       fluidRow(
                                         column(6,
                                                uiOutput("IOFC"),
                                                radioButtons("IOFC",NULL,choices=c("per cow","per cwt"))),
                                         column(6,
                                                uiOutput("NAI"),
                                                radioButtons("NAI",NULL,
                                                             choices=c("w/o housing","w/ housing",
                                                                       "w/ housing + robot salvage"),
                                                             selected="w/ housing + robot salvage")) 
                                       )),
                                column(8,
                                       div(align="center", fluidRow(
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
                                       uiOutput("misc")))
                                
                              ), 
                              # ---------- Sensitivity Analysis -----------   
                              conditionalPanel('input.robust=="Sensitivity"', 
                                               tags$hr(), 
                                               h4("Sensitivity Analysis"),
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
                                                               numericInput("c_val","% Change:", value=20, step=10)
                                                        ),
                                                        column(3,
                                                               uiOutput("c_text")
                                                        ),  
                                                        column(2,
                                                               actionButton("c_store","Add to Table")
                                                        )
                                               ),
                                               fluidRow(
                                                 column(4,
                                                        fluidRow(
                                                          column(6,
                                                                 uiOutput("c_IOFC")),
                                                          column(6,
                                                                 uiOutput("c_NAI")) 
                                                        )),
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
                                               ),
                                               tabsetPanel(
                                                 tabPanel("Sensitivity",DT::dataTableOutput("table_sensitivity")),
                                                 tabPanel("Variables",DT::dataTableOutput("table_input"))
                                               ) 
                              )
             ),
             # ---------- Scenarios Analysis -----------         
             conditionalPanel('input.robust=="Scenarios"', 
                              helpText("Additional Controls and Displays Scenario Analysis")
                              ),
             # ---------- Cash Flow Analysis -----------         
             conditionalPanel('input.robust=="Cash Flow"', 
                              helpText("Additional Controls and Displays for Cash Flow Analysis")
             ),
             # --------- Data Table ---------
             fluidRow(column(1,offset=9,
                             actionButton("c_clear","Clear")),
                      column(2, 
                             downloadButton("c_download","Download"))
             ), 
             fileInput('user_data', 'Upload data',
                          accept=c('.xlsx', "application/vnd.ms-excel",
                                   '.csv')),
             tabsetPanel(
               tabPanel(
             tableOutput('sheet1')),
             tabPanel(
             tableOutput('sheet2'))
             ),
             br(), br()
    ),
    # ---------- Partial Budget Analysis -----------
    tabPanel("Economic Analysis",
             conditionalPanel("input.budget==0",
                              div(bsButton("budget","Calculate",disabled = TRUE, icon = icon("ban")),
                                  helpText("Please review all tabs in Data Entry."),align="center")
             ),
             conditionalPanel("input.budget>0",
                source("ui_partial_budget.R", local=TRUE)
             )
             ),
    # ---------- Additional Analyses -----------
    navbarMenu("More",
               tabPanel("Robustness Check Tools",
                        fluidRow(column(10, offset=1, 
                                        fluidRow(column(6, offset=3,
                                                        radioButtons("robust", "Robustness analysis options", 
                                                                     choices=c("Off","Sensitivity","Scenarios", "Cash Flow")))),
                                        conditionalPanel('input.robust=="Sensitivity"', 
                                                         helpText("Explanation about Sensitivity Analysis")),
                                        conditionalPanel('input.robust=="Scenarios"', 
                                                         helpText("Explanation about Scenario Analysis")),
                                        conditionalPanel('input.robust=="Cash Flow"', 
                                                         helpText("Explanation about Cash Flow Analysis"))
                        ))),
               tabPanel("Investment with New Parlor")
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
    useShinyjs()
  ))




