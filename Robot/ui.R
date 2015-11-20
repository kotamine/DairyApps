library(shiny)
library(shinyBS)
library(markdown)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))
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
             fluidRow(column(width=2),
                      column(width=8,
                             includeMarkdown(file.path("text","introduction.md"))
                      ))
    ),
    # ---------- Data Entry -----------
    tabPanel("Data Entry", id="data_entry",
             # Need to add "$value" for including source in UI: 
             # otherwise "TRUE" will show up at the end of file
             
              source("ui_data_entry_tabs.R", local=TRUE)$value,    
             
             conditionalPanel("input.budget>0",
                              # ---------- Dashboard ----------
                              source("ui_dashboard.R", local=TRUE)$value,    
                              
                              # ---------- Robustness ----------
                              source("ui_robustness.R", local=TRUE)$value,
                              
                              #---------- Dashboard for Sensitivity and Scenarios ---------- 
                              source("ui_dashboard_robustness.R", local=TRUE)$value
             ),
             # --------- Data Table ---------
             br(), br(),
             fluidRow(column(3,offset=2,
                             downloadButton("c_download","Download Data")),
                      column(3, 
             # fileInput() is passessed from the server
             uiOutput('resettableInput'),
             bsAlert("upload_alert")),   
             column(2, actionButton("remove", "Remove Data"))),
#              #
#              tabsetPanel(
#                tabPanel(
#              tableOutput('sheet1')),
#              tabPanel(
#              tableOutput('sheet2'))
#              ),
             br(), br()
    ),
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
        source("ui_cash_flow.R", local=TRUE)$value
        )
),
    # ---------- Additional Analyses -----------
    navbarMenu("More",
               tabPanel("Robustness Check Tools",
                        fluidRow(column(6, offset=3,
                                        radioButtons("robust", "Robustness analysis options", 
                                                                     choices=c("Off","Sensitivity","Scenarios")),
                                        helpText("To assess the robustness of your results, consider changes in key variables. 
                                                 Sensitivity Analysis uses a change in one variable at a time, whereas
                                                 Scenario Analysis uses changes in a set of related variables at a time.
                                                 In Data Entry tab, the results of Sensitivity or Scneario Analysis will 
                                                 appear below the baseline results in a parallel fashion."),
                                        conditionalPanel('input.robust=="Sensitivity"', br(), hr(),
                                                         includeMarkdown(file.path("text","sensitivity.md"))),
                                        conditionalPanel('input.robust=="Scenarios"', br(), hr(),
                                                         includeMarkdown(file.path("text","scenario.md")))
                        ))
                        ),

               tabPanel("Robots vs Parlors",
                        conditionalPanel("input.budget>0",
                                         source("ui_robot_parlor.R", local=TRUE)$value
                        ))
    ),
    # ---------- About -----------
    tabPanel("About",
             fluidRow(column(width=1),
                      column(width=10,
                             includeMarkdown(file.path("text","about.md"))
                      ))
    ),
    useShinyjs(),
collapsible = TRUE
  ))




