library(shiny)
library(shinyBS)
library(rmarkdown)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(XLConnect))


source(file.path("ui_files","ui_functions.R"), local=TRUE)
source(file.path("ui_files", "ui_partial_budget.R"), local=TRUE)  # Contains functions
source(file.path("ui_files", "ui_cash_flow.R"), local=TRUE)  # Contains functions
source(file.path("ui_files", "ui_dashboard.R"), local=TRUE)  # Contains functions


base_profiles <- c("Robots","Retrofit","New")
combo_profiles <- c("RetrofitRobots","RetrofitNew")


shinyUI(  
  fluidPage(
    # list(tags$head(HTML("  "))),
    # tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/UMN.css"),
    # includeHTML("www/UMN_header.html"),
   #  list(tags$head(HTML(" "))),
    div(class="well", style="background-color:#7a0019; color:white;", 
        fluidRow(column(width=8, offset=1, h1("UM Extension Dairy")))),
    navbarPage(
      "Robotic Milking Systems", id = "Navbar",
      # ---------- Introduction  -----------
      tabPanel("Introduction",  value="Introduction",
               fluidRow(column(width=2),
                        column(width=8,
                               includeMarkdown(file.path("text","introduction.md")), 
                               br(),br()
                        ))
      ),
      # ---------- Data Entry -----------
      tabPanel("Data Entry", value="Data_Entry",
               # Need to add "$value" for including source in UI: 
               # otherwise "TRUE" will show up at the end of file

               source(file.path("ui_files","ui_data_entry_tabs.R"), local=TRUE)$value,
               
               tabsetPanel(id="dashboard",
                           tabPanel("Robots", value=base_profiles[1],
                                    uiDashboard("Robots")),
                           tabPanel("Retrofit Parlors", value=base_profiles[2],
                                    uiDashboard("Retrofit Parlors")),
                           tabPanel("New Parlors", value=base_profiles[3],
                                    uiDashboard("New Parlors")),
                           tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                    helpText("This assumes first Retrofit Parlors and then Robots."),
                                    helpText("Values are taken from the two profiles.")),
                           tabPanel("Retrofit/New",  value=combo_profiles[2],
                                    helpText("This assumes first Retrofit Parlors and then New Parlors."),
                                    helpText("Values are taken from the two profiles.")),
                           tabPanel("Summary",
                                    helpText("Summary"))
                           ),
               shinyjs::hidden(radioButtons("IOFC",NULL,choices=c("per cow","per cwt"), selected="per cwt")),
               shinyjs::hidden(radioButtons("NAI",NULL, choices=c("before tax", "after tax"), selected="after tax"))
       ), 
      # tabPanel("Test",
      #          actionButton("test1","test button"),
      #          uiOutput("test1_out")

# tabsetPanel("Investment",
#             tabPanel("Robot",
#                      investmentVariables1("Robot")),
#             tabPanel("Parlor",
#                      investmentVariables1("Parlor"))
# )

#                conditionalPanel("input.budget>0",
#                                 # ---------- Dashboard ----------
#                                 source(file.path("ui_files", "ui_dashboard.R"), local=TRUE)$value,    
# 
#                                 # ---------- Robustness ----------
#                                 source(file.path("ui_files", "ui_robustness.R"), local=TRUE)$value,    
#                                 
#                                 #---------- Dashboard for Sensitivity and Scenarios ---------- 
#                                 source(file.path("ui_files", "ui_dashboard_robustness.R"), local=TRUE)$value
#                                 
#                ),
#                # --------- Data Table ---------
#                br(), br(),
#                hr(),
#                fluidRow(column(2, offset=1,
#                                actionButton("defalut_data","Default Input-Data")),
#                         column(3,
#                                downloadButton("data_download","Download Input-Data")),
#                         column(3, 
#                                # fileInput() is passessed from the server
#                                uiOutput('resettableInput'),
#                                bsAlert("upload_alert")),   
#                         column(2, actionButton("remove", "Remove Input-Data"))),
#                br(), br()
      # ),
      # ---------- Partial Budget Analysis -----------
      tabPanel("Partial Budget", value = "Partial_Budget",
#                conditionalPanel("input.budget==0",
#                                 div(bsButton("budget","Calculate",disabled = TRUE, icon = icon("ban")),
#                                     helpText("Please review all tabs in Data Entry."),align="center")
#                ),
               # conditionalPanel("input.budget>0",
                                fluidRow(column(6, offset=3,
                                                  h4("Partial Budget Analysis",align="center")
                                                )),
                                tabsetPanel(id="partial_budget",
                                            tabPanel("Robots", value=base_profiles[1],
                                                     uiPartialBudget("Robots")),
                                            tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                     uiPartialBudget("Retrofit Parlors")),
                                            tabPanel("New Parlors", value=base_profiles[3],
                                                     uiPartialBudget("New Parlors")),
                                            tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                                     helpText("This assumes first Retrofit Parlors and then Robots."),
                                                     helpText("Values are taken from the two profiles.")),
                                            tabPanel("Retrofit/New",  value=combo_profiles[2],
                                                     helpText("This assumes first Retrofit Parlors and then New Parlors."),
                                                     helpText("Values are taken from the two profiles.")),
                                            tabPanel("Summary",
                                                     helpText("Summary"))
                                )
               # ) 
      ),
      # ---------- Cash Flow Analysis -----------
      tabPanel("Cash Flow", value = "Cash_Flow",
                fluidRow(column(6, offset=3,
                                h4("Cash Flow Analysis",align="center")
                )),
                tabsetPanel(id="cash_flow",
                            tabPanel("Robots", value=base_profiles[1],
                                     uiCashFlow("Robots")),
                            tabPanel("Retrofit Parlors", value=base_profiles[2],
                                     uiCashFlow("Retrofit Parlors")),
                            tabPanel("New Parlors", value=base_profiles[3],
                                     uiCashFlow("New Parlors")),
                            tabPanel("Retrofit/Robots",  value=combo_profiles[1],
                                     helpText("This assumes first Retrofit Parlors and then Robots."),
                                     helpText("Values are taken from the two profiles.")),
                            tabPanel("Retrofit/New",  value=combo_profiles[2],
                                     helpText("This assumes first Retrofit Parlors and then New Parlors."),
                                     helpText("Values are taken from the two profiles.")),
                            tabPanel("Summary",
                                     helpText("Summary"))
                )
      ),
#       # ---------- Additional Analyses -----------
#       navbarMenu("More", value = "More", 
#                  tabPanel("Robustness Checks",
#                           conditionalPanel("input.budget==0",
#                                            div(helpText("Please review all tabs in Data Entry."),align="center")
#                           ),
#                           conditionalPanel("input.budget>0",
#                                            fluidRow(column(6, offset=3,
#                                                            radioButtons("robust", "Robustness analysis options", 
#                                                                         choices=c("Off","Sensitivity","Scenarios")),
#                                                            helpText("To assess the robustness of your results, consider changes in key variables. 
#                                                  Sensitivity Analysis uses a change in one variable at a time, whereas
#                                                  Scenario Analysis uses changes in a set of related variables at a time.
#                                                  In Data Entry tab, the results of Sensitivity or Scneario Analysis will 
#                                                  appear below the baseline results in a parallel fashion."),
#                                                            conditionalPanel('input.robust=="Sensitivity"', br(), hr(),
#                                                                             includeMarkdown(file.path("text","sensitivity.md"))),
#                                                            conditionalPanel('input.robust=="Scenarios"', br(), hr(),
#                                                                             includeMarkdown(file.path("text","scenario.md")))
#                                            ))
#                           )),
#                  
#                  tabPanel("Robots vs Parlors",
#                           conditionalPanel("input.budget==0",
#                                            div(helpText("Please review all tabs in Data Entry."),align="center")
#                           ),
#                           conditionalPanel("input.budget>0",
#                                            source(file.path("ui_files", "ui_robot_parlor.R"), local=TRUE)$value  
#                                            
#                           ))
#       ),
      # ---------- About -----------
      tabPanel("About", value = "About", 
               fluidRow(column(width=1),
                        column(width=10,
                               includeMarkdown(file.path("text","about.md")),
                               br(), br()
                        ))
      ),
      useShinyjs(),
      collapsible = TRUE)
  )
) 




