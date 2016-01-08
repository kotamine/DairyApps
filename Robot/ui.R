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

# Load files that contain functions
source(file.path("ui_files", "ui_data_entry_functions.R"), local=TRUE)
source(file.path("ui_files", "ui_partial_budget.R"), local=TRUE)  
source(file.path("ui_files", "ui_cash_flow.R"), local=TRUE)  
source(file.path("ui_files", "ui_dashboard.R"), local=TRUE) 
source(file.path("ui_files", "ui_sensitivity.R"), local=TRUE)  


base_profiles <- c("Robots","Retrofit","New")
# combo_profiles <- c("RetrofitRobots","RetrofitNew")

refProfileName <-  function(x) {
  switch(x, 
         "Robots"="Robots",
         "Retrofit"="Retrofit Parlors",
         "New"="New Parlors"
  )
}

shinyUI(  
  fluidPage(
    # list(tags$head(HTML("  "))),
    # tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/UMN.css"),
    # includeHTML("www/UMN_header.html"),
    #  list(tags$head(HTML(" "))),
    
    # Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
    #   $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
    # });
    
    # tags$head(tags$script("
    #     window.onload = function() {
    #                       $('#prCapital a:contains(\"Robots\")').parent().addClass('disabled');
    #                       $('#prCapital a:contains(\"Retrofit\")').parent().addClass('disabled');
    #                       $('#prCapital a:contains(\"New\")').parent().addClass('disabled');
    #                       $('#prMilk a:contains(\"Retrofit\")').parent().addClass('disabled');
    #                       $('#prMilk a:contains(\"New\")').parent().addClass('disabled');
    #                       };
    #                       
    #                       ")),
    tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
                               function(message) {
                               eval(message.code);
                               }
    );
                               '))),
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
               conditionalPanel("input.case1>0",
                                source(file.path("ui_files","ui_data_entry_tabs.R"), local=TRUE)$value,
                                
                                conditionalPanel("input.calculation_switch=='ON'",
                                                 tabsetPanel(id="dashboard", 
                                                             #                            The following does not work for some reason..
                                                             #                            lapply(base_profiles, function(profile) {
                                                             #                              tabPanel(refProfileName(profile), value=profile,
                                                             #                                       uiDashboard(profile))
                                                             #                            }) 
                                                             tabPanel("Robots", value=base_profiles[1],
                                                                      uiDashboard(base_profiles[1])),
                                                             tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                                      uiDashboard(base_profiles[2])),
                                                             tabPanel("New Parlors", value=base_profiles[3],
                                                                      uiDashboard(base_profiles[3]))
                                                 )
                                ),
                                shinyjs::hidden(radioButtons("IOFC",NULL,choices=c("per cow","per cwt"), 
                                                             selected="per cwt")),
                                shinyjs::hidden(radioButtons("NAI",NULL, choices=c("before tax", "after tax"), 
                                                             selected="after tax")),
                                br(), 
                                div(radioButtons("calculation_switch","Calculation Switch",
                                                 choices=c("OFF","ON"), inline=TRUE),align="center"),
                                # --------- Data Table ---------
                                br(),
                                hr(),
                                fluidRow(column(2, offset=1,
                                                actionButton("default_data","Default Input-Data")),
                                         column(3,
                                                downloadButton("data_download","Download Input-Data")),
                                         column(3, 
                                                # fileInput() is passessed from the server
                                                uiOutput('resettableInput'),
                                                bsAlert("upload_alert")),   
                                         column(2, actionButton("remove", "Remove Input-Data"))),
                                br(), br()
               ), 
               # Set up a case selection as a starting default value
               conditionalPanel("input.case1==0",
                                fluidRow(
                                  column(8, offset=2,
                                         h4("Please select a case that best describes your operation."),
                                         helpText("The case will load 
                                            an appropriate set of starting values for your user-data inputs."),
                                         br(),
                                         includeMarkdown(file.path("text","cases.md")),
                                         div(bsButton("case1","Select Case 1", style="primary"),align="center")
                                  )
                                )
               )
      ),
      # ---------- Partial Budget Analysis -----------
      tabPanel("Partial Budget", value = "Partial_Budget",
               
               fluidRow(column(6, offset=3,
                               h3("Partial Budget Analysis",align="center")
               )),
               conditionalPanel("input.calculation_switch=='ON'",
                                tabsetPanel(id="partial_budget",
                                            tabPanel("Robots", value=base_profiles[1],
                                                     uiPartialBudget(base_profiles[1])),
                                            tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                     uiPartialBudget(base_profiles[2])),
                                            tabPanel("New Parlors", value=base_profiles[3],
                                                     uiPartialBudget(base_profiles[3]))
                                )
               ),
               conditionalPanel("input.calculation_switch!='ON'",
                                div(helpText("To activate, turn on Calculation Swtich under Data Entry tab."),
                                    align="center")
               )
      ),
      # ---------- Cash Flow Analysis -----------
      tabPanel("Cash Flow", value = "Cash_Flow",
               h3("Cash Flow Analysis",align="center"),
               conditionalPanel("input.calculation_switch=='ON'",
                                tabsetPanel(id="cash_flow",
                                            tabPanel("Robots", value=base_profiles[1],
                                                     uiCashFlow(base_profiles[1])),
                                            tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                     uiCashFlow(base_profiles[2])),
                                            tabPanel("New Parlors", value=base_profiles[3],
                                                     uiCashFlow(base_profiles[3]))
                                )
               ),
               conditionalPanel("input.calculation_switch!='ON'",
                                div(helpText("To activate, turn on Calculation Swtich under Data Entry tab."),
                                    align="center")
               )
      ),
      # ---------- Summary  -----------
      tabPanel("Summary", value="Summary",
               h3("Summary",align="center"),
               conditionalPanel("input.calculation_switch=='ON'",
                                source(file.path("ui_files","ui_summary.R"), local=TRUE)$value
               ),
               conditionalPanel("input.calculation_switch!='ON'",
                                div(helpText("To activate, turn on Calculation Swtich under Data Entry tab."),
                                    align="center")
               )
      ),
      # ---------- Sensitivity Analysis -----------
      tabPanel("Sensitivity", value="Sensitivity",
               h3("Sensitivity Analysis",align="center"),
               conditionalPanel("input.calculation_switch=='ON'",
                                tabsetPanel(id="sensitivity",
                                            tabPanel("Robots", value=base_profiles[1],
                                                     uiSensitivity(base_profiles[1])),
                                            tabPanel("Retrofit Parlors", value=base_profiles[2],
                                                     uiSensitivity(base_profiles[2])),
                                            tabPanel("New Parlors", value=base_profiles[3],
                                                     uiSensitivity(base_profiles[3]))
                                )
               ),
               conditionalPanel("input.calculation_switch!='ON'",
                                div(helpText("To activate, turn on Calculation Swtich under Data Entry tab."),
                                    align="center")
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
      #       ),
      # ---------- About -----------
      tabPanel("About", value = "About", 
               fluidRow(column(width=2),
                        column(width=8,
                               includeMarkdown(file.path("text","about.md")),
                               br(), br()
                        ))
      ),
      useShinyjs(), 
      collapsible = TRUE)
  )
) 




