
source("global_parameters.R")


common_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                       sheetIndex = 1, stringsAsFactors =FALSE) 
profile_specific_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                                 sheetIndex = 2, stringsAsFactors =FALSE)  

partial_budget_notes <- read.xlsx("www/partial_budget_notes.xlsx", 
                                       sheetIndex = 1, stringsAsFactors =FALSE) 

# Load files that contain functions
source(file.path("ui_files", "ui_data_entry_functions.R"), local=TRUE)
source(file.path("ui_files", "ui_partial_budget.R"), local=TRUE)  
source(file.path("ui_files", "ui_cash_flow.R"), local=TRUE)  
source(file.path("ui_files", "ui_dashboard.R"), local=TRUE) 
source(file.path("ui_files", "ui_sensitivity.R"), local=TRUE)  


shinyUI(  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/UMN.css")
    ),
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
    #                       Shiny.addCustomMessageHandler('activeTabs', function(tab_label) {
    #                       $('#mynavlist a:contains(\"' + tab_label + '\")').parent().removeClass('disabled');
    #                       });
    #                       ")),
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
               conditionalPanel("input.case1>0",
                                div(id="dataEntry", h3("Data Entry",HTML(paste(icon("info-circle")))),align="center"),
                                source(file.path("ui_files","ui_data_entry_tabs.R"), local=TRUE)$value,
                                # Need to add "$value" for including source in UI: 
                                # otherwise "TRUE" will show up at the end of file
                                conditionalPanel("input.calculation_switch=='ON'",
                                div(id="dashboardPanel", h3("Dashboard",HTML(paste(icon("info-circle")))),align="center"),
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
                                                 choices=c("OFF","ON"), inline=TRUE),id ='calswitch',align="center"),
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
                                         h3("Select a case that best describes your operation."),
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
                               div(id="partialBudget",
                                   h3("Partial Budget Analysis",HTML(paste(icon("info-circle")))),align="center"),
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
               div(id="cashFlow",
                   h3("Cash Flow Analysis",HTML(paste(icon("info-circle")))),align="center"),
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
               div(id="summary",
                   h3("Summary of Investment Profiles",
                      HTML(paste(icon("info-circle")))),align="center"),
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
               div(id="sensitivityAnalysis",
                   h3("Sensitivity Analysis",HTML(paste(icon("info-circle")))),align="center"),
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
      # ---------- About -----------
      tabPanel("About", value = "About", 
               fluidRow(column(width=2),
                        column(width=8,
                               includeMarkdown(file.path("text","about.md")),
                               br(), br()
                        ))
      ),
      useShinyjs(), 
      collapsible = TRUE),
      source(file.path("ui_files","ui_tooltip.R"), local=TRUE)$value,
      source(file.path("ui_files","ui_modal.R"), local=TRUE)$value
    )
)   




