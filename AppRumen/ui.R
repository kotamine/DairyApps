library(shiny)
library(shinyBS)
library(shinydashboard)
library(googlesheets)
library(rmongodb)
suppressPackageStartupMessages(library(dplyr))

source("helpers.R")


shinyUI( 
  dashboardPage( 
    dashboardHeader(title ="AppRumen",
                   dropdownMenuOutput("messageMenu"),
                    dropdownMenuOutput("notificationMenu"),
                    dropdownMenuOutput("taskMenu")
                   ), 
    # shinyjs::hidden(selectInput("storage", "Select Storage Type", choices=c("gsheets"))),
 
    
#     ## Sidebar content
#     dashboardSidebar(
#       sidebarMenu(
#         menuItem("New Post", tabName = "postTab", icon = icon("dashboard")),
#         menuItem("View Table", tabName = "tableTab", icon = icon("dashboard"))
#       )
#     ),
#     
#     ## Body content
#     dashboardBody(
#       tabItems(
#         # First tab content
#         tabItem(tabName = "postTab",
#                 fluidRow(
#                   box(plotOutput("plot1", height = 250)),
#                   
#                   box(
#                     title = "Controls",
#                     sliderInput("slider", "Number of observations:", 1, 100, 50)
#                   )
#                 )
#         ),
#         
#         # Second tab content
#         tabItem(tabName = "tableTab",
#                 h2("Widgets tab content")
#         )
#       )
#     ),
    ## Sidebar content
dashboardSidebar(
  sidebarMenu(
    id = "tabs",
#     # Custom CSS to hide the default logout panel
#     tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
#     
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    
    menuItem("Active Posts",tabName = "mainTab",icon=icon("group")),
    menuItem("New Post", tabName = "postTab", icon = icon("comment")),
    menuItem("Completed Posts", tabName = "completedTab", icon = icon("trophy")),
    menuItem("Resolved Posts", tabName = "resolvedTab", icon = icon("check")),
    menuItem("Discontinued Posts", tabName = "discontinuedTab", icon = icon("moon-o")),
    menuItem("Table View", tabName = "tableTab", icon = icon("th")), 
    menuItem("About", tabName = "aboutTab", icon = icon("cog"))
  )
), 
  dashboardBody(    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ), 
    # includeMarkdown(file.path("text", "appDesc.md"))
      tabItems(
#         id="mainTabs",type="tabs",
        tabItem(tabName = "mainTab", 
      # ------------- Main --------------------
               bsCollapse(id = "collapseMain", open = "Posts",
                          bsCollapsePanel(
                            "Posts", style = "info",
                            checkboxGroupInput("filterCategory", "Category", choices=c("Milk","Forage","Labor","Social"),
                                               selected = c("Milk","Forage","Labor","Social"), inline = TRUE), 
                            fluidRow(column(5,numericInput("n_boxes","Number of Posts", value=10, min=0,step=5,max=100)),
                                     column(7,selectInput("sortPost","Sort",
                                                          choices=c("Most recently posted","Most recently commented",
                                                                    "Most commented", "Most viewed",
                                                                    "Highest interests")))),
                            fluidRow(
                              uiOutput("postboxes")
                            ) 
                          ),
                          bsCollapsePanel(
                            "Details", style = "success",
                            # ------------- Comment --------------------
                            div(id = "post_form",
                                #h4("Suggested App Name"),
                                uiOutput("selectedPost"),
                                br(),
                                fluidRow(column(5,
                                  actionButton("edit","Edit (author only)","primary")
                                ), 
                                column(5,
                                  conditionalPanel("input.view_archive_comments ==  
                                                 input.close_archive_comments",     
                                  actionButton("view_archive_comments","View comments in archive","link")
                                       ))),
                                conditionalPanel("input.view_archive_comments >
                                                 input.close_archive_comments",
                                    br(),
                                    numericInput("n_archive_comments","Number of archived comments",
                                                 min=1, step=5, value=10, max=100),
                                    uiOutput("selectedArchiveComments"),
                                    fluidRow(column(3,offset=9, 
                                                    actionButton("close_archive_comments","Hide","link")
                                       ))
                                ),
                                br(), br(),
#                                 textInput(inputId="comment", label="Comment"),
#                                 tags$head(tags$style(type="text/css", "#comment {height: 100px}")),
#                                 tags$textarea(id="comment", rows=30, cols=40, "Default value"),
                                h5(strong("Comment")), 
                                inputTextarea('comment', '',5,50), 
                                   tags$head(tags$style(type="text/css", "#comment {border-color: #C0C0C0}")),
                                selectInput("novelty","Novelty", 
                                            choices=c("That's a new idea!"=1,
                                                       "Tweak a similar App!"=2,
                                                       "There's an App for that!"=3)),
                                conditionalPanel("input.novelty>1",
                                                 textInput("app_link","Name of an Existing App",value="NA")),
                                sliderInput("interest","Interest",min=0,max=5,step=1,value=3),
                                tags$head(tags$style(type="text/css", "#post {height: 100px}")),
                                br(),
                                actionButton("gmail2","Google Account", "primary"), br(),br(), 
                                textInput("comment_user_name","User Name"),
                                textInput("comment_email_address","Email Address"),
                                
                                actionButton("comment_send", "Send","primary"),
                                shinyjs::hidden(
                                  span(id = "submitMsg2", "Sending...", style = "margin-left: 15px;")
                                )
                            ),
                            shinyjs::hidden(
                              div(id = "error2",
                                  div(br(), tags$b("Error: "), span(id = "errorMsg2")),
                                  style = "color: red;"
                              )
                            ),
                            # hidden input field tracking the timestamp of the submission
                            shinyjs::hidden(textInput("timestamp2", "")),
                            shinyjs::hidden(textInput("commentID", ""))
               )
          )
    ),
          tabItem(tabName ="postTab",
            # ------------- Post --------------------
            #$ title="New Post", id="postTab",
               div(id = "post_form",
                   textInput("post_name","Suggested App Name"),
                   selectInput("post_category","Category", 
                               choices=c("Milk","Forage","Labor","Social")),
#                    textInput(inputId="post", label="Description"),
#                    tags$head(tags$style(type="text/css", "#post {height: 200px}")),
                   h5(strong("Description")), 
                   inputTextarea('post', '',20,50), 
                   tags$head(tags$style(type="text/css", "#post {border-color: #C0C0C0}")),
               br(), br(),
               actionButton("gmail1","Google Account","primary"), br(), br(),    
               textInput("user_name","User Name"),
               textInput("email_address","Email Address"),
           actionButton("post_send", "Send","primary"),
           shinyjs::hidden(
                span(id = "submitMsg", "Sending...", style = "margin-left: 15px;")
           )
           ),
           shinyjs::hidden(
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "errorMsg")),
                    style = "color: red;"
                )
           ),
           # hidden input field tracking the timestamp of the submission
           shinyjs::hidden(textInput("timestamp", "")),
           shinyjs::hidden(textInput("postID", "")),
           shinyjs::hidden(numericInput("edits","", value=0)),
           shinyjs::hidden(numericInput("currnet_views","", value=0)),
           shinyjs::hidden(numericInput("cumulative_views","", value=0)),
           shinyjs::hidden(numericInput("current_comments","", value=0)),
           shinyjs::hidden(numericInput("cumulative_comments","", value=0)),
           shinyjs::hidden(textInput("timestamp_comment","", value=NA)),
           shinyjs::hidden(numericInput("average_interests","",value=NA))
     ),
     tabItem(tabName="completedTab", 
             helpText("list of completed posts")
     ),
     tabItem(tabName="resolvedTab", 
             helpText("list of resolved posts for which app already exists")
     ),
     tabItem(tabName="discontinuedTab", 
             helpText("list of discontinued posts")
     ),
     tabItem(tabName="tableTab",
          selectInput("selectTable","Table Type",
                      choices=c("table_posts","table_comments",
                                "table_archive_posts", "table_archive_comments")),
          DT::dataTableOutput("viewTable")
     ),
    tabItem(tabName="aboutTab", 
      helpText("some explanation and credits")
     )
)
),
shinyjs::useShinyjs()
))




