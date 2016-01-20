library(shiny)
library(shinyBS)
library(shinydashboard)
library(rmarkdown)
library(DT)
library(googlesheets)
library(data.table)
suppressPackageStartupMessages(library(dplyr))

source("helpers.R")

shinyUI( 
  dashboardPage( 
    dashboardHeader(title ="AppRumen",
                    dropdownMenuOutput("messageMenu"),
                    dropdownMenuOutput("notificationMenu"),
                    dropdownMenuOutput("taskMenu")
    ), 
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        
        # The dynamically-generated user panel
        uiOutput("userpanel"),
        
        menuItem("All Posts",tabName = "mainTab",icon=icon("comments")),
        menuItem("New Post", tabName = "postTab", icon = icon("lightbulb-o")),
        menuItem("People", tabName="peopleTab", icon=icon("group")),
        menuItem("Notice",tabName="notice",icon=icon("check")),
        menuItem("Table View", tabName = "tableTab", icon = icon("th")),
        menuItem("About", tabName = "aboutTab", icon = icon("cog"))
      )
    ), 
    dashboardBody(    
      tags$head(
        tags$link(rel = "stylesheet", typfe = "text/css", href = "custom.css")
      ),
      div(id="log_in_page",
        h2(icon("google-plus-square"), "Please log in:", align="center"),
        div(actionButton("log_in_0","Login","primary"), align="center")),
      shinyjs::hidden(div(id="after_log_in",
      tabItems(
        tabItem(tabName = "mainTab", 
                # ------------- Main --------------------
#                 bsCollapse(id = "collapseMain", multiple = FALSE, open = "Posts", 
#                            bsCollapsePanel("Posts", style = "info",
#                                            source(file.path("ui_files","ui_posts.R"), local=TRUE)$value 
#                            ),
#                            bsCollapsePanel("Details", style = "success",
#                              source(file.path("ui_files","ui_details.R"), local=TRUE)$value
#                            )
#                 )
                source(file.path("ui_files","ui_main.R"), local=TRUE)$value
      ),
      tabItem(tabName ="postTab",
              source(file.path("ui_files","ui_new_post.R"), local=TRUE)$value
      ),
      # ------------- People --------------------
      tabItem(tabName="peopleTab",
              source(file.path("ui_files","ui_people.R"), local=TRUE)$value
      ),
      # ------------- Notice --------------------
      tabItem(tabName="notice",
              source(file.path("ui_files","ui_notice.R"), local=TRUE)$value
              ),   
      tabItem(tabName="tableTab",
              selectInput("selectTable","Table Type",
                          choices=c("posts","completed_posts", "resolved_posts",
                                    "discontinued_posts", "archive_posts",
                                    "comments", "archive_comments")),
              DT::dataTableOutput("viewTable")
      ),
      tabItem(tabName="aboutTab",
            includeMarkdown(file.path("text","about.md"))
      )
     )
    ))),
    shinyjs::useShinyjs()
  ))   





