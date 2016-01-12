library(shiny)
library(shinyBS)
library(shinydashboard)
library(rmarkdown)
library(googlesheets)
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
        #     menuItem("Completed Posts", tabName = "completedTab", icon = icon("trophy")),
        #     menuItem("Resolved Posts", tabName = "resolvedTab", icon = icon("check")),
        #     menuItem("Discontinued Posts", tabName = "discontinuedTab", icon = icon("moon-o")),
        menuItem("Table View", tabName = "tableTab", icon = icon("th")),
        menuItem("About", tabName = "aboutTab", icon = icon("cog"))
      )
    ), 
    dashboardBody(    
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
      tabItems(
        tabItem(tabName = "mainTab", 
                # ------------- Main --------------------
                bsCollapse(id = "collapseMain", multiple = FALSE, open = "Posts", 
                           bsCollapsePanel("Posts", style = "info",
                                           source(file.path("ui_files","ui_posts.R"), local=TRUE)$value 
                           ),
                           bsCollapsePanel("Details", style = "success",
                             source(file.path("ui_files","ui_details.R"), local=TRUE)$value
                           )
                )
      ),
      tabItem(tabName ="postTab",
              source(file.path("ui_files","ui_new_post.R"), local=TRUE)$value
      ),
      # ------------- People --------------------
      tabItem(tabName="peopleTab",
              source(file.path("ui_files","ui_people.R"), local=TRUE)$value
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
    )),
    shinyjs::useShinyjs()
  ))   





