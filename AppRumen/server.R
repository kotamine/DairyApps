library(shiny)
library(shinyBS)
library(shinydashboard)
library(rmarkdown)
library(DT)
library(googlesheets)
library(mongolite)
library(data.table)
suppressPackageStartupMessages(library(dplyr))
library(magrittr)

source("helpers.R")

 
shinyServer(function(input, output, session) {
     
     rv <- reactiveValues(view = NULL, edit_auth = FALSE, edit_user_auth=FALSE,
                          selectedPost = NULL, selectedComments = NULL,
                          comment_reset =0, post_reset=0, 
                          reply_reset=0, msg_rest=0,
                          message_content = NULL,
                          back_to_selected_post = 0, 
                          back_to_selected_user = 0,
                          view_rest=0) 
     
     user_session <- reactiveValues(info = NULL)

     tables <- reactiveValues()
     
     
     # Log in when opening the website
     observeEvent(input$log_in_0, {
       # Give googlesheets permission to access your spreadsheets and google drive
       gs_auth( new_user = TRUE)
       user_session$info <- gs_user()
       updateTextInput(session, "user_name", value = user_session$info$displayName)
       updateTextInput(session, "email_address", value =  user_session$info$emailAddress)
       updateTextInput(session, "comment_user_name", value = user_session$info$displayName)
       updateTextInput(session, "comment_email_address", value =  user_session$info$emailAddress)
     })
     
     # Show/hide the UI through log-in 
     observe({
       if (!is.null(user_session$info)) {
        shinyjs:: hide("log_in_page")
        shinyjs:: show("after_log_in")
       } else {
         shinyjs:: show("log_in_page")
         shinyjs:: hide("after_log_in")
       }
     })
     
     
     ## Connect to mongodb from a mongolab location
     host <- "ds061954.mongolab.com:61954"
     username <- "user1"
     password <- "user1"
     db <- "app_rumen"
     url <- paste0("mongodb://",username,":",password,"@", host, "/",db)

     mongo_posts <- mongo(collection="posts", db=db, url = url)
     mongo_comments <- mongo(collection="comments", db=db, url = url)
     mongo_archive_posts <- mongo(collection="archive_posts", db=db, url = url)
     mongo_archive_comments <- mongo(collection="archive_comments", db=db, url = url)
     mongo_completed_posts <- mongo(collection="completed_posts", db=db, url = url)
     mongo_resolved_posts <- mongo(collection="resolved_posts", db=db, url = url)
     mongo_discontinued_posts <- mongo(collection="discontinued_posts", db=db, url = url)
     mongo_users <- mongo(collection="users", db=db, url = url)
     mongo_likes <- mongo(collection="likes", db=db, url = url)
     mongo_follow_post <- mongo(collection="follow_post", db=db, url = url)
     mongo_follow_user <- mongo(collection="follow_user", db=db, url = url)
     mongo_messages <-   mongo(collection="messages", db=db, url = url)
     mongo_system_use <-   mongo(collection="system_use", db=db, url = url)

     # browser()
     mongo_posts$index('{"post_category":1}')

     source(file.path("session_files","session_populate.R"), local=TRUE)

     source(file.path("session_files","session_post_comment.R"), local=TRUE)

     source(file.path("session_files","session_details_edit.R"), local=TRUE)

     source(file.path("session_files","session_user_edit.R"), local=TRUE)

     source(file.path("session_files","session_notice.R"), local=TRUE)
     
     source(file.path("session_files","session_notification.R"), local=TRUE)

     source(file.path("session_files","session_misc.R"), local=TRUE)

}) 


