library(shiny)
library(shinyBS)
library(shinydashboard)
library(rmarkdown)
library(googlesheets)
library(mongolite) 
suppressPackageStartupMessages(library(dplyr))
library(magrittr)

source("helpers.R")

 
shinyServer(function(input, output, session) {
     
     rv <- reactiveValues(view = NULL, edit_auth = FALSE, 
                          selectedPost = NULL, selectedComments = NULL,
                          comment_reset =0, post_reset=0, 
                          back_to_selected_post = 0, 
                          back_to_selected_user = 0,
                          view_rest=0) 
     
     user_session <- reactiveValues(info = NULL)
     
     tables <- reactiveValues()
     
     browser()
     
     
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
     mongo_system_use <-   mongo(collection="system_use", db=db, url = url)

     mongo_posts$index('{"post_category":1}')

     source(file.path("session_files","session_populate.R"), local=TRUE)

     source(file.path("session_files","session_post_comment.R"), local=TRUE)

     source(file.path("session_files","session_details_edit.R"), local=TRUE)

     source(file.path("session_files","session_user_edit.R"), local=TRUE)

     source(file.path("session_files","session_notification.R"), local=TRUE)

     source(file.path("session_files","session_misc.R"), local=TRUE)

}) 


