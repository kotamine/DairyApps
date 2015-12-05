library(shiny)
library(shinyBS)
library(shinydashboard)
library(googlesheets)
library(mongolite) 
suppressPackageStartupMessages(library(dplyr))
library(magrittr)

source("helpers.R")


shinyServer(function(input, output, session) {
     
     rv <- reactiveValues(view = NULL, edit_auth = FALSE, 
                          selectedPost = NULL, selectedComments = NULL)
     
     user_session <- reactiveValues(info = NULL)
     
     browser()
     
     ## Connect to mongodb from a mongolab location 
     host <- "ds061954.mongolab.com:61954" 
     username <- "user1" 
     password <- "user1" 
     db <- "app_rumen" 
     url <- paste0("mongodb://",username,":",password,"@", host, "/",db) 

     
     # load databases after "Send" operations   
     table_posts <- reactive({
       input$post_send
        mongo(collection="posts", db=db, url = url)$find()
     })
     
     table_comments <- reactive({
       input$comment_send
       mongo(collection="comments", db=db, url = url)$find()
     })
     
     table_archive_posts <- reactive({
       input$edit_send
       mongo(collection="table_archive_posts", db=db, url = url)$find()
     })
     
     table_archive_comments <- reactive({
       input$edit_send
       mongo(collection="table_archive_comments", db=db, url = url)$find()
     })
     
 
     source("session_populate.R", local=TRUE)
     
     source("session_post_comment.R", local=TRUE)
     
     source("session_details_edit.R", local=TRUE)
     
     source("session_notification.R", local=TRUE)
     
     source("session_misc.R", local=TRUE)

})


