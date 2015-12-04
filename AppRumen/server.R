library(shiny)
library(shinyBS)
library(shinydashboard)
library(googlesheets)
library(mongolite) 
suppressPackageStartupMessages(library(dplyr))
library(magrittr)

source("helpers.R")


shinyServer(function(input, output, session) {
     
   # Give an initial value to the timestamp field
     updateTextInput(session, "timestamp", value = get_time_human())
     updateTextInput(session, "postID", value = get_time_epoch())
  
     rv <- reactiveValues(view = NULL, edit_auth = FALSE, 
                          selectedPost = NULL, selectedComments = NULL)
     browser()
     
     ## Connect to mongodb from a mongolab location 
     host <- "ds061954.mongolab.com:61954" 
     username <- "user1" 
     password <- "user1" 
     db <- "app_rumen" 
     url <- paste0("mongodb://",username,":",password,"@", host, "/",db) 

     
     # load databases after "Send"  
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
     
     # Prepare postings in Posts
     output$postboxes <- renderUI({
       rv$back_to_active_post
       
       table_posts_copy <-  mongo(collection="posts", db=db, url = url)$find()
       table_posts_copy <-  table_posts_copy[table_posts_copy$post_category %in% input$filterCategory,]
       table_posts_copy$average_interest[table_posts_copy$average_interest=="NA"] <- 0        
       tmp_sort <- switch(input$sortPost, 
                          "Most recently posted"=table_posts_copy$timestamp,
                          "Most recently commented"=table_posts_copy$timestamp_comment,
                          "Most commented"= - table_posts_copy$cumulative_comments, 
                          "Most viewed"= - table_posts_copy$cumulative_views,
                          "Highest interests"= - table_posts_copy$average_interest)
       
       sorted_table_posts <- table_posts_copy[order(tmp_sort),]
       
       N <- dim(table_posts_copy)[1]
       if (is.null(input$n_boxes) || is.na(input$n_boxes)) {
         n <- 0
       } else {
         n <- min(input$n_boxes,N)
       }
       
       lapply(1:n, function(i) {
         tmp_post <- sorted_table_posts[i,]  
         box(
           # browser(), 
           p("App Name:  ", strong(tmp_post$post_name),br(),
             "Category:  ", tmp_post$post_category,br(),
             "Description:  ",paste(strtrim(tmp_post$post,140),"..."),br(),
             "Views:  ", tmp_post$cumulative_views, br(),
             "Comments:  ", tmp_post$cumulative_comments, br(),
             "Average Interest:  ",tmp_post$average_interest, br(),
             "Date:  ", strtrim(tmp_post$timestamp,10),br(),
             "By:  ", tmp_post$user_name
           ),
           br(),
           actionButton(inputId = paste0("view", i),"View","primary") 
         )
       })
     })
     
     observeEvent(input$n_boxes, {
     lapply(c(1:input$n_boxes), function(x) {
       observeEvent(input[[paste0("view",x)]], ({
         updateCollapse(session, "collapseMain", open = "Details")
         rv$view <- x
       }))
     })
     })
 

     source("session_post_comment.R", local=TRUE)
     
     source("session_edit.R", local=TRUE)
     
     source("session_notification.R", local=TRUE)
     
     source("session_misc.R", local=TRUE)

})


