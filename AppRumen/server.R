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
     
     
     # N <- mongo(collection="posts", db=db, url = url)$count()  # not needed?  
     
     # Prepare postings in Posts
     output$postboxes <- renderUI({
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
     
     lapply()
     observeEvent(input$view1, ({
       updateCollapse(session, "collapseMain", open = "Details")
       rv$view <- 1
     }))
     
     observeEvent(input$view2, ({
       updateCollapse(session, "collapseMain", open = "Details")
       rv$view <- 2
     }))
     
     observeEvent(input$view3, ({
       updateCollapse(session, "collapseMain", open = "Details")
       rv$view <- 3
     })) 
     
     
     

    

     # ---------- Event: post_send button ------------
     observeEvent(input$post_send, {
          # Update the timestamp field to be the current time
          updateTextInput(session, "timestamp", value = get_time_human())
          updateTextInput(session, "postID", value = get_time_epoch())
       
          # User-experience stuff
          shinyjs::disable("post_send")
          shinyjs::show("submitMsg")
          shinyjs::hide("error")
          on.exit({
               shinyjs::enable("post_send")
               shinyjs::hide("submitMsg")
          })
          
          # Add a row to the data (show an error message in case of error)
          tryCatch({
              # save_data_gsheets(post_data(), "table_posts")
              "table_posts" %>% gs_title %>% gs_add_row(input = post_data())
               updateTabItems(session, "tabs","postTab")
          },
          error = function(err) {
               shinyjs::text("errorMsg", err$message)
               shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
               shinyjs::logjs(err)
          })
     })
     
     # ---------- Event: comment_send button ------------
     observeEvent(input$comment_send, {
       # Update the timestamp field to be the current time
       updateTextInput(session, "timestamp2", value = get_time_human())

       # Increase the counters for comments  
       browser()
       tmp_post <- rv$selectedPost
       if (tmp_post$average_interest=="NA") {
            tmp_post$average_interest <-   input$interest
       } 
       else {
            tmp_post$average_interest <- (tmp_post$average_interest*tmp_post$cumulative_comments + 
                                               input$interest) / (tmp_post$current_comments + 1)     
       }
       
       tmp_post$current_comments <- tmp_post$cumulative_comments + 1
       tmp_post$cumulative_comments <- tmp_post$cumulative_comments + 1
       tmp_post$timestamp_comment <- get_time_epoch()
       "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post, 
                                                    anchor=paste0("A",rv$post_location), col_names=FALSE)
       
       # User-experience stuff
       shinyjs::disable("comment_send")
       shinyjs::show("submitMsg2")
       shinyjs::hide("error2")
       on.exit({
         shinyjs::enable("comment_send")
         shinyjs::hide("submitMsg2")
       })
       
       # Add a row to the data (show an error message in case of error)
       tryCatch({
         #save_data_gsheets(comment_data(), "table_comments")
         "table_comments" %>% gs_title %>% gs_add_row(input = comment_data())
         updateTabItems(session,"tabs","mainTab")
       },
       error = function(err) {
         shinyjs::text("errorMsg2", err$message)
         shinyjs::show(id = "error2", anim = TRUE, animType = "fade")      
         shinyjs::logjs(err)
       })
     })
     


     
     # Show tables of posts and comments 
     output$viewTable <- DT::renderDataTable({
          view_posts <- table_posts(); 
          view_posts <- view_posts %>% select(timestamp, post_name, post_category, user_name,
                                              edits,current_views,cumulative_views,current_comments,
                                              cumulative_comments, average_interest)
          view_comments <- table_comments(); 
          view_comments <- view_comments %>% select(timestamp2, post_name, comment_user_name,
                                              novelty, app_link, interest)
          
          view_archive_posts <- table_archive_posts(); 
          view_archive_posts <- view_archive_posts %>% select(timestamp, post_name, post_category, user_name,
                                              edits,current_views,cumulative_views,current_comments,
                                              cumulative_comments, average_interest)
          view_archive_comments <- table_archive_comments(); 
          view_archive_comments <- view_archive_comments %>% select(timestamp2, post_name, comment_user_name,
                                                    novelty, app_link, interest)
          
          tbl <- switch(input$selectTable,
                 "table_posts"=  view_posts ,
                 "table_comments"=  view_comments,
                 "table_archive_posts"= view_archive_posts,
                 "table_archive_comments"= view_archive_comments
                 )
          DT::datatable( 
               tbl,
               rownames = FALSE, 
          options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
          )
     })
     
     

     
     # ------------ Show Selected Post and Enable Edit process ----------------
     # Prepare the display of a selectet post in Details
     output$selectedPost  <- renderUI({
          validate(
               need(!is.null(rv$view), 'No post is selected.')
          )
          # obtain a new copy of table (may be updated from some othere sessions)
         # table_posts_copy <- table_posts() 
#           table_posts_copy <- load_data_gsheets("table_posts")
#           table_comments_copy <- load_data_gsheets("table_comments") 
          table_posts_copy <-  mongo(collection="posts", db=db, url = url)$find()
          table_comments_copy <-  mongo(collection="comments", db=db, url = url)$find()
       
          rv$selectedPost <- table_posts_copy[rv$view,]
          tmp_post <- rv$selectedPost
          updateTextInput(session, "postID", value = tmp_post$postID)
          updateTextInput(session, "post_name", value = tmp_post$post_name)
          updateTextInput(session, "timestamp", value = get_time_human())
          
          tmp_comments <- table_comments_copy[tmp_post$postID==table_comments_copy$postID,]
          rv$selectedComments <- tmp_comments
          tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
          N_comments <- dim(tmp_comments)[1]
          
       if (rv$edit_auth) {
         #  prepare output$selectedPost for editing
         wellPanel(
           textInput("post_name_ed", "App Name", value = tmp_post$post_name),
           p( strong("By: "), tmp_post$user_name, br(), br()),
           selectInput("post_category_ed","Category", selected=tmp_post$post_category,
                       choices=c("Milk","Forage","Labor","Social")),
#            textInput(inputId="post_ed", label="Description",value= tmp_post$post),
#            tags$head(tags$style(type="text/css", "#post_ed {height: 200px; width: 100%; 
#                                 text-align:center; display: block;}")),
           h5(strong("Description")), 
           inputTextarea('post_ed', value= tmp_post$post,20,50), 
           tags$head(tags$style(type="text/css", "#post_ed {border-color: #C0C0C0}")),
           br(), 
             p(strong("Views: "), tmp_post$cumulative_views, br(),
              strong("Comments:"), tmp_post$cumulative_comments, br(), 
              strong("Average Interest: "),tmp_post$average_interest, br(),
              strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
              strong("Edits:"), tmp_post$edits, br(),
              strong("Views since last edit: "), tmp_post$current_views, br(),
              strong("Comments since last edit:"), tmp_post$current_comments, br(),
              br(), 
              strong("<< Comments >> ")),
           
           retrieveComments(N_comments, tmp_comments),
           selectInput("decision","Decision",choices=c("Continue editing", "Move it to Completed Posts",
                                                       "Move it to Existing App Found", "Move it to Discontinued")),
           sliderInput("completeness","Degree of Completion",min=0,step=5,value=5,max=100), 
           actionButton("edit_send","Finish editing and Update")
         )
       } else {
#        tmp_post <- table_posts_copy[rv$view,]
#        updateTextInput(session, "postID", value = tmp_post$postID)
#        updateTextInput(session, "post_name", value = tmp_post$post_name)
#        
       # Increase the counter of views            
#        tmp_location <- which(tmp_post$postID ==  table_posts_copy[,c("postID")] ) + 1
#        rv$post_location <- paste0("A",tmp_location)
#        tmp_post$current_views <- tmp_post$current_views + 1
#        tmp_post$cumulative_views <- tmp_post$cumulative_views + 1
#        "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,  
#                                                     anchor=rv$post_location, col_names = FALSE)
#         
       update_views <- paste0('{"$set":{', '"current_views":', (tmp_post$current_views + 1),
                               ', "cumulative_views":', (tmp_post$cumulative_views + 1), '}}')

        postID <- paste0('{"postID":', tmp_post$postID, '}')
        mongo(collection="posts", db=db, url = url)$update(postID, update=update_views)
       
       
       
#        tmp_comments <- table_comments_copy[tmp_post$postID==table_comments_copy$postID,]
#        tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
#        N_comments <- dim(tmp_comments)[1]
#        
       # prepare output$selectedPost for commenting
       wellPanel(
       h4(strong("App Name: "), tmp_post$post_name),
       p( strong("By: "), tmp_post$user_name, br(), br(),
         strong("Category: "), tmp_post$post_category,br(),
         strong("Description: "),tmp_post$post,br(), br(),
         strong("Views: "), tmp_post$cumulative_views, br(),
         strong("Comments:"), tmp_post$cumulative_comments, br(), 
         strong("Average Interest: "),tmp_post$average_interest, br(),
         strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
         strong("Edits:"), tmp_post$edits, br(),
         strong("Views since last edit: "), tmp_post$current_views, br(),
         strong("Comments since last edit:"), tmp_post$current_comments, br(),
          br(), 
         strong("<< Comments >> ")),
       
       retrieveComments(N_comments, tmp_comments)
       
#        # Insert previuos comments
#        lapply(1:N_comments, function(i) {
#          tmp_com_item  <- tmp_comments[i,] 
#          wellPanel(
#            p(tmp_com_item$comment,br(),
#              " - ",tmp_com_item$comment_user_name, " posted on ",
#              strtrim(tmp_com_item$timestamp2,10))
#          )
#        })
       )
       }
     })
     
     
     # Function to insert previuos comments
     retrieveComments <- function(N_comments, tmp_comments) {
          if (N_comments>0) {
          lapply(1:N_comments, function(i) {
               tmp_com_item  <- tmp_comments[i,] 
               wellPanel(
                    p(tmp_com_item$comment,br(),
                      " - ",tmp_com_item$comment_user_name, " posted on ",
                      strtrim(tmp_com_item$timestamp2,10))
               )
          })
          } else { 
               (p("Leave the first comment for this idea!"))
          }
     }    
      
         
     # Open up description for edit 
     observeEvent(input$edit, { 
       # authentication via google account
       rv$edit_auth <- TRUE
     })
     
     # ---------- Event: edit_send button ------------
     observeEvent(input$edit_send, {
#  not needed?       # Update the timestamp field to be the current time
#        updateTextInput(session, "timestamp", value = get_time_human())

#        # User-experience stuff
#        shinyjs::disable("post_send")
#        shinyjs::show("submitMsg")
#        shinyjs::hide("error")
#        on.exit({
#          shinyjs::enable("post_send")
#          shinyjs::hide("submitMsg")
#        })
          
       # move the old post and comments to archive tables
       "table_archive_posts" %>% gs_title %>% gs_add_row(input = rv$selectedPost)
       N_comments <- dim(rv$selectedComments)[1]
       if (N_comments>0) {
       tmp_archive_table_comments <- load_data_gsheets("table_archive_comments")
       loc_archive_comments <- dim(tmp_archive_table_comments)[1] + 1
       "table_archive_comments" %>% gs_title %>% gs_edit_cells(input = rv$selectedComments, trim=TRUE,
                                                               anchor=paste0("A",loc_archive_comments),
                                                               col_names=FALSE)
       # remove old comments 
       tmp_table_comments <- load_data_gsheets("table_comments")
       tmp_table_comments  <- tmp_table_comments[!(tmp_table_comments$commentID %in% rv$selectedComments$commentID),] 
       "table_comments" %>% gs_title %>% gs_edit_cells(input = tmp_table_comments, trim=TRUE, 
                                                       anchor="A2", col_names = FALSE)
       }
       
       # update the counters for edits, currrent_veiws, and current_comments
       tmp_post <- rv$selectedPost
       tmp_post$timestamp <- get_time_human()
       tmp_post$edits <- tmp_post$edits + 1
       tmp_post$current_views <- 0
       tmp_post$current_comments <- 0
       "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,
                                                    anchor=rv$post_location, col_names = FALSE)
                              

      
       
       #updateTabItems(session, "tabs","postTab")
       
#        # Save the data (show an error message in case of error)
#        tryCatch({
#          # save_data_gsheets(post_data(), "table_posts")
#          "table_posts" %>% gs_title %>% gs_add_row(input = post_data())
#          updateTabItems(session, "tabs","postTab")
#        },
#        error = function(err) {
#          shinyjs::text("errorMsg", err$message)
#          shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
#          shinyjs::logjs(err)
#        })
       rv$edit_auth <- FALSE
     })
     

     
     source("session_notification.R", local=TRUE)
     
     source("session_misc.R", local=TRUE)

})


