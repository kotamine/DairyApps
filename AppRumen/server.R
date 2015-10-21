library(shiny)
library(shinyBS)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(dplyr))
library(magrittr)

source("helpers.R")


shinyServer(function(input, output, session) {
     # Give an initial value to the timestamp field
     updateTextInput(session, "timestamp", value = get_time_human())
     updateTextInput(session, "postID", value = get_time_epoch())
  
     rv <- reactiveValues(view = NULL, edit_auth = FALSE, 
                          selectedPost = NULL, selectedComments = NULL)
     
     # --------------- initializing the setup --------------------
     # Get keys for google sheets  
     ( KEY_1 <- "1d4tp2eMxs4u0UgQmiCNSM5Md3J0d2TUnrl6VlEciU5o")
     ( KEY_2 <- "1dpMa59iRabMR1c0Q6nu5U8g2Um9IMBFcpCmmCHu-4WE")
     ( KEY_3 <- "1HbE_bJDoe5t90mKAkbw8cRaLfOcVitwj76glZQ2R41o")
     ( KEY_4 <- "19vNaXeUjkYsKMOvCQXsUl29kR6xNTgDMIpeAXR_Ihb0") 
     ( KEY_5 <- "1irfY3pIOCFupFeBAXErj1_fDtUnh2heZc3WvCoVBkxg") 
     
     third_party_key_1 <- KEY_1 %>%  gs_key()
     third_party_key_2 <- KEY_2 %>%  gs_key()
     third_party_key_3 <- KEY_3 %>%  gs_key()
     third_party_key_4 <- KEY_4 %>%  gs_key()
     third_party_key_5 <- KEY_5 %>%  gs_key()
     
      # Enable the Submit button when all mandatory fields are filled out
     observe({
          fields_post_filled <-
               fields_post_mandatory %>%
               sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
               all
          shinyjs::toggleState("post_send", fields_post_filled)
          
          fields_comment_filled <-
               fields_comment_mandatory %>%
               sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
               all
          shinyjs::toggleState("comment_send", fields_comment_filled)
          
     })
    
     # Gather all the form inputs
     post_data <- reactive({
          sapply(fields_post, function(x) x = input[[x]])
     })
     
     comment_data <- reactive({
          sapply(fields_comment, function(x) x = input[[x]])
     })
     
    # disable email_address in Post
     shinyjs::toggleState("email_address", FALSE)
     user_session <- reactiveValues(info = NULL)
     output$userpanel <- renderUI({
       # session$user is non-NULL only in authenticated sessions
       if (!is.null(user_session$info$token_valid)) {
         sidebarUserPanel(
           span("Welcome ", user_session$info$displayName),
           subtitle = actionButton("log_out", "Logout","link")) 
       } else {
         sidebarUserPanel(span("   ",icon("google"),  
         actionButton("log_in","Login","link")))
       } 
     }) 
     
     observe({
       if (is.null(user_session$info$token_valid)) {
         tmp_edit <- FALSE
       }
       else {
         if (user_session$info$emailAddress == rv$selectedPost$email_address) {
           tmp_edit <- TRUE
         } else {
           tmp_edit <- FALSE
         }
       }
       shinyjs::toggleState("edit", tmp_edit)
     })
     
     observeEvent(input$gmail1, {
          # Give googlesheets permission to access your spreadsheets and google drive
          gs_auth( new_user = TRUE)
          user_session$info <- gs_user()
          browser()
          updateTextInput(session, "user_name", value = user_session$info$displayName)
          updateTextInput(session, "email_address", value = user_session$info$emailAddress)
          updateTextInput(session, "comment_user_name", value = user_session$info$displayName)
          updateTextInput(session, "comment_email_address", value =  user_session$info$emailAddress)
     })
     
     observeEvent(input$gmail2, {
          # Give googlesheets permission to access your spreadsheets and google drive
          gs_auth( new_user = TRUE)
          user_session_info <- gs_user()
          updateTextInput(session, "user_name", value = user_session_info$displayName)
          updateTextInput(session, "email_address", value =  user_session_info$emailAddress)
          updateTextInput(session, "comment_user_name", value = user_session_info$displayName)
          updateTextInput(session, "comment_email_address", value =  user_session_info$emailAddress)
     })
     system_use <- gs_title("system_use")
     
     messageData <- system_use  %>% gs_read_csv(ws="messageData")
     #messageData <-  load_data_gsheets("system_use", ws="messageData")
          
     output$messageMenu <- renderMenu({
       # Code to generate each of the messageItems here, in a list. This assumes
       # that messageData is a data frame with two columns, 'from' and 'message'.
       loc_messageData <-  messageData 
        msgs <- apply(loc_messageData, 1, function(row) {
         messageItem(from = row[["from"]], message = row[["message"]])
       })
       
       # This is equivalent to calling:
       #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
       dropdownMenu(type = "messages", .list = msgs)
     })
     
     output$taskMenu <- renderMenu({
     dropdownMenu(type = "tasks", badgeStatus = "success",
                  taskItem(value = 90, color = "green",
                           "Documentation"
                  ),
                  taskItem(value = 17, color = "aqua",
                           "Project X"
                  ),
                  taskItem(value = 75, color = "yellow",
                           "Server deployment"
                  ),
                  taskItem(value = 80, color = "red",
                           "Overall project"
                  )
     )
     })
     
     output$notificationMenu <- renderMenu({ 
     dropdownMenu(type = "notifications",
                  notificationItem(
                    text = "5 new users today",
                    icon("users")
                  ),
                  notificationItem(
                    text = "12 items delivered",
                    icon("truck"),
                    status = "success"
                  ),
                  notificationItem(
                    text = "Server load at 86%",
                    icon = icon("exclamation-triangle"),
                    status = "warning"
                  )
     )
     })
     
     # When the post_send button is clicked 
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
     
     # When the comment_send button is clicked 
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
     

     # Update the tables whenever a new submission is made 
     table_posts <- reactive({
          input$post_send
          load_data_gsheets("table_posts")
     })
     
     table_comments <- reactive({
          input$comment_send
          load_data_gsheets("table_comments")
     })
     
     table_archive_posts <- reactive({
          input$edit_send
          load_data_gsheets("table_archive_posts")
     })
     
     table_archive_comments <- reactive({
          input$edit_send
          load_data_gsheets("table_archive_comments")
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
     
     
     # obtain initial copies of tables for post-processing
     table_posts_copy <- gs_title("table_posts")  
     table_posts_copy <- gs_read_csv(table_posts_copy)  
     N <- dim(table_posts_copy)[1]
     
     table_comments_copy <- gs_title("table_comments")  
     table_comments_copy <- gs_read_csv(table_comments_copy)  
     
     # Prepare postings in Posts
     output$postboxes <- renderUI({
       table_posts_copy <- load_data_gsheets("table_posts")
       table_posts_copy <-  table_posts_copy[ table_posts_copy$post_category %in% input$filterCategory,]
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
           # helpText(paste(table_posts_copy[i,])),
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
     
     # ------------ Show Selected Post and Enable Edit process ----------------
     # Prepare the display of a selectet post in Details
     output$selectedPost  <- renderUI({
          validate(
               need(!is.null(rv$view), 'No post is selected.')
          )
          # obtain a new copy of table (may be updated from some othere sessions)
         # table_posts_copy <- table_posts() 
          table_posts_copy <- load_data_gsheets("table_posts")
          table_comments_copy <- load_data_gsheets("table_comments") 
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
       tmp_location <- which(tmp_post$postID ==  table_posts_copy[,c("postID")] ) + 1
       rv$post_location <- paste0("A",tmp_location)
       tmp_post$current_views <- tmp_post$current_views + 1
       tmp_post$cumulative_views <- tmp_post$cumulative_views + 1
       "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,  
                                                    anchor=rv$post_location, col_names = FALSE)
       
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
     
     

     
#      output$selectedPost <- reactive({
#        tmp0 <- input$view1
#        tmp <- paste(substitute(input$view1))[3]
#        i <- as.integer(sub("view","",tmp))
#        browser()
#        paste(table_posts_copy[i,])
#      })
#      
#      output$selectedPost <- reactive({
#        tmp0 <- input$view2
#        tmp <- paste(substitute(input$view2))
#        i <- as.integer(sub("view","",tmp[3]))
#        browser()
#        
#        paste(table_posts_copy[i,])
#      })
#      
#      output$selectedPost <- reactive({
#        tmp0 <- input$view3
#        tmp <- paste(substitute(input$view3))
#        i <- as.integer(sub("view","",tmp[3]))
#        browser()
#        paste(table_posts_copy[i,])
#      })
#      
   #  browser()
     
#      # Allow user to download responses
#      output$downloadBtn <- downloadHandler(
#           filename = function() { 
#                paste0(TABLE_NAME, "_", input$storage, "_", get_time_human(), '.csv')
#           },
#           content = function(file) {
#                write.csv(responses_data(), file, row.names = FALSE)
#           }
#      )
#      

#      
#   
#   gap <- gap %>% gs_gs()
#   
#   oceania <- gap %>% gs_read_csv(ws = "Oceania")
#   oceania
#   
#   browser()   
 
#     gap_xlsx <- gs_upload(system.file("mini-gap.xlsx", package = "googlesheets"))
# #   gs_title("Gapminder") %>% 
# #     gs_download(to = "gap_xlsx.xlsx")
#   iris %>%
#     head(5) %>%
#     write.csv("iris.csv", row.names = FALSE)
#   iris_ss <- gs_upload("iris.csv")
#   
#   foo <- gs_new("foo")
#   foo <- foo %>% gs_ws_new("add_row")
#   foo <- foo %>% 
#     gs_add_row(ws = "add_row", input = head(iris, 1), trim = TRUE)
#   for(i in 2:6) {
#     foo <- foo %>% gs_add_row(ws = "add_row", input = iris[i, ])
#     Sys.sleep(0.3)
#   }
#   
#   Gapminder %>% gs_add_row(ws = "Africa", input = head(africa, 1))
#   

})


