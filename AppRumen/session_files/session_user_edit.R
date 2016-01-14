
# ------------ Show Selected User and Enable Edit process ----------------
# Prepare the display of a selectet post in User
output$selectedUser  <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$back_to_selected_user 
  
  browser()
  
#   # User-experience stuff
#   shinyjs::show("loadMsg")
#   shinyjs::hide("details_contents")
#   on.exit({
#     shinyjs::hide("loadMsg")
#     shinyjs::show("details_contents")
#   })
#   shinyjs::hide("view_archive_comments")
  
  
  isolate({
    
    if (rv$user_trafic=="comment") {
      field_userID <- paste0('{"email_address":', '"',rv$active_comment_users_email[rv$view_comment_user],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',rv$active_comment_users_email[rv$view_comment_user],'"','}')
      
    } else if (rv$user_trafic=="archive_comment") {
      field_userID <- paste0('{"email_address":', '"',
                             rv$active_archive_comment_users_email[rv$view_archive_comment_user],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',
                             rv$active_archive_comment_users_email[rv$view_archive_comment_user],'"','}')
    } else {
      field_userID <- paste0('{"email_address":', '"',rv$active_users_email[rv$view_user],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',rv$active_users_email[rv$view_user],'"','}')
    }

    rv$selectedUser <-  mongo_users$find(field_userID)  
    
    if ( dim(rv$selectedUser)[1] ==0)  { rv$selectedUser <- NULL }
    validate( 
      need(!is.null(field_userID) & !is.null(rv$selectedUser), 'No individual is selected.')
    )  
    
    tmp_user <- rv$selectedUser[1,]
    
    # Retrive info related to this user 
    tmp_list <- list(active_posts = mongo_posts$find(field_userID), 
                     comments = mongo_comments$find(field_userID_com),  
                     archive_posts = mongo_archive_posts$find(field_userID),
                     archive_comments = mongo_archive_comments$find(field_userID_com),
                     completed_posts = mongo_completed_posts$find(field_userID),
                     resolved_posts = mongo_resolved_posts$find(field_userID),
                     discontinued_posts = mongo_discontinued_posts$find(field_userID)
                     )
    
    all_post_names <- c("active_posts", "completed_posts","resolved_posts", "discontinued_posts")
    
    lapply(c(all_post_names, "comments"),
           function(item) tmp_user[[item]] <<- tmp_list[[item]] %>% nrow())  

    tmp_user$total_posts <- (tmp_user$active_posts + tmp_user$completed_posts + 
                              + tmp_user$resolved_posts +  tmp_user$discontinued_posts)
  
    tmp_user$total_posts <-  lapply(all_post_names, 
                                    function(item) tmp_user[[item]]) %>% unlist() %>% sum()
    
    tmp_user$total_views <- lapply(all_post_names, 
                                   function(item) tmp_list[[item]]$cumulative_views) %>% unlist() %>% sum()
    
    tmp_user$total_comments <- lapply(all_post_names, 
                                      function(item) tmp_list[[item]]$cumulative_comments) %>% unlist() %>% sum()
    
    tmp_user$average_interest <- lapply(all_post_names, 
                                        function(item) {
                                          tmp_list[[item]]$average_interest * tmp_list[[item]]$cumulative_comments
                                        }) %>% unlist() %>% sum() %>% div2(tmp_user$total_comments)
    
    tmp_user$last_posted <- lapply(all_post_names, 
                                   function(item) {
                                     tmp_list[[item]]$timestamp 
                                     }) %>% unlist() %>% sort2(decreasing=TRUE) %>% "["(1) 
    
    tmp_user$last_commented <- lapply(c("comments","archive_comments"), 
                                      function(item) {
                                        tmp_list[[item]]$timestamp2 
                                      }) %>% unlist() %>% sort2(decreasing=TRUE) %>% "["(1) 

  })
  
#   if (!rv$edit_user_auth) {
#     # regular view without editing  
#     
    # Increase the view counter of user page
    update_views <- paste0('{"$set":{', '"profile_views":', as.integer(tmp_user$profile_views + 1), '}}')
    mongo_users$update(field_userID, update=update_views)
    
    # prepare output$selectedUser for commenting
    wellPanel(
      h3(strong(tmp_user$user_name)),
      p( strong("Profession: "), tmp_user$profession, br(), 
         strong("Interests: "),tmp_user$interests,br(),
         strong("LinkedIn: "), tmp_user$linkedin, br(),
         strong("About: "), tmp_user$about, br(), 
         br(),
         strong("Stats of User"), br(),
         strong("Profile Views: "),tmp_user$profile_views, br(),
         strong("Total Comments Made:"), tmp_user$comments, br(),
         strong("User Since:"), strtrim(tmp_user$timestamp,10),br(),
         strong("Last Logged In:"), strtrim(tmp_user$last_logged_in,10),br(),
         strong("Last Posted:"), strtrim(tmp_user$last_posted,10),br(),
         strong("Last Commented:"), strtrim(tmp_user$last_commented,10),br(),
         strong("Followers:"), strtrim(tmp_user$n_followers,10),br(),
         br(),
         strong("Stats of Posts: "), br(),
         strong("Total Posts: "),tmp_user$total_posts, br(),
         strong("Active: "),tmp_user$active_posts, br(),
         strong("Completed: "),tmp_user$completed_posts, br(),
         strong("Resolved: "),tmp_user$resolved_posts, br(),
         strong("Discontinued: "),tmp_user$discontinued_posts, br(),
         strong("Total Views: "),tmp_user$total_views, br(),
         strong("Total Comments Received:"), tmp_user$total_comments, br(),
         strong("Average Interest: "),round(tmp_user$average_interest,2),
         strong("Followed Posts:"), strtrim(tmp_user$n_followed_posts,10),br(),
         br(),br(), 
         actionButton("filter_user", "Filter Posts by this User")
      )
      )
#     
#   } else {   
#     #  prepare output$selectedPost for editing
# #     wellPanel( 
# #       textInput("post_name_ed", "App Name", value = tmp_post$post_name),
# #       
# #       p( strong("By: "), tmp_post$user_name), br(),
# #       selectInput("post_category_ed","Category", selected=tmp_post$post_category,
# #                   choices=c("Milk","Forage","Labor","Social")),
# #       h5(strong("Description")), 
# #       inputTextarea('post_ed', value= tmp_post$post,20,50), 
# #       tags$head(tags$style(type="text/css", "#post_ed {border-color: #C0C0C0}")),
# #       br(), 
# #       p(strong("Views: "), tmp_post$cumulative_views, br(),
# #         strong("Comments:"), tmp_post$cumulative_comments, br(), 
# #         strong("Average Interest: "),round(tmp_post$average_interest,2), br(),
# #         strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
# #         strong("Edits:"), tmp_post$edits, br(),
# #         strong("Views since last edited: "), tmp_post$current_views, br(),
# #         strong("Comments since last edited:"), tmp_post$current_comments, br(),
# #         br(), 
# #         strong("<< Comments >> ")
# #       ),
# #       
# #       retrieveComments(N_comments, tmp_comments),
# #       selectInput("decision","Decision",choices=c("Continue editing"="c1", "Move it Completed Posts"="c2",
# #                                                   "Move to Resolved Posts"="c3", "Move to Discontinued Posts"="c4")),
# #       sliderInput("completeness","Degree of Completion",min=0,step=5,value=5,max=100), 
# #       actionButton("edit_send","Update","primary")
#     # ) 
#   }
})   




# Open up description for edit 
observeEvent(input$user_edit, { 
  # authentication via google account
  # shinyjs::hide("show_comment_box")
  rv$user_edit_auth <- TRUE
})

# ---------- Event: edit_send button ------------
observeEvent(input$user_edit_send, {
  
#   # User-experience stuff
#   shinyjs::disable("post_send")
#   shinyjs::show("submitMsg")
#   shinyjs::hide("error")
#   on.exit({
#     shinyjs::enable("post_send")
#     shinyjs::hide("submitMsg")
#   })
  
  return()
#   
#   tmp_user <- rv$selectedUser
#   
#   
#   # move the old post and comments to archive tables
#   mongo_archive_posts$insert(rv$selectedPost)
#   
#   N_comments <- dim(rv$selectedComments)[1]
#   if (!is.null(N_comments)) {
#     mongo_archive_comments$insert(rv$selectedComments)
#     
#     # remove old comments 
#     field_postID <- paste0('{"postID":', tmp_post$postID, '}')
#     mongo_comments$remove(field_postID, multiple = TRUE) 
#     
#   }
#   
#   if (input$decision=="c1") {
#     # Continue editing
#     update_edit <- paste0('{"$set":{', 
#                           '"timestamp":','"', get_time_human(), '"',
#                           ', "post_name":',  '"', input$post_name_ed, '"',
#                           ', "post_category":',  '"', input$post_category_ed, '"', 
#                           ', "edits":', as.integer(tmp_post$edits + 1), 
#                           ', "current_views":', 0, 
#                           ', "current_comments":', 0, 
#                           ', "post":', '"',input$post_ed, '"',
#                           '}}')
#     
#     mongo_posts$update(field_postID, update=update_edit)
#     
#   } else { 
#     # Decision to move to: Completed, Resolved, or Discontinued
#     new_row <- tmp_post
#     new_row$timestamp <- get_time_human() 
#     new_row$edits <- as.integer(new_row$edits + 1)
#     new_row$post_name_ed <- input$post_name_ed
#     new_row$post_category_ed <- input$post_category_ed
#     new_row$current_views <- 0
#     new_row$current_comments <- 0
#     new_row$post <- input$post_ed
#     
#     if (input$decision=="c2") {
#       
#       tryCatch({
#         mongo_completed_posts$insert(new_row) 
#         updateTabItems(session, "tabs","completedTab") 
#       })
#       
#     } else if (input$decision=="c3") { 
#       
#       tryCatch({
#         mongo_resolved_posts$insert(new_row) 
#         updateTabItems(session, "tabs","resolvedTab") 
#       })
#       
#     } else {
#       tryCatch({
#         mongo_discontinued_posts$insert(new_row) 
#         updateTabItems(session, "tabs","discontinuedTab") 
#       })
#     }
#     
#     mongo_posts$remove(field_postID) 
#     # Updating input$n_boxes triggers update of "Posts" panel 
#     updateNumericInput(session,"n_boxes","Number of Posts", value=(input$n_boxes-1), min=0,step=5,max=100)
#     updateCollapse(session, "collapseMain", open = "Posts")
#   }
#   
#   rv$back_to_selected_post <- rv$back_to_selected_post + 1
#   
#   shinyjs::show("show_comment_box")
#   rv$edit_auth <- FALSE
})



