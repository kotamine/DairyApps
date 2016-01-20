
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
    } else if (rv$user_trafic=="post") {
      field_userID <- paste0('{"email_address":', '"',rv$active_posts_email[rv$view_user],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',rv$active_posts_email[rv$view_user],'"','}')
    } else if (rv$user_trafic=="message")  {
      rv$user_trafic <- "NA"
      field_userID <- paste0('{"email_address":', '"',rv$active_senders_email[rv$view_sender],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',rv$active_senders_email[rv$view_sender],'"','}')
    } else {
      field_userID <- paste0('{"email_address":', '"',rv$active_users_email[rv$view_user],'"','}')
      field_userID_com <- paste0('{"comment_email_address":', '"',rv$active_users_email[rv$view_user],'"','}')
    }
    
    rv$selected_user <-  mongo_users$find(field_userID)  
    rv$selected_user_email <-  rv$selected_user$email_address
    
    if ( dim(rv$selected_user)[1] ==0)  { rv$selected_user <- NULL }
    validate( 
      need(!is.null(field_userID) & !is.null(rv$selected_user), 'No individual is selected.')
    )  
    
    tmp_user <- rv$selected_user[1,]
  }) 
  
    if (!rv$edit_user_auth) {
      # Render regular view without editing
      
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

    # Increase the view counter of user page
    update_views <- paste0('{"$set":{', '"profile_views":', as.integer(tmp_user$profile_views + 1), '}}')
    
    
    mongo_users$update(field_userID, update=update_views)
    
    
    # Prepare output$selected_user for commenting
    wellPanel(
      h3(strong(tmp_user$user_name)),
      p( strong("Profession: "), tmp_user$profession, br(), 
         strong("Interests: "), gsub(",",", ",tmp_user$interests),br(),
         strong("Location: "),tmp_user$location,br(),
         strong("LinkedIn: "), tmp_user$linked_in, br(),
         strong("About: "), tmp_user$about, br(), 
         br(),
         strong("User Stats:"), br(),
         strong("Profile Views: "),tmp_user$profile_views, br(),
         strong("Total Comments Made:"), tmp_user$comments, br(),
         strong("User Since:"), strtrim(tmp_user$timestamp,10),br(),
         strong("Last Logged In:"), strtrim(tmp_user$last_logged_in,10),br(),
         strong("Last Posted:"), strtrim(tmp_user$last_posted,10),br(),
         strong("Last Commented:"), strtrim(tmp_user$last_commented,10),br(),
         strong("Followers:"), strtrim(tmp_user$n_followers,10),br(),
         br(),
         strong("Post Stats: "), br(),
         strong("Total Posts: "),tmp_user$total_posts, br(),
         strong("Active: "),tmp_user$active_posts, br(),
         strong("Completed: "),tmp_user$completed_posts, br(),
         strong("Resolved: "),tmp_user$resolved_posts, br(),
         strong("Discontinued: "),tmp_user$discontinued_posts, br(),
         strong("Total Views: "),tmp_user$total_views, br(),
         strong("Total Comments Received:"), tmp_user$total_comments, br(),
         strong("Average Interest: "),round(tmp_user$average_interest,2), br(),
         strong("Followed Posts:"), strtrim(tmp_user$n_followed_posts,10),br(),
         br(),
         div(id="button_user_edit",
             fluidRow(column(5,
                actionButton("user_edit","Edit (author only)","primary"))
             ),
             br()
        )
      ))

  } else {
     # Prepare output$selectedPost for editing
  
   vec_interests <- ifelse(is.null(tmp_user$interests), NULL,
                              strsplit(tmp_user$interests[[1]],",")) %>% unlist() %>% c()
  
    wellPanel(
      h3(strong(tmp_user$user_name)),
      p( selectInput("profession", "Profession", 
                   choices=vars_profession,
                   selected=tmp_user$profession), 
         checkboxGroupInput("interests", "Interests", 
                            choices=vars_interests,
                            selected =vec_interests, 
                            inline = TRUE),
         selectInput("location", "Location", 
                     choices=c("Minnesota",state.name[state.name!="Minnesota"], "Other"),
                     selected=tmp_user$location), br(),
         textInput("linked_in", "LinkedIn", value=tmp_user$linked_in), br(),
         strong("About: "), br(),
         inputTextarea('about', value=tmp_user$about,20,50), br(), 
         tags$head(tags$style(type="text/css", "#about {border-color: #C0C0C0}")),
         br(),br(), 
      actionButton("user_edit_send","Update","primary")
    )
    )
  }
})




# Open up description for edit 
observeEvent(input$user_edit, { 
  # authentication via google account
  # shinyjs::hide("show_comment_box")
  rv$edit_user_auth <- TRUE
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

  tmp_user <- rv$selected_user[1,]
  str_interests <- input$interests[1]
  lapply(seq_along(input$interests)[-1], 
         function(i) str_interests <<- paste0(str_interests,",", input$interests[i]))
  
  tag_interests <- input$interests[1]
  lapply(seq_along(input$interests)[-1], 
         function(i) tag_interests <<- paste0(tag_interests,": ", input$interests[i]))
  
  field_userID <- paste0('{"email_address":', '"', tmp_user$email_address, '"', '}')
  
  update_edit <- paste0('{"$set":{',
                          '  "profession":',  '"', input$profession, '"',
                          ', "interests":',  '["', str_interests, '"]',
                          ', "location":',  '"', input$location, '"',
                          ', "linked_in":', '"', input$linked_in, '"',
                          ', "about":', '"', input$about, '"', 
                          '}}')

  mongo_users$update(field_userID, update=update_edit)

  rv$back_to_selected_user <- rv$back_to_selected_user + 1

  rv$edit_user_auth <- FALSE
})



