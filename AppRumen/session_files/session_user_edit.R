
# ------------ Show Selected User and Enable Edit process ----------------
# Prepare the display of a selectet post in User
output$selectedUser  <- renderUI({ 
  # React to the change in rv$selected_user
  browser()
  
   need(!is.null(rv$selected_user), 
       'No individual is selected.') %>% validate()
   
   need(nrow(rv$selected_user)>0, 
        'No individual is selected.') %>% validate()
   
    tmp_user <- rv$selected_user
  
    field_userID <- paste0('{"email_address":', '"',tmp_user$email_address,'"','}')
    field_userID_com <- paste0('{"comment_email_address":', 
                               '"',tmp_user$comment_users_email,'"','}')
  
    if (!rv$edit_user_auth) {
    # Render regular view without editing
      
    # Retrive info related to this user 
      
    posts0 <- mongo_posts$find(field_userID)
    posts <- posts0[posts0$status!="Archive",]
    
    comments <-mongo_comments$find(field_userID_com)

    # tmp_user$total_posts <-  dim(posts)[1]
   
    tmp_user$total_views <- sum(posts$cumulative_views)
    
    tmp_user$total_comments_received <- sum(posts$cumulative_comments) 
    
    tmp_user$last_posted <- posts$timestamp %>% sort2(decreasing=TRUE) %>% "["(1) 
    
    # tmp_user$total_comments_made <-  dim(comments)[1]
    
    tmp_user$last_commented <- comments$timestamp2 %>% sort2(decreasing=TRUE) %>% "["(1) 
    
    
    active <- posts$status=="Active"
    completed <- posts$status=="Completed"
    resolved <- posts$status=="Resolved"
    
    # Increase the view counter of user page
    update_views <- paste0('{"$set":{', '"profile_views":', as.integer(tmp_user$profile_views + 1), '}}')
  
    mongo_users$update(field_userID, update=update_views)

    # Prepare output$selected_user for commenting
    wellPanel(
      h3(strong(tmp_user$user_name)),
      p( strong("Profession: "), tmp_user$profession, br(), 
         strong("Interests: "), gsub(",",", ",tmp_user$interests),br(),
         strong("Location: "), tmp_user$location,br(),
         strong("LinkedIn: "), tmp_user$linked_in, br(),
         strong("About Me: "), tmp_user$about, br(),
         br(),
         strong("Stats:"), br(),
         strong("Profile Views: "),tmp_user$profile_views, br(),
         strong("Total Comments Made:"), tmp_user$total_comments_made, br(),
         strong("User Since:"), strtrim(tmp_user$timestamp,10),br(),
         # strong("Last Logged In:"), strtrim(tmp_user$last_logged_in,10),br(),  # CREATE USER LOG ?
         strong("Last Posted:"), strtrim(tmp_user$last_posted,10),br(),
         strong("Last Commented:"), strtrim(tmp_user$last_commented,10),br(), 
         strong("Followers:"), strtrim(tmp_user$n_followers,10),br(),
         br(),
         strong("Posts: "), br(),
         strong("Total Posts: "),tmp_user$total_posts, br(),
         strong("Total Views: "), tmp_user$total_views), br(),
         # LIST THE POST NAMES HERE
         strong("Active: "), list_post_links(posts$post_name[active],
                                             posts$postID[active], "user_post"), br(),
         strong("Completed: "), list_post_links(posts$post_name[completed],
                                                posts$postID[completed], "user_post"), br(),
         strong("Resolved: "), list_post_links(posts$post_name[resolved],
                                               posts$postID[resolved], "user_post"), br(),
         insert_edit("user_edit",
                  tmp_user$email_address, user_session$info$emailAddress)
      )  
  } else { 
     # Prepare output$selectedUser for editing
  
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
         strong("About Me: "), br(),
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
  rv$edit_user_auth <- TRUE
})

# ---------- Event: edit_send button ------------
observeEvent(input$user_edit_send, {
  
  tmp_user <- rv$selected_user
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

  rv$edit_user_auth <- FALSE
})



