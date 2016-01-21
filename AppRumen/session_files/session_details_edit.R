
# ------------ Show Selected Post and Enable Edit process ----------------
# Prepare the display of a selectet post in Details
output$selectedPost  <- renderUI({ 
  # React to the change in rv$selected_post
  browser() 
  
  # User-experience stuff
  shinyjs::show("loadMsg")
  shinyjs::hide("details_contents")
  on.exit({
    shinyjs::hide("loadMsg")
    shinyjs::show("details_contents")
  })
  shinyjs::hide("view_archive_comments")
  
  need(!is.null(rv$selected_post), 'No post is selected.') %>% validate()
   
  tmp_post <- rv$selected_post

  if (!rv$edit_auth) {
  
  isolate({
  active_comments <- rv$selected_comments[rv$selected_comments$comment_status=="Active",] 
  active_comment_users_email <- active_comments$comment_email_address
  rv$N_comments <- dim(active_comments )[1]

  if (rv$N_comments>0) {
    rv$active_comments <- active_comments[order(active_comments$timestamp2),] 
    gen_post_links(active_comment_users_email,"comment_user")
  } else {
    rv$active_comments <-NULL
  }
  
  # Repeat for archive_comments
  archive_comments <- rv$selected_comments[rv$selected_comments$comment_status=="Archive",] 
  if (length(archive_comments)[1]>0) {
    rv$archive_comments <- archive_comments[order(archive_comments$timestamp2),] 
  } else {
    rv$archive_comments <- NULL
  }
  
  })
 
    # regular view without editing  
    field_postID <- paste0('{"postID":', tmp_post$postID,'}')
    update_views <- paste0('{"$set":{', '"current_views":', as.integer(tmp_post$current_views + 1),
                           ', "cumulative_views":', as.integer(tmp_post$cumulative_views + 1), '}}')
    
    mongo_posts$update(field_postID, update=update_views)
    
    gen_user_links(tmp_post$email_address, "post_user0", N=1)
    
    # Update "viewed"=1 on comments if the owner of the post is viewing
    if (!is.null(user_session$info$emailAddress) & length(active_comment_users_email)>0) {
      if (user_session$info$emailAddress==tmp_post$email_address) {
        mongo_comments$update(field_postID, '{"$set": {"viewed_by_owner":1}}', multiple=TRUE)
      }
    }
    
    # prepare output$selected_post for commenting
    wellPanel(
      h3(strong(tmp_post$post_name)),
      p( strong("By: "), actionButton("post_user01", tmp_post$user_name, "link"), br(),
         strong("Category: "), tmp_post$post_category,br(),
         strong("Description: "),tmp_post$post), br(),
      p(strong("Views: "), tmp_post$cumulative_views, br(),
         strong("Comments:"), tmp_post$cumulative_comments, br(), 
         strong("Likes: "), tmp_post$likes, br(),
         strong("Follower: "), tmp_post$n_followers, br(),
         strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
         strong("Edits:"), tmp_post$edits, br(),
         strong("Views since last edit: "), tmp_post$current_views, br(),
         strong("Comments since last edit:"), tmp_post$current_comments), br(), 
          insert_edit("edit",
                  tmp_post$email_address, user_session$info$emailAddress),  
         br(),  
         strong("<< Comments >> "), 
       
         retrieveComments(rv$N_comments, rv$active_comments)
    )
   
  } else { 
    #  prepare output$selected_post for editing
    wellPanel( 
      textInput("post_name_ed", "App Name", value = tmp_post$post_name),
      
      p( strong("By: "), tmp_post$user_name), br(),
      selectInput("post_category_ed","Category", selected=tmp_post$post_category,
                  choices=vars_category),
      h5(strong("Description")), 
      inputTextarea('post_ed', value= tmp_post$post,20,50), 
      tags$head(tags$style(type="text/css", "#post_ed {border-color: #C0C0C0}")),
      br(), 
      p(strong("Views: "), tmp_post$cumulative_views, br(),
        strong("Comments:"), tmp_post$cumulative_comments, br(), 
        strong("Likes: "), tmp_post$likes, br(),
        strong("Follower: "), tmp_post$n_followers, br(),
        strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
        strong("Edits:"), tmp_post$edits, br(),
        strong("Views since last edited: "), tmp_post$current_views, br(),
        strong("Comments since last edited:"), tmp_post$current_comments, br(),
        br(), 
        strong("<< Comments >> ")
      ),
      
      retrieveComments(rv$N_comments, rv$active_comments),
      selectInput("decision","Decision",choices=c("Continue editing"="c1", "Label Completed"="c2",
                                                  "Label Resolved"="c3", "Label Discontinued"="c4")),
      sliderInput("completeness","Degree of Completion",min=0,step=5,value=tmp_post$completeness,max=100), 
      actionButton("edit_send","Update","primary")
    ) 
  }
})  


output$selectedArchiveComments <- renderUI({
  N <- dim(rv$archive_comments)[1]
  if (!is.null(N_archive_comments)) {
    n <- min(input$n_archive_comments, N)
    archive_comment_users_email <- rv$archive_comments$comment_email_address
    gen_post_links(archive_comment_users_email,"archive_comment_user", n)
    
    div(retrieveComments(N, rv$archive_comments[1:N,],archive=TURE)) 
  } else {
    return(p("There is no archive comment."))
  }
})
  
  

# Open up description for edit 
observeEvent(input$edit, { 
  shinyjs::hide("show_comment_box")
  rv$edit_auth <- TRUE
})

# ---------- Event: edit_send button ------------
observeEvent(input$edit_send, { 

         # User-experience stuff
         shinyjs::disable("post_send")
         shinyjs::show("submitMsg")
         shinyjs::hide("error")
         on.exit({
           shinyjs::enable("post_send")
           shinyjs::hide("submitMsg")
         })
  
         tmp_post <- rv$selected_post
         tmp_post$status <- "Archive"
         
  # Change the status of the old post and comments to archive 
  mongo_posts$insert(tmp_post)
  
  active_comments <- rv$selected_comments[rv$selected_comments$comment_status=="Active",] 
  N_comments <- dim(active_comments)[1]
 
   com_list <- c()
   if (N_comments>0) {
    for (i in 1:N_comments) {
      ifelse(i<N_comments, 
             com_list <- paste0(com_list, active_comments$commentID[i],", "),
             com_list <- paste0(com_list, active_comments$commentID[i])
             )
    }
     field_commentID <- paste0('{"commentID": {"$in": [', com_list, ']}}')
     update_status <-  paste0('{"$set" : { "comment_status": "Archive"}}')
     mongo_comments$update(field_commentID, update_status)
     
  }
  
  field_postID <- paste0('{"postID":', tmp_post$postID, '}')
  
  update_post <- function(status) {
    paste0('{"$set":{', 
           '"timestamp":','"', get_time_human(), '"',
           ', "status":',  '"', status, '"',
           ', "post_name":',  '"', input$post_name_ed, '"',
           ', "post_category":',  '"', input$post_category_ed, '"', 
           ', "edits":', as.integer(tmp_post$edits + 1), 
           ', "current_views":', 0, 
           ', "current_comments":', 0, 
           ', "post":', '"', input$post_ed, '"',
           '}}')
  }
  
  if (input$decision=="c1") {
  # Continue editing
      mongo_posts$update(field_postID, update= update_post("Active"))
  } else if (input$decision=="c2") { 
  # Completed
    mongo_posts$update(field_postID, update= update_post("Completed"))
  } else if (input$decision=="c3") { 
    # Resolved
    mongo_posts$update(field_postID, update= update_post("Resloved"))
  } else { 
    # Discontinued
    mongo_posts$update(field_postID, update= update_post("Discontinued"))
  }
 
  shinyjs::show("show_comment_box") 
  rv$edit_auth <- FALSE 
}) 

