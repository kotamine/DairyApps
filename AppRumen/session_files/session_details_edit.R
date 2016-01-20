
# ------------ Show Selected Post and Enable Edit process ----------------
# Prepare the display of a selectet post in Details
output$selectedPost  <- renderUI({ 
  # Acts as a trigger when the user is viewing
  rv$back_to_selected_post 
  rv$notice_comment_postID
  rv$notice_progress_postID
  rv$notice_follow_postID
  browser() 
  
  
  # User-experience stuff
  shinyjs::show("loadMsg")
  shinyjs::hide("details_contents")
  on.exit({
    shinyjs::hide("loadMsg")
    shinyjs::show("details_contents")
  })
  shinyjs::hide("view_archive_comments")
  
  isolate({
  
  if (rv$post_trafic=="notice_comment") {
    field_postID <- paste0('{"postID":', rv$notice_comment_postID,'}') 
  } else if (rv$post_trafic=="notice_progress") {
    field_postID <- paste0('{"postID":', rv$notice_progress_postID,'}') 
  } else if (rv$post_trafic=="notice_follow") {
    field_postID <- paste0('{"postID":', rv$notice_follow_postID,'}') 
  } else { 
    field_postID <- paste0('{"postID":', rv$active_postsID[rv$view],'}')
  }
  rv$post_trafic <- "NA"
      
  rv$selected_post <-  mongo_posts$find(field_postID)
  rv$selected_post_id <-  rv$active_postsID[rv$view]
  
  if ( dim(rv$selected_post)[1] ==0)  { rv$selected_post <- NULL }

  validate(
    need( !is.null(rv$selected_post), 'No post is selected.')
  ) 
  
  tmp_post <- rv$selected_post
  
  
  
  rv$selected_comments <- mongo_comments$find(field_postID)
  tmp_comments <- rv$selected_comments 
  rv$active_comment_users_email <- tmp_comments$comment_email_address
  
  rv$user_trafic <- "post"
  
  N_comments <- dim(tmp_comments)[1]

  if (N_comments>0) {
  tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
  
  lapply(c(1:N_comments), function(x) {
    observeEvent(input[[paste0("comment_user",x)]], ({
      browser()
      rv$view_comment_user <- x
      rv$user_trafic <- "comment"
      # Update "Details" panel via trigger "rv$back_to_selected_user"  
      rv$back_to_selected_user <- rv$back_to_selected_user + 1
      updateTabItems(session, "tabs","peopleTab")
      updateCollapse(session, "collapsePeople", open = "Details")
    }))
  })
  }
  
  # Update "viewed"=1 on comments if the owner of the post is viewing
  if (!is.null(user_session$info$emailAddress) & N_comments>0) {
     if (user_session$info$emailAddress==tmp_post$email_address) {
    mongo_comments$update(field_postID, '{"$set": {"viewed":1}}', multiple=TRUE)
     }
  }
  
  # Repeat for archive_comments
  rv$selectedArchiveComments <- mongo_archive_comments$find(field_postID)
  tmp_archive_comments <- rv$selectedArchiveComments

  N_archive_comments <- dim(tmp_archive_comments)[1]
  if (N_archive_comments>0) {
    rv$tmp_archive_comments <- tmp_archive_comments[order(tmp_archive_comments$timestamp2),]
    
    lapply(c(1:N_archive_comments), function(x) {
      observeEvent(input[[paste0("archive_comment_user",x)]], ({
        rv$view_archive_comment_user <- x
        rv$user_trafic <- "archive_comment" 
        # Update "Details" panel via trigger "rv$back_to_selected_user"  
        rv$back_to_selected_user <- rv$back_to_selected_user + 1
        updateTabItems(session, "tabs","peopleTab")
        updateCollapse(session, "collapsePeople", open = "Details")
      })
     )
    })
    
  } else {
    rv$tmp_archive_comments <- NULL
  }
  
  }) 
  
  
  if (!rv$edit_auth) {
    # regular view without editing  

    update_views <- paste0('{"$set":{', '"current_views":', as.integer(tmp_post$current_views + 1),
                           ', "cumulative_views":', as.integer(tmp_post$cumulative_views + 1), '}}')
    
    mongo_posts$update(field_postID, update=update_views)
    
    
    # prepare output$selected_post for commenting
    wellPanel(
      h3(strong(tmp_post$post_name)),
      p( strong("By: "), actionButton(inputId = "post_user", tmp_post$user_name, "link"), br(),
         strong("Category: "), tmp_post$post_category,br(),
         strong("Description: "),tmp_post$post,br(), br(),
         strong("Views: "), tmp_post$cumulative_views, br(),
         strong("Comments:"), tmp_post$cumulative_comments, br(), 
         strong("Average Interest: "),round(tmp_post$average_interest,2), br(),
         strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
         strong("Edits:"), tmp_post$edits, br(),
         strong("Views since last edit: "), tmp_post$current_views, br(),
         strong("Comments since last edit:"), tmp_post$current_comments, br(),
         br(), 
         strong("<< Comments >> ")),
      
      retrieveComments(N_comments, tmp_comments)
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
        strong("Average Interest: "),round(tmp_post$average_interest,2), br(),
        strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
        strong("Edits:"), tmp_post$edits, br(),
        strong("Views since last edited: "), tmp_post$current_views, br(),
        strong("Comments since last edited:"), tmp_post$current_comments, br(),
        br(), 
        strong("<< Comments >> ")
      ),
      
      retrieveComments(N_comments, tmp_comments),
      selectInput("decision","Decision",choices=c("Continue editing"="c1", "Move it Completed Posts"="c2",
                                                  "Move to Resolved Posts"="c3", "Move to Discontinued Posts"="c4")),
      sliderInput("completeness","Degree of Completion",min=0,step=5,value=5,max=100), 
      actionButton("edit_send","Update","primary")
    ) 
  }
}) 


output$selectedArchiveComments <- renderUI({
  N_archive_comments <- dim(rv$tmp_archive_comments)[1]
  if (!is.null(N_archive_comments)) {
    N <- min(input$n_archive_comments,N_archive_comments)
    div(retrieveComments(N, rv$tmp_archive_comments[1:N,])) 
  } else {
    return()
  }
})
  

observeEvent(input$post_user, {
    rv$view_user <- rv$view 
    # Update "Details" panel via trigger "rv$back_to_selected_user"  
    rv$back_to_selected_user <- rv$back_to_selected_user + 1
    updateTabItems(session, "tabs","peopleTab")
    updateCollapse(session, "collapsePeople", open = "Details")
})
  

# Open up description for edit 
observeEvent(input$edit, { 
  # authentication via google account
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
  
  
  # move the old post and comments to archive tables
  mongo_archive_posts$insert(rv$selected_post)
  
  N_comments <- dim(rv$selected_comments)[1]
  if (!is.null(N_comments)) {
    mongo_archive_comments$insert(rv$selected_comments)
    
    # remove old comments 
    field_postID <- paste0('{"postID":', tmp_post$postID, '}')
    mongo_comments$remove(field_postID, multiple = TRUE) 
    
  }

  if (input$decision=="c1") {
  # Continue editing
    update_edit <- paste0('{"$set":{', 
                        '"timestamp":','"', get_time_human(), '"',
                        ', "post_name":',  '"', input$post_name_ed, '"',
                        ', "post_category":',  '"', input$post_category_ed, '"', 
                        ', "edits":', as.integer(tmp_post$edits + 1), 
                        ', "current_views":', 0, 
                        ', "current_comments":', 0, 
                        ', "post":', '"',input$post_ed, '"',
                        '}}')
  
  mongo_posts$update(field_postID, update=update_edit)
  
  } else { 
    # Decision to move to: Completed, Resolved, or Discontinued
    new_row <- tmp_post
    new_row$timestamp <- get_time_human() 
    new_row$edits <- as.integer(new_row$edits + 1)
    new_row$post_name_ed <- input$post_name_ed
    new_row$post_category_ed <- input$post_category_ed
    new_row$current_views <- 0
    new_row$current_comments <- 0
    new_row$post <- input$post_ed
    
    if (input$decision=="c2") {
      
      tryCatch({
        mongo_completed_posts$insert(new_row) 
        updateTabItems(session, "tabs","completedTab") 
      })
      
    } else if (input$decision=="c3") { 
      
      tryCatch({
        mongo_resolved_posts$insert(new_row) 
        updateTabItems(session, "tabs","resolvedTab") 
      })
      
    } else {
      tryCatch({
        mongo_discontinued_posts$insert(new_row) 
        updateTabItems(session, "tabs","discontinuedTab") 
      })
    }

    mongo_posts$remove(field_postID) 
    # Updating input$n_boxes triggers update of "Posts" panel 
    updateNumericInput(session,"n_boxes","Number of Posts", value=(input$n_boxes-1), min=0,step=5,max=100)
    updateCollapse(session, "collapseMain", open = "Posts")
  }
  
  rv$back_to_selected_post <- rv$back_to_selected_post + 1
  
  shinyjs::show("show_comment_box")
  rv$edit_auth <- FALSE
})

