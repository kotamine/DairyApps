# ---------- Event: post_send button ------------
observeEvent(input$post_send, {
  
  # User-experience stuff
  shinyjs::disable("post_send")
  shinyjs::show("submitMsg")
  shinyjs::hide("error")
  on.exit({
    shinyjs::enable("post_send")
    shinyjs::hide("submitMsg")
  })
  
  rv$postID <- as.integer(get_time_epoch())
  rv$timestamp <- get_time_human()
  rv$timestamp_comment <- 0 
  new_row <- row_inputs(fields_post)
  
  # Add a row to the data (show an error message in case of error)
  tryCatch({
    mongo_posts$insert(new_row) 
    updateTabItems(session, "tabs","mainTab") 
  },
  
  error = function(err) {
    shinyjs::text("errorMsg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  })
  
  updateTextInput(session, "post_name","Suggested App Name", value="")
  updateSelectInput(session, "post_category","Category", 
              choices=c("Milk","Forage","Labor","Social"))
  
  rv$post_reset  <- rv$post_reset + 1
  rv$back_to_active_post <- rv$back_to_active_post + 1
  updateNumericInput(session,"n_boxes","Number of Posts", value=(input$n_boxes+1), min=0,step=5,max=100)
  updateCollapse(session, "collapseMain", open = "Posts")
})



# ---------- Event: comment_send button ------------
observeEvent(input$comment_send, {
 
  # User-experience stuff
  shinyjs::disable("comment_send")
  shinyjs::show("submitMsg2")
  shinyjs::hide("error2")
  on.exit({
    shinyjs::enable("comment_send")
    shinyjs::hide("submitMsg2")
  })
  
  tmp_post <- rv$selected_post

  rv$commentID <- as.integer(get_time_epoch())
  rv$timestamp2 <- get_time_human()
  rv$postID <- tmp_post$postID
  new_row <- row_inputs(fields_comment)
  new_row$post_name <- tmp_post$post_name 
  
  # Add a row to Comments table (show an error message in case of error)
  tryCatch({
    mongo_comments$insert(new_row) 
    updateTabItems(session,"tabs","mainTab")
  }, 

  error = function(err) {
    shinyjs::text("errorMsg2", err$message)
    shinyjs::show(id = "error2", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  }) 
  
  
  # Updae comment number and average_interest in Posts table
  if (tmp_post$average_interest==0) {
    average_interest <-   input$interest
  } 
  else {
    average_interest <- (tmp_post$average_interest*tmp_post$cumulative_comments + 
                           input$interest) / (tmp_post$cumulative_comments + 1)     
  }
  

  update_comments <- paste0('{"$set":{', 
                            '"current_comments":', (tmp_post$current_comments  + 1),
                            ', "cumulative_comments":', (tmp_post$cumulative_comments + 1),
                            ', "timestamp_comment":', get_time_epoch(), 
                            ', "average_interest":', average_interest, '}}')
  
  field_postID <- paste0('{"postID":', tmp_post$postID, '}')
  mongo_posts$update(field_postID, update=update_comments)
  
  updateTextInput(session, "app_link","Name of a similar App",value="NA") 
  updateSliderInput(session, "interest","Interest",min=1,max=5,step=1,value=3) 
  rv$comment_reset <-  rv$comment_reset + 1 
  rv$back_to_selected_post <- rv$back_to_selected_post + 1
}) 


output$resetable_comment <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$comment_reset 
  
  div(
    inputTextarea('comment', '',5,50), 
    tags$head(tags$style(type="text/css", "#comment {border-color: #C0C0C0}"))
    )
})


output$resetable_post <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$post_reset 
 
   div(
    inputTextarea('post', '', 40,50), 
    tags$head(tags$style(type="text/css", "#post {border-color: #C0C0C0}"))
  )
})


