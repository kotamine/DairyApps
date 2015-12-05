# ---------- Event: post_send button ------------
observeEvent(input$post_send, {
  
  browser()
  
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
    mongo(collection="posts", db=db, url = url)$insert(new_row) 
    updateTabItems(session, "tabs","mainTab")
  },
  
  error = function(err) {
    shinyjs::text("errorMsg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  })
  rv$back_to_active_post <- rv$back_to_active_post + 1
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
  
  tmp_post <- rv$selectedPost

  rv$commentID <- as.integer(get_time_epoch())
  rv$timestamp2 <- get_time_human()
  rv$postID <- tmp_post$postID
  rv$post_name <- tmp_post$post_name 
  new_row <- row_inputs(fields_comment)

  # Add a row to Comments table (show an error message in case of error)
  tryCatch({
    mongo(collection="comments", db=db, url = url)$insert(new_row) 
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
  mongo(collection="posts", db=db, url = url)$update(field_postID, update=update_comments)
  
  rv$back_to_selected_post <- rv$back_to_selected_post + 1
}) 

