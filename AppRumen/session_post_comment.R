# ---------- Event: post_send button ------------
observeEvent(input$post_send, {
  # Update the timestamp field to be the current time
  
  ## FIX UPDATE INPUT TYPE OPERATIONS
  updateTextInput(session, "timestamp", value = get_time_human())
  updateTextInput(session, "timestamp_comment", value = get_time_human())
  updateTextInput(session, "average_interest", value = "NA")
  
  updateTextInput(session, "postID", value = get_time_epoch())
  
  browser()  
  
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
    # "table_posts" %>% gs_title %>% gs_add_row(input = post_data())
    
    new_row <- post_data() %>% rbind() %>% data.frame()
    m <- mongo(collection="posts", db=db, url = url)
    colnames(new_row) <- colnames(m$find())
    m$insert(new_row) 
    
    updateTabItems(session, "tabs","postTab")
  },
  error = function(err) {
    shinyjs::text("errorMsg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  })
  rv$back_to_active_post <- rv$back_to_active_post + 1
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
  
  #        tmp_post$current_comments <- tmp_post$cumulative_comments + 1
  #        tmp_post$cumulative_comments <- tmp_post$cumulative_comments + 1
  #        tmp_post$timestamp_comment <- get_time_epoch()
  #        "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post, 
  #                                                     anchor=paste0("A",rv$post_location), col_names=FALSE)
  #        
  
  update_comments <- paste0('{"$set":{', '"current_views":', (tmp_post$current_comments  + 1),
                            ', "cumulative_views":', (tmp_post$cumulative_comments + 1),
                            ', "timestamp_comment":', get_time_epoch(), '}}')
  
  postID <- paste0('{"postID":', tmp_post$postID, '}')
  mongo(collection="posts", db=db, url = url)$update(postID, update=update_comments)
  
  
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
    # "table_comments" %>% gs_title %>% gs_add_row(input = comment_data())
    new_row <- comment_data() %>% matrix(nrow=1) %>% data.frame()
    mongo(collection="comments", db=db, url = url)$insert(new_row)
    updateTabItems(session,"tabs","mainTab")
  }, 
  error = function(err) {
    shinyjs::text("errorMsg2", err$message)
    shinyjs::show(id = "error2", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  }) 
}) 

