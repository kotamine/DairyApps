output$resetable_comment <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$comment_reset 
  if (!is.null(user_session$info)) {
    div(
      inputTextarea('comment', '',5,50), 
      tags$head(tags$style(type="text/css", "#comment {border-color: #C0C0C0}"))
    )
  } else {
    div(
      shinyjs::disabled(inputTextarea('comment', '',5,50)), 
      tags$head(tags$style(type="text/css", "#comment {border-color: #C0C0C0}"))
    )
  }
})


output$resetable_post <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$post_reset 
  if (!is.null(user_session$info)) {
    div(
      inputTextarea('post', '', 40,50), 
      tags$head(tags$style(type="text/css", "#post {border-color: #C0C0C0}"))
    ) 
  } else {
    div(
      shinyjs::disabled(inputTextarea('post', '', 40,50)), 
      tags$head(tags$style(type="text/css", "#post {border-color: #C0C0C0}"))
    ) 
  }
})



# Gather all the form inputs
row_inputs <- function(fields, list) {
  new_row <- lapply(fields, function(x) {
    if (!is.null(input[[x]])) {
      x = input[[x]] 
    } else {
      x = list[[x]]
    }
  }) %>% data.frame()
  rownames(new_row) <- NULL
  colnames(new_row) <- fields
  return(new_row)
}


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
  
  list1 <- list()
  list1$timestamp <- get_time_human()
  list1$postID <- as.integer(get_time_epoch())
  list1$status <- "Active"
  list1$user_name <- rv$user_name
  list1$email_address <- rv$email_address
  lapply(c("edits", "current_views", "cumulative_views", "current_comments",
    "cumulative_comments", "timestamp_comment","likes", "n_followers"), 
    function(item) list1[[item]] <<- 0 )
  list1$completeness <- 5 

  new_row <- row_inputs(fields_post, list1)
  
  # Add a row to the data (show error message in case of error)
  tryCatch({
    mongo_posts$insert(new_row) 
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
  updateNumericInput(session,"n_boxes","Number of Posts", value=(input$n_boxes+1), min=0,step=5,max=100)
  
  updateCollapse(session, "collapseMain", open = "Posts")
  updateTabItems(session, "tabs", selected="mainTab")
})



# ---------- Event: comment_send button ------------
observeEvent(input$comment_send, {
 browser()
  # User-experience stuff
  shinyjs::disable("comment_send")
  shinyjs::show("submitMsg2")
  shinyjs::hide("error2")
  on.exit({
    shinyjs::enable("comment_send")
    shinyjs::hide("submitMsg2")
  })
  
  tmp_post <- rv$selected_post
  
  list1 <- list()
  list1$commentID <- as.integer(get_time_epoch())
  list1$timestamp2 <- get_time_human()
  list1$postID <- tmp_post$postID
  list1$post_name <- tmp_post$post_name
  list1$comment_status <- "Active"
  list1$comment_user_name <- rv$user_name
  list1$comment_email_address <- rv$email_address
  list1$viewed_by_owner <- 0
  
  new_row <- row_inputs(fields_comment, list1)

  # Add a row to Comments table (show an error message in case of error)
  tryCatch({
    mongo_comments$insert(new_row) 
  }, 

  error = function(err) {
    shinyjs::text("errorMsg2", err$message)
    shinyjs::show(id = "error2", anim = TRUE, animType = "fade")      
    shinyjs::logjs(err)
  }) 
  
  update_comments <- paste0('{"$set":{', 
                            '"current_comments":', (tmp_post$current_comments  + 1),
                            ', "cumulative_comments":', (tmp_post$cumulative_comments + 1),
                            ', "timestamp_comment":', get_time_epoch(), 
                            '}}')
  
  field_postID <- paste0('{"postID":', tmp_post$postID, '}')
  mongo_posts$update(field_postID, update=update_comments)
  
  updateTextInput(session, "app_link","Name of a similar App",value="NA") 
  rv$comment_reset <-  rv$comment_reset + 1 
  
  tmp_post2 <- mongo_posts$find(field_postID)
  rv$selected_post <- tmp_post2[tmp_post2$status!="Archive",]
  rv$selected_comments <- mongo_comments$find(field_postID) 
}) 



