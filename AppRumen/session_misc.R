
# Gather all the form inputs
row_inputs <- function(fields) {
  new_row <- lapply(fields, function(x) {
    if (!is.null(input[[x]])) {
      x = input[[x]] 
    } else {
      x = rv[[x]]
    }
  }) %>% data.frame()
  rownames(new_row) <- NULL
  colnames(new_row) <- fields
  return(new_row)
}



# disable email_address in Post
shinyjs::toggleState("email_address", FALSE)



output$userpanel <- renderUI({
  # session$user is non-NULL only when authenticated 
  if (!is.null(user_session$info$token_valid)) {
    sidebarUserPanel(
      span("Welcome ", user_session$info$displayName),
      subtitle = actionButton("log_out", "Logout","link")) 
  } else {
    sidebarUserPanel(span("   ",icon("google"),  
                          actionButton("log_in","Login","link")))
  } 
}) 

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



# Allow the owner of the selected post to edit
observe({

  if (is.null(user_session$info$token_valid) | is.null( rv$selectedPost$email_address)) {
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
  updateTextInput(session, "user_name", value = user_session$info$displayName)
  updateTextInput(session, "email_address", value = user_session$info$emailAddress)
  updateTextInput(session, "comment_user_name", value = user_session$info$displayName)
  updateTextInput(session, "comment_email_address", value =  user_session$info$emailAddress)
})

observeEvent(input$gmail2, {
  # Give googlesheets permission to access your spreadsheets and google drive
  gs_auth( new_user = TRUE)
  user_session$info <- gs_user()
  updateTextInput(session, "user_name", value = user_session$info$displayName)
  updateTextInput(session, "email_address", value =  user_session$info$emailAddress)
  updateTextInput(session, "comment_user_name", value = user_session$info$displayName)
  updateTextInput(session, "comment_email_address", value =  user_session$info$emailAddress)
})



# Show tables of posts and comments 
output$viewTable <- DT::renderDataTable({
  browser()
  
  view_posts <- table_posts(); 
  view_posts <- view_posts %>% select(timestamp, postID, post_name, post_category, user_name,
                                      edits,current_views,cumulative_views,current_comments,
                                      cumulative_comments, average_interest)
  view_comments <- table_comments(); 
  view_comments <- view_comments %>% select(timestamp2, commentID, postID, post_name, comment_user_name,
                                            novelty, app_link, interest)
  
  view_archive_posts <- table_archive_posts(); 
  view_archive_posts <- view_archive_posts %>% select(timestamp, postID, post_name, post_category, user_name,
                                                      edits,current_views,cumulative_views,current_comments,
                                                      cumulative_comments, average_interest)
  view_archive_comments <- table_archive_comments(); 
  view_archive_comments <- view_archive_comments %>% select(timestamp2, commentID, postID, post_name, comment_user_name,
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


