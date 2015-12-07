
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


# Show/hide archive comments
shinyjs::onclick("a_view_archive_comments",
                 shinyjs::toggle(id="view_archive_comments", anim = TRUE)
)


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


# load databases after "Send" operations   
tables$posts <- reactive({
  input$post_send
  input$comment_send
  input$edit_send
  mongo_posts$find()
})

tables$comments <- reactive({
  input$comment_send
  mongo_comments$find()
})

tables$archive_posts <- reactive({
  input$edit_send
  mongo_archive_posts$find()
})

tables$archive_comments <- reactive({
  input$edit_send
  mongo_archive_comments$find()
})

tables$completed_posts <- reactive({
  input$edit_send
  mongo_completed_posts$find()
})

tables$resolved_posts <- reactive({
  input$edit_send
  mongo_resolved_posts$find()
})

tables$discontinued_posts <- reactive({
  input$edit_send
  mongo_discontinued_posts$find()
})



# Show tables of posts and comments 
output$viewTable <- DT::renderDataTable({ 
  view_tables <- list()

  for (x in c("posts","completed_posts","resolved_posts","discontinued_posts", "archive_posts")) {
    view_tables[[paste(x)]] <- tables[[paste(x)]]() %>% select(timestamp, postID, post_name, post_category, user_name,
                                                               edits,current_views,cumulative_views,current_comments,
                                                               cumulative_comments, average_interest)
  }  
  
  for (x in c("comments","archive_comments")) {
    view_tables[[paste(x)]] <- tables[[paste(x)]]() %>% select(timestamp2, commentID, postID, post_name, comment_user_name,
                                                               novelty, app_link, interest)
    
  }

  tbl <- switch(input$selectTable,
                "posts"=  view_tables[["posts"]],
                "completed_posts"= view_tables[["completed_posts"]],
                "resolved_posts"= view_tables[["resolved_posts"]],
                "discontinued_posts"= view_tables[["discontinued_posts"]],
                "archive_posts"= view_tables[["archive_posts"]],
                "comments"=  view_tables[["comments"]],
                "archive_comments"= view_tables[["archive_comments"]]
  ) 
  DT::datatable( 
    tbl,
    rownames = FALSE, 
    options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
  )
})


