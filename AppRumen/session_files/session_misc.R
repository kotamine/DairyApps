## -------- code regarding log in info ---------
# Log in when opening the website 
lapply(c("log_in_0","log_in"), function(item) {
  observeEvent(input[[item]], { 
    google_login()
  })
})

# Give googlesheets permission to access your spreadsheets and google drive
google_login <- function() {
  gs_auth( new_user = TRUE)
  user_session$guest <- FALSE
  user_session$info <- gs_user()
  rv$user_name <- user_session$info$displayName
  rv$email_address <- user_session$info$emailAddress
}

observeEvent(input$log_in_guest, {
  user_session$guest <- TRUE
})

observeEvent(input$log_out, {
  user_session$info <- NULL
  rv$user_name <- NULL
  rv$email_address <- NULL
}) 


send_buttons <- c("post_send","comment_send", "message_user")

observe({
  user_session$guest 
  
  if (!is.null(user_session$info)) {
    lapply(c(fields_post_mandatory,fields_comment_mandatory,send_buttons ),
           function(item)  shinyjs:: enable(item))
    
  } else {
    lapply(c(fields_post_mandatory,fields_comment_mandatory,send_buttons ),
           function(item)  shinyjs:: disable(item))
  }
})

# Show/hide the UI through log-in 
observe({
  if (!is.null(user_session$info) | user_session$guest) {
    shinyjs:: hide("log_in_page")
    shinyjs:: show("after_log_in")
  } else {
    shinyjs:: show("log_in_page")
    shinyjs:: hide("after_log_in")
  }
  if (!is.null(user_session$info)) { 
    shinyjs:: show("after_log_in_notice")
  } else {
    shinyjs:: hide("after_log_in_notice")
  }
})

# Create User if it is the first time   
observeEvent(user_session$info, {
  if (is.null(user_session$info)) return()
  
  if (rv$email_address %in% mongo_users$find()$email_address) {
    field_userID <- paste0('{"email_address":"', rv$email_address,'"}')
    tmp_user <- mongo_users$find(field_userID)
    update_log_in <- paste0('{"$set": {"last_logged_in":"', get_time_human(),'",
                     "n_log_in":', (tmp_user$n_log_in + 1),'}}')
    
    mongo_users$update(field_userID,update_log_in)
    
  } else {
    new_row <- lapply(user_fields, function(var) NA) %>% data.frame()
    colnames(new_row) <- user_fields
    new_row$timestamp <- get_time_human()
    new_row$email_address <- rv$email_address
    new_row$user_name <- rv$user_name
    new_row$last_logged_in <- get_time_human()
    new_row$profile_views <- 0
    new_row$n_followed_posts <- 0
    new_row$n_followers <- 0
    new_row$n_log_in <- 0
    mongo_users$insert(new_row)
  }
  
})


## ------------- Enable/disable by condition --------------
# Allow the owner of the selected post or selected user to edit
insert_edit <- function(var_name, item1, item2) {
  # lappy() can be used for UI-side logic 
  lapply(1, function(a) { # a is just a placeholder 
  ifelse(is.null(user_session$info) | is.null(item1), # check NULL only for item1
    tmp_edit <- FALSE,
    ifelse(item1 == item2, tmp_edit <- TRUE, tmp_edit <- FALSE)) 
    if (tmp_edit) {
      div(actionButton(var_name,"Edit (author only)","primary"), br())
    }
  })
}



 

# Enable the Submit button when all mandatory fields are filled out
observe({
  fields_post_filled <-
    fields_post_mandatory %>%
    sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
    all()
  shinyjs::toggleState("post_send", fields_post_filled)
  
  fields_comment_filled <-
    fields_comment_mandatory %>%
    sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
    all()
  shinyjs::toggleState("comment_send", fields_comment_filled)
})


# Change between Like to Liked
switch_like_unlike <- function(mongo_db, like_show, unlike_show,
                               var_name1, var_name2, var1, var2, var2_sub,
                               button1, button2) {
list(observe({ 
  if (is.null(user_session$info)) return()
    shinyjs::enable(like_show)
    input[[button1]]
    input[[button2]]
  if (length(mongo_db$find(
    paste0('{"',var_name1, '": "', rv[[var1]],
           '", "', var_name2,'": "', rv[[var2]][[var2_sub]],'"}')))>0) {
    shinyjs::hide(like_show) 
    shinyjs::show(unlike_show)
  } else {
    shinyjs::hide(unlike_show)
    shinyjs::show(like_show) 
  }
}),
observeEvent(input[[button1]], {
  new_row <- matrix(c(rv[[var1]],rv[[var2]][[var2_sub]]),nrow=1) %>% data.frame()
  colnames1 <- (mongo_db$find() %>% colnames())
  if (length(colnames1)>ncol(new_row)) {
    new_row <- cbind(new_row, rep(0, (length(colnames1)-ncol(new_row))))
  }
  colnames(new_row) <- colnames1 
  mongo_db$insert(new_row)
}), 
observeEvent(input[[button2]], {
  mongo_db$remove(paste0('{ "',var_name1, '": "', rv[[var1]], 
                            '", "',var_name2,'": "', rv[[var2]][[var2_sub]],'"}')) 
})
)
}

switch_like_unlike(mongo_likes, "like_0","like_1", "user", "post",
                  "email_address", "selected_post", "postID",
                   "like","unlike") %>% unlist()

switch_like_unlike(mongo_follow_post, "follow_0","follow_1", "user", "post",
                   "email_address", "selected_post", "postID",
                   "follow","unfollow") %>% unlist() 

switch_like_unlike(mongo_follow_user, "follow_user_0","follow_user_1", "follower", "followed",
                   "email_address", "selected_user", "email_address",
                   "follow_user","unfollow_user") %>% unlist() 
 

  
# Show/hide archive comments
shinyjs::onclick("a_view_archive_comments",
                 shinyjs::toggle(id="view_archive_comments", anim = TRUE)
)








## --- The following is used in Table View. This section may be omitted. ---- 
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


