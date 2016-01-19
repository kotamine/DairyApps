
# Send a message to another user
observeEvent(input[["message_user"]], {
  updateTabItems(session, "tabs", selected="notice")
  updateTabItems(session, "Message", selected="Content")
  updateTextInput(session, "message_to","Message to:",value=rv$selected_user$user_name)
  # output$message_conentent <- NULL
  # MORE HERE
})

observeEvent(input[["send_message"]], { 
  
  # add message here 
  
  updateTextInput(session, "message_to","Message to:",value=NULL)
})

# observe({
#   need(length(user_session$info)>0," ",NULL) %>% validate()
#   
#   browser()
# }) 

output$table_notice_message <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  browser()
  
  userID <- paste0('{"receiver_email_address":', '"',user_session$info$emailAddress,'"','}')
  tbl <- mongo_messages$find(userID) 
  tbl <- tbl[rev(order(tbl$timestamp)),]

  DT::datatable( 
    tbl,
    rownames = FALSE, 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  )
})

output$message_conentent <- renderUI({
  
})

output$table_notice_comment <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  browser()
  
  userID <- paste0('{"email_address":', user_session$info$emailAddress,'}')
  posts <- mongo_posts$find(userID)
  
  colnames1 <- c(mongo_comments$find() %>% colnames())
  tbl <- matrix(NA,nrow=0,ncol=length(colnames1)) %>% data.frame() 
  colnames(tbl)  <- colnames1
  
  for (post_id in posts$postID) {
    loc_postID <- paste0('{"postID":', '"',post_id,'"','}')
    comments <- mongo_comments$find(loc_postID)
    if (length(comments)>0) tbl <- rbind(tbl, comments) 
  }
  
  DT::datatable( 
    tbl,
    rownames = FALSE, 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  )
})

output$table_notice_follow <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  browser()
  
  userID <- paste0('{"user":', user_session$info$emailAddress,'}')
  following_postID <- mongo_follow_post$find(userID)$post 
  
  userID2 <- paste0('{"follower":', user_session$info$emailAddress,'}')
  following_userID <- mongo_follow_user$find(following_userID2)
  
 
  filter_postID <- list_filter_items(following_postID)
  filter_userID <- list_filter_items(following_userID)
  tbl <- mongo_posts$find('{ $or: [ {"postID": { "$in": [', filter_postID,']}},',
                            '{"email_address": { "$in": [', filter_userID,']}} ]}')

  DT::datatable( 
    tbl,
    rownames = FALSE, 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  )
})

output$table_notice_progress <- DT::renderDataTable({  
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  
  
})

