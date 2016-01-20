


# Direct from message to user
observeEvent(input[["message_user"]], {
  updateTextInput(session,"msg_user","To:",value=rv$selected_user$user_name)
  updateTabsetPanel(session,"Message_tab","New")
  updateTabItems(session, "tabs", selected="notice")
})



# Send a message
observeEvent(input[["send_msg"]],{
  new_row <-  data.frame(message_id=user_session$message$message_id,
                         timestamp=get_time_human(),
                         sender_email_address=user_session$info$emailAddress,
                         sender_name=user_session$info$displayName,
                         receiver_email_address = rv$selected_user$email_address,       
                         receiver_name = rv$selected_user$user_name,
                         viewed_by_receiver = 0,
                         content = input$msg_content,
                         title =input$msg_title)
  
  mongo_messages$insert(new_row)
  updateTabsetPanel(session, "Message_tab",selected="Sent")
  rv$msg_reset <-   rv$msg_reset + 1
  updateTextInput(session,"msg_title","Title:",value=NULL)
  updateTextInput(session, "msg_user","To:",value=NULL)
})

# Resettable message content
output$resettable_msg_content <- renderUI({
  rv$msg_reset
  
  div(
    inputTextarea('msg_content', value=NULL,5,50),
    tags$head(tags$style(type="text/css", "#reply_content {border-color: #C0C0C0}"))
  )
})


# Function to retrive messages
retrieveMessages <- function(messages) {
   div(h4(" Exchange: ",strong(messages[1,]$sender_name), 
          "and", strong(messages[1,]$receiver_name)),
       h4(" Title: ", strong(messages[1,]$title)),
  
  lapply(1:dim(messages)[1], function(i) { 
    # messages <- messages[order(messages$timestamp),]
    tmp <- messages[i,]
    rv$active_senders_email <- messages$sender_email_address 
    # observeEvent(input[[paste0("message_sender",i)]], {
    #   browser()
    #   rv$view_sender <- i 
    #   rv$user_trafic <- "message"
    #   updateCollapse(session,"collapsePeople","Details")
    #   updateTabsetPanel(session,'tabs',"peopleTab")
    # })
    
    shinyjs::onclick(paste0("message_sender",i), {
      rv$view_sender <- i 
      rv$user_trafic <- "message"
      updateCollapse(session,"collapsePeople","Details")
      updateTabsetPanel(session,'tabs',"peopleTab")
    })
  
    wellPanel(  
      p(tmp$content, br(),
        # " -", actionButton(inputId =paste0("message_sender",i), tmp$sender_name, "link"), 
        " -", HTML(paste0('<a id="message_sender',i,'">',tmp$sender_name, '</a>')), 
        "at", substring(tmp$timestamp,12,16), 
        "on", strtrim(tmp$timestamp,10)
      ) 
    )
  })
   )
}   


output$table_notice_message <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  userID <- paste0('{"receiver_email_address": "',user_session$info$emailAddress,'"}')
  tbl <- mongo_messages$find(userID) 
  tbl <- tbl[rev(order(tbl$timestamp)),]
  tbl$viewed <- tbl$viewed_by_receiver

  DT::datatable( 
    tbl[c('message_id','timestamp','sender_name', 'title','viewed')],
    rownames = FALSE,
    colnames =c('id','Time','Sender', 'Title','Viewed'),
    selection = 'single', 
    options = list(scrollX = TRUE)
  ) %>% 
    formatStyle('viewed',
                color=styleInterval(0,c('gray','black')),
                backgroundColor = styleInterval(0, c('yellow','white'))
    )
})

output$table_notice_sent <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  userID <- paste0('{"sender_email_address": "',user_session$info$emailAddress,'"}')
  tbl <- mongo_messages$find(userID) 
  tbl <- tbl[rev(order(tbl$timestamp)),]
  tbl$viewed <- tbl$viewed_by_receiver
  
  DT::datatable( 
    tbl[c('message_id','timestamp','receiver_name', 'title','viewed')],
    rownames = FALSE,
    colnames =c('id','Time','Receiver', 'Title','Viewed'),
    selection = 'single', 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  ) %>% 
    formatStyle('viewed',
                color=styleInterval(0,c('gray','black')),
                backgroundColor = styleInterval(0, c('yellow','white'))
    )
})


# Change tab from Message Box to Content
observeEvent(input$table_notice_message_rows_selected, {
               rv$message_content <- "received"
               updateTabsetPanel(session, "Message_tab",selected="Content")
             })

observeEvent(input$table_notice_sent_rows_selected, {
  rv$message_content <- "sent"
  updateTabsetPanel(session, "Message_tab",selected="Content")
})



output$message_content <- renderUI({
  # Show selected row in DT table 
  need(length(rv$message_content)>0,"No message selected.")  %>% validate()
  
  if (rv$message_content=="received") {
    selected <- input$table_notice_message_rows_selected
  } else {
    selected <- input$table_notice_sent_rows_selected
  }
  
  need(length(selected)>0,"No message selected.")  %>% validate()
  
  selected <- selected[length(selected)]
  messageID <- paste0('{"message_id":', selected,'}')
  message_seq <- mongo_messages$find(messageID)
  message_seq <- message_seq[order(message_seq$timestamp),]
  
  need(dim(message_seq)[1]>0,"No message.")  %>% validate()
  
  isolate({
    user_session$message <- 
        list(message_id=as.integer(selected), 
             sender_email_address=
               message_seq$sender_email_address[message_seq$sender_email_address!=user_session$info$emailAddress][1],
             sender_name=
               message_seq$sender_name[message_seq$sender_name!=user_session$info$displayName][1],
             title= (message_seq$title)[1]
            ) 
  })
  mongo_messages$update(messageID, '{"$set": {"viewed_by_receiver":1}}',multiple=TRUE)
  
  div(retrieveMessages(message_seq))
}) 



# Resettable reply content
output$resettable_reply_content <- renderUI({
  rv$reply_reset
  
  div(
  inputTextarea('reply_content', value=NULL,5,50),
  tags$head(tags$style(type="text/css", "#reply_content {border-color: #C0C0C0}"))
  )
})

# Send reply
observeEvent(input$send_reply, {

  new_row <-  data.frame(message_id=user_session$message$message_id,
                       timestamp=get_time_human(),
                       sender_email_address=user_session$info$emailAddress,
                       sender_name=user_session$info$displayName,
                       receiver_email_address = user_session$message$sender_email_address,       
                       receiver_name = user_session$message$sender_name,
                       viewed_by_receiver = 0,
                       content = input$reply_content,
                       title = paste('Re:',user_session$message$title))
  
  mongo_messages$insert(new_row)
  updateTabsetPanel(session, "Message_tab",selected="Message Box")
  rv$reply_reset <-   rv$reply_reset + 1
  
})



# ---------------- Comments received ------------------
output$table_notice_comment <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  browser()
  
  userID <- paste0('{"email_address": "', user_session$info$emailAddress,'"}')
  posts <- mongo_posts$find(userID)
  
  need(length(posts)>0,"No posts.") %>% validate()
  
  
  colnames1 <- c(mongo_comments$find() %>% colnames())
  tbl <- matrix(NA,nrow=0,ncol=length(colnames1)) %>% data.frame() 
  colnames(tbl)  <- colnames1
  
  for (post_id in posts$postID) {
    loc_postID <- paste0('{"postID":',post_id,'}')
    comments <- mongo_comments$find(loc_postID)
    if (length(comments)>0) tbl <- rbind(tbl, comments) 
  }
  
  need(dim(tbl)[1]>0,"No comments.") %>% validate()
  
  tbl$comment_15 <- lapply(tbl$comment, function(com) paste0(strtrim(com,15),"..")) %>% unlist()
  tbl$post_link <- lapply(1:length(tbl$post_name), function(i) {
    paste0('<a id="link_comment_post',i,'">',tbl$post_name[i], '</a>')
  }) %>% unlist() 
  
  lapply(1:length(tbl$post_name), function(i) { 
  shinyjs::onclick(paste0("link_comment_post",i), {
    rv$post_trafic <- "notice_comment"
    rv$notice_comment_postID <- tbl$postID[i]
    updateCollapse(session,"collapseMain","Details")
    updateTabItems(session, "tabs", selected="mainTab")
  })
  })

  DT::datatable( 
    tbl[c('timestamp2','post_link','comment_15','viewed')],
    escape = FALSE,
    # rownames = FALSE, 
    colnames = c('Time','Post Name','Comment..','Viewed'), 
    selection = 'single', 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  ) %>% 
    formatStyle('viewed',
                color=styleInterval(0,c('gray','black')),
                backgroundColor = styleInterval(0, c('yellow','white'))
    )
}) 


# ---------------- Updates of posts the user follows ------------------
output$table_notice_follow <- DT::renderDataTable({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  # browser()
  
  userID <- paste0('{"user": "', user_session$info$emailAddress,'"}')
  following_postID <- mongo_follow_post$find(userID)$post 
  
  userID2 <- paste0('{"follower": "', user_session$info$emailAddress,'"}')
  following_userID <- mongo_follow_user$find(userID2)
  
 
  filter_postID <- list_filter_items(following_postID, numeric=TRUE)
  filter_userID <- list_filter_items(following_userID)
  tbl <- mongo_posts$find(paste0('{ "$or": [ {"postID": { "$in": [', filter_postID,']}},',
                            '{"email_address": { "$in": [', filter_userID,']}} ]}'))

  DT::datatable( 
    tbl[c('post_name', 'user_name','timestamp','status','edits','completeness')],
    colnames = c('Post Name', 'User Name','Last Updated','Status','Edits','Completeness'),
    rownames = FALSE,
    selection = 'single', 
    options = list(lengthChange = FALSE, scrollX = TRUE)
  )  
  # %>% 
  #   formatStyle('viewed',
  #               color=styleInterval(0,c('gray','black')),
  #               backgroundColor = styleInterval(0, c('yellow','white'))
  #   )
}) 


# ---------------- Progress in user's posts ------------------
output$table_notice_progress <- DT::renderDataTable({  
  need(length(user_session$info)>0," ",NULL) %>% validate()
  

  userID <- paste0('{"email_address": "', user_session$info$emailAddress,'"}')
  posts <- mongo_posts$find(userID)
  tbl <- posts[c('post_name','post_category','cumulative_views','cumulative_comments','likes', 'completeness')]   
  
  DT::datatable( 
    tbl,
    rownames = FALSE, 
    
    selection = 'single', 
    colnames =c('Post Name','Category','Cumulative Views','Cumulative Comments','Likes', 'Completeness'),
    options = list(lengthChange = FALSE, searching=FALSE, scrollX = TRUE)
  ) 
})

