


output$userpanel <- renderUI({
  # session$user is non-NULL only when authenticated 
  if (!is.null(user_session$info$token_valid)) {
    fist_name <- paste0("Welcome ", strsplit(rv$user_name," ")[[1]][1],"!")
    sidebarUserPanel(
      span(fist_name),
      subtitle = actionButton("log_out", "Logout","link")) 
  } else {
    sidebarUserPanel(span("   ",icon("google"),  
                          actionButton("log_in","Login","link")))
  } 
}) 




output$messageMenu <- renderMenu({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageData is a data frame with two columns, 'from' and 'message'.
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  userID <- paste0('{"receiver_email_address": "', rv$email_address,'"}')
  tbl <- mongo_messages$find(userID) 
  tbl <- tbl[tbl$viewed_by_receiver==0,]
  
  if (nrow(tbl)>0) {
    msgs <- lapply(1:nrow(tbl), function(row) {
          messageItem(from = tbl[row,]$sender_name,
                      message = paste0(strtrim(tbl[row,]$content,25),".."))
        })
  } else {
    msgs = NULL
  }
  
  # This is equivalent to calling:
  dropdownMenu(type = "messages", .list = msgs)
})

output$taskMenu <- renderMenu({
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, color = "green",
                        "Documentation"
               ),
               taskItem(value = 17, color = "aqua",
                        "Project X"
               ),
               taskItem(value = 75, color = "yellow",
                        "Server deployment"
               ),
               taskItem(value = 80, color = "red",
                        "Overall project"
               )
  )
})

output$notificationMenu <- renderMenu({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "XX comments received",
                 icon("bell-o"),
                 status = "info"
               ),
               notificationItem(
                 text = "XX updates of posts you follow",
                 icon("lightbulb-o"),
                 status = "info"
               ),
               notificationItem(
                 text = "XX posts being discussed in total.",
                 icon = icon("comments"),
                 status = "success"
               ),
               notificationItem(
                 text = "XX posts completed in total.",
                 icon = icon("trophy"),
                 status = "success"
               ),
               notificationItem(
                 text = "XX comments exchanged in total.",
                 icon = icon("commenting"),
                 status = "success"
               )
  )
})
