

# system_use <- gs_title("system_use")
messageData <-   mongo_system_use$find() 



output$userpanel <- renderUI({
  # session$user is non-NULL only when authenticated 
  if (!is.null(user_session$info$token_valid)) {
    fist_name <- paste0("Welcome ", strsplit(user_session$info$displayName," ")[[1]][1],"!")
    sidebarUserPanel(
      span(fist_name),
      subtitle = actionButton("log_out", "Logout","link")) 
  } else {
    sidebarUserPanel(span("   ",icon("google"),  
                          actionButton("log_in","Login","link")))
  } 
}) 

observeEvent(input$log_out, {
  user_session$info <- NULL
})


output$messageMenu <- renderMenu({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageData is a data frame with two columns, 'from' and 'message'.
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  browser()
  
  userID <- paste0('{"receiver_email_address": "',user_session$info$emailAddress,'"}')
  tbl <- mongo_messages$find(userID) 
  tbl <- tbl[tbl$viewed_by_receiver==0,]
  
  if (dim(tbl)[1]>0) {
  msgs <- apply(tbl, 1, function(row) {
    messageItem(from = row$sender_name, message = paste0(strtrim(row$content,15),".."))
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
                 text = "XX posts being discussed.",
                 icon = icon("comments"),
                 status = "success"
               ),
               notificationItem(
                 text = "XX posts completed.",
                 icon = icon("trophy"),
                 status = "success"
               ),
               notificationItem(
                 text = "XX comments exchanged.",
                 icon = icon("heart-o"),
                 status = "success"
               )
  )
})
