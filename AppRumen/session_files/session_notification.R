


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
  
  browser()
  
#   field_userID <- paste0('{"email_address": "', rv$email_address,'"}')
#   posts0 <- mongo_posts$find(field_userID)
#   posts <- posts0[posts0$status!="Archive",]
  
  posts <- my_posts()
  colors <- c("red","yellow","aqua","green")
  
  if (nrow(posts)>0)
  color_code <- lapply(posts$completeness, 
                      function(val) {
                        if (val<25) { 
                          colors[1] 
                        } else if (val<50) {
                          colors[2]
                        } else if (val<75) {
                          colors[3]
                        } else colors[4]
                          }) %>% unlist()
  
  dropdownMenu(type = "tasks", badgeStatus = "success", 
                
               lapply(1:nrow(posts), function(i) {
                 taskItem(value = posts[i,]$completeness, 
                          color = color_code[i],
                          posts[i,]$post_name
                 )
               })
#                taskItem(value = 90, color = "green",
#                         "Documentation"
#                ),
  )
})

output$notificationMenu <- renderMenu({ 
  need(length(user_session$info)>0," ",NULL) %>% validate()
  
  # browser()
  
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
                 icon = icon("users"),
                 status = "success"
               )
  )
})
