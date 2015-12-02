

# system_use <- gs_title("system_use")
messageData <-   mongo(collection="system_use", db=db, url = url)$find()
#messageData <-  load_data_gsheets("system_use", ws="messageData")

output$messageMenu <- renderMenu({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageData is a data frame with two columns, 'from' and 'message'.
  loc_messageData <-  messageData 
  msgs <- apply(loc_messageData, 1, function(row) {
    messageItem(from = row[["from"]], message = row[["message"]])
  })
  
  # This is equivalent to calling:
  #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  dropdownMenu(type = "messages", .list = msgs)
})

output$taskMenu <- renderMenu({
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
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "5 new users today",
                 icon("users")
               ),
               notificationItem(
                 text = "12 items delivered",
                 icon("truck"),
                 status = "success"
               ),
               notificationItem(
                 text = "Server load at 86%",
                 icon = icon("exclamation-triangle"),
                 status = "warning"
               )
  )
})
