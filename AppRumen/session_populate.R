
# ------- Prepare postings in Active Posts ------- 


filter_posts <- reactive({
  
  status <- c()
  for (i in 1:length(input$filterStatus)) {
    status  <- paste0( status,'"',input$filterStatus[i],'"')
    if (i < length(input$filterStatus))  status <- paste0( status,',')  
  }
  
  categories <- c()
  for (i in 1:length(input$filterCategory)) {
    categories <- paste0(categories,'"',input$filterCategory[i],'"')
    if (i < length(input$filterCategory)) categories <- paste0(categories,',')  
  }
  
  filter <-paste0('{"status": { "$in": [', status,'] }, "post_category": {"$in": [', categories,'] }}')
  return(filter)
})


output$postboxes <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$back_to_active_post 
  
    validate(
      need( (!is.null(input$n_boxes) & input$n_boxes>0) , 'Please enter the number of posts.')
    )  
  
  table_posts_copy <-  mongo_posts$find(filter_posts()) 

  tmp_sort <- switch(input$sortPost, 
                     "Most recently posted"=  table_posts_copy$timestamp,
                     "Most recently commented"= table_posts_copy$timestamp_comment,
                     "Most commented"= table_posts_copy$cumulative_comments, 
                     "Most viewed"= table_posts_copy$cumulative_views,
                     "Highest interests"=  table_posts_copy$average_interest)

  # Sorted posts that will be retreived in "Details" panel via rv$view 
  sorted_table_posts <- table_posts_copy[rev(order(tmp_sort)),]
  rv$active_postsID <-  sorted_table_posts$postID 
  rv$user_trafic <- "post"
  
  lapply(c(1:input$n_boxes), function(x) {
    observeEvent(input[[paste0("view",x)]], ({
      rv$view <- x
      # Update "Details" panel via trigger "rv$back_to_selected_post"  
      rv$back_to_selected_post <- rv$back_to_selected_post + 1
      updateTabItems(session, "tabs","mainTab")
      updateCollapse(session, "collapseMain", open = "Details")
    }))
  })
  
  rv$active_users_email <- sorted_table_posts$email_address
    
  lapply(c(1:input$n_boxes_people), function(x) {
    observeEvent(input[[paste0("user",x)]], ({
      rv$view_user <- x
      # Update "Details" panel via trigger "rv$back_to_selected_user"  
      rv$back_to_selected_user <- rv$back_to_selected_user + 1
      updateTabItems(session, "tabs","peopleTab")
      updateCollapse(session, "collapsePeople", open = "Details")
    }))
  })
  
  
  N <- dim(table_posts_copy)[1]
  if (is.null(input$n_boxes) || is.na(input$n_boxes)) {
    n <- 0
  } else {
    n <- min(input$n_boxes,N)
  }
  
  lapply(1:n, function(i) {
    tmp_post <- sorted_table_posts[i,]  
    box(
      # browser(), 
      p("App Name:  ", strong(tmp_post$post_name),br(),
        "Category:  ", tmp_post$post_category,br(),
        "Description:  ",paste(strtrim(tmp_post$post,140),"..."),br(),
        "Views:  ", tmp_post$cumulative_views, br(),
        "Comments:  ", tmp_post$cumulative_comments, br(),
        "Average Interest:  ", round(tmp_post$average_interest,2), br(),
        "Date:  ", strtrim(tmp_post$timestamp,10), br(), 
       "By:", actionButton(inputId = paste0("user", i), tmp_post$user_name, "link")
       ), 
      actionButton(inputId = paste0("view", i),"View","primary") 
    ) 
  })
})

# 
# observeEvent(input$n_boxes, {
#   lapply(c(1:input$n_boxes), function(x) {
#     observeEvent(input[[paste0("view",x)]], ({
#       updateTabItems(session, "tabs","mainTab")
#       updateCollapse(session, "collapseMain", open = "Details")
#       rv$view <- x
#     }))
#   })
# })


output$peopleboxes <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$back_to_active_people 
  
  validate(
    need( (!is.null(input$n_boxes_people) & input$n_boxes_people>0) , 'Please enter the number of posts.')
  )  
  
  helpText("test")
#   table_posts_copy <-  mongo_posts$find(filter_category()) 
#   
#   tmp_sort <- switch(input$sortPost, 
#                      "Most recently posted"=  table_posts_copy$timestamp,
#                      "Most recently commented"= table_posts_copy$timestamp_comment,
#                      "Most commented"= table_posts_copy$cumulative_comments, 
#                      "Most viewed"= table_posts_copy$cumulative_views,
#                      "Highest interests"=  table_posts_copy$average_interest)
#   
#   # Sorted posts that will be retreived in "Details" panel via rv$view 
#   sorted_table_posts <- table_posts_copy[rev(order(tmp_sort)),]
#   rv$active_postsID <-  sorted_table_posts$postID 
#   lapply(c(1:input$n_boxes), function(x) {
#     observeEvent(input[[paste0("view",x)]], ({
#       rv$view <- x
#       # Update "Details" panel via trigger "rv$back_to_selected_post"  
#       rv$back_to_selected_post <- rv$back_to_selected_post + 1
#       updateTabItems(session, "tabs","mainTab")
#       updateCollapse(session, "collapseMain", open = "Details")
#     }))
#   })
#   
#   N <- dim(table_posts_copy)[1]
#   if (is.null(input$n_boxes) || is.na(input$n_boxes)) {
#     n <- 0
#   } else {
#     n <- min(input$n_boxes,N)
#   }
#   
#   lapply(1:n, function(i) {
#     tmp_post <- sorted_table_posts[i,]  
#     box(
#       # browser(), 
#       p("App Name:  ", strong(tmp_post$post_name),br(),
#         "Category:  ", tmp_post$post_category,br(),
#         "Description:  ",paste(strtrim(tmp_post$post,140),"..."),br(),
#         "Views:  ", tmp_post$cumulative_views, br(),
#         "Comments:  ", tmp_post$cumulative_comments, br(),
#         "Average Interest:  ", round(tmp_post$average_interest,2), br(),
#         "Date:  ", strtrim(tmp_post$timestamp,10),br(),
#         "By:  ", tmp_post$user_name
#       ),
#       br(),
#       actionButton(inputId = paste0("view", i),"View","primary") 
#     )
#   })
  
  
})

