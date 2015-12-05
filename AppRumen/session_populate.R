
# ------- Prepare postings in Active Posts ------- 
output$postboxes <- renderUI({
  rv$back_to_active_post
  
  table_posts_copy <-  mongo(collection="posts", db=db, url = url)$find()
  table_posts_copy <-  table_posts_copy[table_posts_copy$post_category %in% input$filterCategory,]
  # table_posts_copy$average_interest[table_posts_copy$average_interest=="NA"] <- 0        
  
  tmp_sort <- switch(input$sortPost, 
                     "Most recently posted"=  table_posts_copy$timestamp,
                     "Most recently commented"= table_posts_copy$timestamp_comment,
                     "Most commented"= table_posts_copy$cumulative_comments, 
                     "Most viewed"= table_posts_copy$cumulative_views,
                     "Highest interests"=  table_posts_copy$average_interest)

  sorted_table_posts <- table_posts_copy[rev(order(tmp_sort)),]
  rv$active_postsID <-  sorted_table_posts$postID 
  
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
        "Average Interest:  ",tmp_post$average_interest, br(),
        "Date:  ", strtrim(tmp_post$timestamp,10),br(),
        "By:  ", tmp_post$user_name
      ),
      br(),
      actionButton(inputId = paste0("view", i),"View","primary") 
    )
  })
})

observeEvent(input$n_boxes, {
  lapply(c(1:input$n_boxes), function(x) {
    observeEvent(input[[paste0("view",x)]], ({
      updateCollapse(session, "collapseMain", open = "Details")
      rv$view <- x
    }))
  })
})

