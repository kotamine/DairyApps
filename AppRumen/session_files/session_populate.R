
# ------- Prepare postings in Active Posts ------- 

# Construct mongdb qry content
list_filter_items <- function(var, numeric=FALSE) {
   v_list <- c()
  for (i in seq_along(var)) {  
    ifelse(numeric, 
           v_list  <- paste0(v_list, var[i]),
           v_list  <- paste0(v_list, '"', var[i], '"')
    )
    if (i < length(var))  v_list <- paste0(v_list,',')  
  }
  return(v_list)
}

# Construct mongdb qry content (tag-like method)
list_filter_items2 <- function(var, vec) {
  v_list <- c()
  vec_list <- c()
  for (i in seq_along(var)) vec_list <- c(vec_list,vec[grepl(var[i], vec)])
  vec_list <- vec_list %>% unique()
  for (i in seq_along(vec_list)) {
    v_list  <- paste0(v_list, '"', vec_list[i], '"')
    if (i < length(vec_list))  v_list <- paste0(v_list,',')  
  }
  return(v_list)
}

filter_posts <- reactive({
  status <- list_filter_items(input$filterStatus)
  categories <- list_filter_items(input$filterCategory)

  filter <-paste0('{"status": { "$in": [', status,'] }, "post_category": {"$in": [', categories,'] }}')
  return(filter)
})


filter_people <- reactive({
  profession <- list_filter_items(input$filterProfessions)
  interests <- list_filter_items2(input$filterInterests,  
                                  mongo_users$find()$interests %>% unlist() %>% unique()) 
  
  filter <-paste0('{"profession": { "$in": [', profession,'] }, "interests": {"$in": [', interests,'] }}')
  return(filter)
})

output$postboxes <- renderUI({
  # Acts as a trigger when the user is viewing
  rv$back_to_active_post 
  
    validate(
      need( (!is.null(input$n_boxes) & input$n_boxes>0) , 'Please enter the number of posts.')
    )  
  
  table_posts_copy <-  mongo_posts$find(filter_posts()) 
  if (length(table_posts_copy)==0) return()
  
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
  
  rv$active_posts_email <- sorted_table_posts$email_address
    
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
    need( (!is.null(input$n_boxes_people) & input$n_boxes_people>0) , 
          'Please enter the number of people.')
  )  
  browser()
  
  isolate({
  if (rv$user_trafic == "message") {
    updateCollapse(session,"collapsePeople","Details")
    return()
  }
  }) 
  
  table_users_copy <-  mongo_users$find(filter_people()) 
  if (length(table_users_copy)==0) return()
  
  tmp_sort <- switch(input$sortPeople, 
                     "Most recently joined"= table_users_copy$timestamp, # ADD TIMESTAMP IN USERS
#                      "Most posted" = table_users_copy$total_posts,  # ADD COUNTER FROM POST AND COMMENTS
#                      "Most commented"= table_users_copy$comments, 
                     "Most viewed"=table_users_copy$profile_views,
                     "Most followed"=table_users_copy$n_followers)
  
  # Sorted user profiles that will be retreived in "Details" panel via rv$view 
  sorted_table_users <- table_users_copy[rev(order(tmp_sort)),]
  rv$active_users_email <-  sorted_table_users$email_address 
  rv$user_trafic <- "people"
  
  lapply(c(1:input$n_boxes), function(x) {
    observeEvent(input[[paste0("view_user",x)]], ({
      rv$view_user <- x
      # Update "Details" panel via trigger "rv$back_to_selected_post"  
      rv$back_to_selected_user <- rv$back_to_selected_user + 1
      updateTabItems(session, "tabs","peopleTab")
      updateCollapse(session, "collapsePeople", open = "Details")
    }))
  })
  
  N <- dim(table_users_copy)[1]
  if (is.null(input$n_boxes_people) || is.na(input$n_boxes_people)) {
    n <- 0
  } else {
    n <- min(input$n_boxes_people,N)
  }
  
  lapply(1:n, function(i) {
    tmp_user <- sorted_table_users[i,]  
    box(
      # browser(), 
      p("User Name:  ", strong(tmp_user$user_name),br(),
        "Profession:  ", tmp_user$profession,br(),
        "Interests:", tmp_user$interests, br(), 
        "About:  ",paste(strtrim(tmp_user$about,50),"..."),br(),
#         "Posts:  ", tmp_user$total_posts, br(),
#         "Comments:  ", tmp_user$comments, br(),
        "Views:  ", tmp_user$profile_views, br(),
        "Followers:  ", tmp_user$n_followers, br(),
        "Since:  ", strtrim(tmp_user$timestamp,10),br()
      ),
      br(),
      actionButton(inputId = paste0("view_user", i),"View","primary") 
    )
  })
  
})

