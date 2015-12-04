# ------------ Show Selected Post and Enable Edit process ----------------
# Prepare the display of a selectet post in Details
output$selectedPost  <- renderUI({
  validate(
    need(!is.null(rv$view), 'No post is selected.')
  )
  # obtain a new copy of table (may be updated from some othere sessions)
  # table_posts_copy <- table_posts() 
  #           table_posts_copy <- load_data_gsheets("table_posts")
  #           table_comments_copy <- load_data_gsheets("table_comments") 
  table_posts_copy <-  mongo(collection="posts", db=db, url = url)$find()
  table_comments_copy <-  mongo(collection="comments", db=db, url = url)$find()
  
  rv$selectedPost <- table_posts_copy[rv$view,]
  tmp_post <- rv$selectedPost
  updateTextInput(session, "postID", value = tmp_post$postID)
  updateTextInput(session, "timestamp", value = get_time_human())
  
  tmp_comments <- table_comments_copy[tmp_post$postID==table_comments_copy$postID,]
  rv$selectedComments <- tmp_comments
  tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
  N_comments <- dim(tmp_comments)[1]
  
  if (rv$edit_auth) {
    #  prepare output$selectedPost for editing
    wellPanel(
      textInput("post_name_ed", "App Name", value = tmp_post$post_name),
      
      p( strong("By: "), tmp_post$user_name, br(), br()),
      selectInput("post_category_ed","Category", selected=tmp_post$post_category,
                  choices=c("Milk","Forage","Labor","Social")),
      #            textInput(inputId="post_ed", label="Description",value= tmp_post$post),
      #            tags$head(tags$style(type="text/css", "#post_ed {height: 200px; width: 100%; 
      #                                 text-align:center; display: block;}")),
      h5(strong("Description")), 
      inputTextarea('post_ed', value= tmp_post$post,20,50), 
      tags$head(tags$style(type="text/css", "#post_ed {border-color: #C0C0C0}")),
      br(), 
      p(strong("Views: "), tmp_post$cumulative_views, br(),
        strong("Comments:"), tmp_post$cumulative_comments, br(), 
        strong("Average Interest: "),tmp_post$average_interest, br(),
        strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
        strong("Edits:"), tmp_post$edits, br(),
        strong("Views since last edited: "), tmp_post$current_views, br(),
        strong("Comments since last edited:"), tmp_post$current_comments, br(),
        br(), 
        strong("<< Comments >> ")),
      
      retrieveComments(N_comments, tmp_comments),
      selectInput("decision","Decision",choices=c("Continue editing", "Move it to Completed Posts",
                                                  "Move it to Existing App Found", "Move it to Discontinued")),
      sliderInput("completeness","Degree of Completion",min=0,step=5,value=5,max=100), 
      actionButton("edit_send","Finish editing and Update")
    )
  } else {
    #        tmp_post <- table_posts_copy[rv$view,]
    #        updateTextInput(session, "postID", value = tmp_post$postID)
    #        updateTextInput(session, "post_name", value = tmp_post$post_name)
    #        
    # Increase the counter of views            
    #        tmp_location <- which(tmp_post$postID ==  table_posts_copy[,c("postID")] ) + 1
    #        rv$post_location <- paste0("A",tmp_location)
    #        tmp_post$current_views <- tmp_post$current_views + 1
    #        tmp_post$cumulative_views <- tmp_post$cumulative_views + 1
    #        "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,  
    #                                                     anchor=rv$post_location, col_names = FALSE)
    #         
    update_views <- paste0('{"$set":{', '"current_views":', (tmp_post$current_views + 1),
                           ', "cumulative_views":', (tmp_post$cumulative_views + 1), '}}')
    
    postID <- paste0('{"postID":', tmp_post$postID, '}')
    mongo(collection="posts", db=db, url = url)$update(postID, update=update_views)
    
    
    
    #        tmp_comments <- table_comments_copy[tmp_post$postID==table_comments_copy$postID,]
    #        tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
    #        N_comments <- dim(tmp_comments)[1]
    #        
    # prepare output$selectedPost for commenting
    wellPanel(
      h4(strong("App Name: "), tmp_post$post_name),
      p( strong("By: "), tmp_post$user_name, br(), br(),
         strong("Category: "), tmp_post$post_category,br(),
         strong("Description: "),tmp_post$post,br(), br(),
         strong("Views: "), tmp_post$cumulative_views, br(),
         strong("Comments:"), tmp_post$cumulative_comments, br(), 
         strong("Average Interest: "),tmp_post$average_interest, br(),
         strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
         strong("Edits:"), tmp_post$edits, br(),
         strong("Views since last edit: "), tmp_post$current_views, br(),
         strong("Comments since last edit:"), tmp_post$current_comments, br(),
         br(), 
         strong("<< Comments >> ")),
      
      retrieveComments(N_comments, tmp_comments)
      
      #        # Insert previuos comments
      #        lapply(1:N_comments, function(i) {
      #          tmp_com_item  <- tmp_comments[i,] 
      #          wellPanel(
      #            p(tmp_com_item$comment,br(),
      #              " - ",tmp_com_item$comment_user_name, " posted on ",
      #              strtrim(tmp_com_item$timestamp2,10))
      #          )
      #        })
    )
  }
})




# Open up description for edit 
observeEvent(input$edit, { 
  # authentication via google account
  rv$edit_auth <- TRUE
})

# ---------- Event: edit_send button ------------
observeEvent(input$edit_send, {
  #  not needed?       # Update the timestamp field to be the current time
  #        updateTextInput(session, "timestamp", value = get_time_human())
  
  #        # User-experience stuff
  #        shinyjs::disable("post_send")
  #        shinyjs::show("submitMsg")
  #        shinyjs::hide("error")
  #        on.exit({
  #          shinyjs::enable("post_send")
  #          shinyjs::hide("submitMsg")
  #        })
  
  # move the old post and comments to archive tables
  "table_archive_posts" %>% gs_title %>% gs_add_row(input = rv$selectedPost)
  N_comments <- dim(rv$selectedComments)[1]
  if (N_comments>0) {
    tmp_archive_table_comments <- load_data_gsheets("table_archive_comments")
    loc_archive_comments <- dim(tmp_archive_table_comments)[1] + 1
    "table_archive_comments" %>% gs_title %>% gs_edit_cells(input = rv$selectedComments, trim=TRUE,
                                                            anchor=paste0("A",loc_archive_comments),
                                                            col_names=FALSE)
    # remove old comments 
    tmp_table_comments <- load_data_gsheets("table_comments")
    tmp_table_comments  <- tmp_table_comments[!(tmp_table_comments$commentID %in% rv$selectedComments$commentID),] 
    "table_comments" %>% gs_title %>% gs_edit_cells(input = tmp_table_comments, trim=TRUE, 
                                                    anchor="A2", col_names = FALSE)
  }
  
  # update the counters for edits, currrent_veiws, and current_comments
  tmp_post <- rv$selectedPost
  tmp_post$timestamp <- get_time_human()
  tmp_post$edits <- tmp_post$edits + 1
  tmp_post$current_views <- 0
  tmp_post$current_comments <- 0
  "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,
                                               anchor=rv$post_location, col_names = FALSE)
  
  
  
  
  #updateTabItems(session, "tabs","postTab")
  
  #        # Save the data (show an error message in case of error)
  #        tryCatch({
  #          # save_data_gsheets(post_data(), "table_posts")
  #          "table_posts" %>% gs_title %>% gs_add_row(input = post_data())
  #          updateTabItems(session, "tabs","postTab")
  #        },
  #        error = function(err) {
  #          shinyjs::text("errorMsg", err$message)
  #          shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
  #          shinyjs::logjs(err)
  #        })
  rv$edit_auth <- FALSE
})
