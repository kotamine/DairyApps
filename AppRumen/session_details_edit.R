
# ------------ Show Selected Post and Enable Edit process ----------------
# Prepare the display of a selectet post in Details
output$selectedPost  <- renderUI({
  validate(
    need(!is.null(rv$view), 'No post is selected.')
  )
  
  rv$back_to_selected_post
  
  field_postID <- paste0('{"postID":', rv$active_postsID[rv$view],'}')
  rv$selectedPost <-  mongo(collection="posts", db=db, url = url)$find(field_postID)
  tmp_post <- rv$selectedPost
  
  rv$selectedComments <- mongo(collection="comments", db=db, url = url)$find(field_postID)
  tmp_comments <- rv$selectedComments 
  
  N_comments <- dim(tmp_comments)[1]
  if (N_comments>0) {
  tmp_comments <- tmp_comments[order(tmp_comments$timestamp2),]
  } 
  
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
        strong("Average Interest: "),round(tmp_post$average_interest,2), br(),
        strong("Date:  "), strtrim(tmp_post$timestamp,10),br(),br(),
        strong("Edits:"), tmp_post$edits, br(),
        strong("Views since last edited: "), tmp_post$current_views, br(),
        strong("Comments since last edited:"), tmp_post$current_comments, br(),
        br(), 
         strong("<< Comments >> ")
        ),
      
      retrieveComments(N_comments, tmp_comments),
      selectInput("decision","Decision",choices=c("Continue editing", "Move it to Completed Posts",
                                                  "Move it to Existing App Found", "Move it to Discontinued")),
      sliderInput("completeness","Degree of Completion",min=0,step=5,value=5,max=100), 
      actionButton("edit_send","Finish editing and Update","primary")
    ) 
  } else { 
        
    update_views <- paste0('{"$set":{', '"current_views":', as.integer(tmp_post$current_views + 1),
                           ', "cumulative_views":', as.integer(tmp_post$cumulative_views + 1), '}}')
    
    mongo(collection="posts", db=db, url = url)$update(field_postID, update=update_views)
    
    
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
  
    )
  }
}) 



# Open up description for edit 
observeEvent(input$edit, { 
  # authentication via google account
  shinyjs::hide("show_comment_box")
  rv$edit_auth <- TRUE
})

# ---------- Event: edit_send button ------------
observeEvent(input$edit_send, {

         # User-experience stuff
         shinyjs::disable("post_send")
         shinyjs::show("submitMsg")
         shinyjs::hide("error")
         on.exit({
           shinyjs::enable("post_send")
           shinyjs::hide("submitMsg")
         })
  
  browser()       
  tmp_post <- rv$selectedPost
  
  # move the old post and comments to archive tables
 # "table_archive_posts" %>% gs_title %>% gs_add_row(input = rv$selectedPost)
  
  mongo(collection="archive_posts", db=db, url = url)$insert(rv$selectedPost)
  
  N_comments <- dim(rv$selectedComments)[1]
  if (!is.null(N_comments)) {
#     tmp_archive_table_comments <- load_data_gsheets("table_archive_comments")
#     
#     loc_archive_comments <- dim(tmp_archive_table_comments)[1] + 1
#     "table_archive_comments" %>% gs_title %>% gs_edit_cells(input = rv$selectedComments, trim=TRUE,
#                                                             anchor=paste0("A",loc_archive_comments),
#                                                             col_names=FALSE)
    
    mongo(collection="archive_comments", db=db, url = url)$insert(rv$selectedComments)
    
    # remove old comments 
#     tmp_table_comments <- load_data_gsheets("table_comments")
#     tmp_table_comments  <- tmp_table_comments[!(tmp_table_comments$commentID %in% rv$selectedComments$commentID),] 
#     "table_comments" %>% gs_title %>% gs_edit_cells(input = tmp_table_comments, trim=TRUE, 
#                                                     anchor="A2", col_names = FALSE)
    
    field_postID <- paste0('{"postID":', tmp_post$postID, '}')
    mongo(collection="comments", db=db, url = url)$remove(field_postID, multiple = TRUE) 
    
  }
  
  # update the counters for edits, currrent_veiws, and current_comments
#   tmp_post$timestamp <- get_time_human()
#   tmp_post$edits <- tmp_post$edits + 1
#   tmp_post$current_views <- 0
#   tmp_post$current_comments <- 0
#   "table_posts" %>% gs_title %>% gs_edit_cells(input = tmp_post,
#                                                anchor=rv$post_location, col_names = FALSE)
#   

  update_edit <- paste0('{"$set":{', 
                        '"timestamp":','"', get_time_human(), '"',
                        ', "post_name":',  '"', input$post_name_ed, '"',
                        ', "post_category":',  '"', input$post_category_ed, '"', 
                        ', "edits":', as.integer(tmp_post$edits + 1), 
                        ', "current_views":', 0, 
                        ', "current_comments":', 0, 
                        ', "post":', '"',input$post_ed, '"',
                        '}}')
  
  mongo(collection="posts", db=db, url = url)$update(field_postID, update=update_edit)
  
  rv$back_to_selected_post <- rv$back_to_selected_post + 1
  
  shinyjs::show("show_comment_box")
  rv$edit_auth <- FALSE
})
