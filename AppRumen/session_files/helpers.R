## ------------------ Functions used in filtering mongo databases -------------------------

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


## ------------------ Functions that create links -------------------------
gen_post_links <- function(post_IDs, link_id, N=length(post_IDs)) { 
    if (N==0) return()
    lapply(1:N, function(i) { 
      observeEvent(input[[paste0(link_id,i)]], {
        field_postID <- paste0('{"postID":', post_IDs[i],'}')
        tmp_post <-  mongo_posts$find(field_postID)
        rv$selected_post <- tmp_post[tmp_post$status!="Archive",]
        rv$selected_comments <- mongo_comments$find(field_postID)
        updateTabItems(session, "tabs", selected="mainTab")
        updateCollapse(session,"collapseMain","Details")
        # updateTabItems(session, "tabs","peopleTab")
    })
  })
}

# shinyjs::onclick(paste0(link_id,i), {... })  
#   would implement something similar but cannot be overwritten

gen_user_links <- function(user_IDs, link_id, N=length(user_IDs)) { 
  if (N==0) return()
  lapply(1:N, function(i) { 
    observeEvent(input[[paste0(link_id,i)]], {
      field_userID <- paste0('{"email_address":"', user_IDs[i],'"}')
      rv$selected_user <-  mongo_users$find(field_userID)
      updateTabItems(session,'tabs',"peopleTab")
      updateCollapse(session,"collapsePeople","Details")
      # updateTabItems(session, "tabs","peopleTab")
    })
  })
}

list_post_links <- function(post_names, post_IDs, link_id, 
                            N=length(post_names), nocomma=FALSE) {
  if (N==0) return()
  lapply(1:N, function(i) { 
    shinyjs::onclick(paste0(link_id,i), {
      field_postID <- paste0('{"postID":', post_IDs[i],'}')
      tmp_post <-  mongo_posts$find(field_postID)
      rv$selected_post <- tmp_post[tmp_post$status!="Archive",]
      rv$selected_comments <- mongo_comments$find(field_postID)
      updateCollapse(session,"collapseMain","Details")
      updateTabItems(session, "tabs", selected="mainTab")
    })
  })
  
  lapply(1:N, function(i) { 
    if (nocomma) {
      HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>')) 
    } else {
      if (i<N) {
        HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>,&nbsp;&nbsp;'))
      } else {
        HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>'))
      }
    }
  })
}



# Function to insert previuos comments
retrieveComments <- function(N_comments, tmp_comments, archive=FALSE) {
  if (archive)  {
    comment_user <-  "archive_comment_user"
  } else {
    comment_user <-  "comment_user"
  }
  if (N_comments>0) {
    lapply(1:N_comments, function(i) {
      tmp_com_item  <- tmp_comments[i,] 
      if(is.null(tmp_com_item$app_link)) {
        tmp_com_item$app_link <- NA
      }
      
      if (is.na(tmp_com_item$app_link)) { 
        app_suggested <- ""
      } else { 
        app_suggested <- paste("Similar App:", tmp_com_item$app_link, "HTML(<br>)")
      }
      
        wellPanel( 
          p(
            HTML("&ldquo;"), tmp_com_item$comment, HTML("&rdquo;"),br(),
            app_suggested,
            " - ", actionButton(inputId =paste0(comment_user,i), tmp_com_item$comment_user_name, "link"), 
            " posted on ", 
            strtrim(tmp_com_item$timestamp2,10)) 
        )
    })
  } else { 
    p("Leave the first comment on this idea!")
  }
}    


# Function to retrive messages
retrieveMessages <- function(messages) {
  
  gen_user_links(messages$sender_email_address, "message_sender")
  
  div(h4("Conversation: ",strong(messages[1,]$sender_name), 
         "and", strong(messages[1,]$receiver_name)),
      h4(" Title: ", strong(messages[1,]$title)),

      lapply(1:dim(messages)[1], function(i) { 
      messages <- messages[order(messages$timestamp),]
            tmp <- messages[i,]
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
