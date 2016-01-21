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
        browser()
        
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
      field_postID <- paste0('{"email_address":"', user_IDs[i],'"}')
      rv$selected_user <-  mongo_users$find(field_postID)
      updateCollapse(session,"collapsePeople","Details")
      updateTabsetPanel(session,'tabs',"peopleTab")
      # updateTabItems(session, "tabs","peopleTab")
    })
  })
}

list_post_links <- function(post_names, post_IDs, link_id, N=length(post_names)) {
  if (N==0) return()
  browser()
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
    if (i<N) {
        HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>,&nbsp;&nbsp;'))
    } else {
        HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>'))
    }
    # HTML(paste0('<a id="',link_id,i,'">', post_names[i], '</a>,&nbsp;&nbsp;'))
  })
}



gen_post_id_links <- function(post_names, post_IDs, link_id,
                              post_trafic, varname_postID) {
  lapply(1:length(post_names), function(i) { 
    shinyjs::onclick(paste0(link_id,i), {
      rv$post_trafic <- post_trafic
      rv[[varname_postID]] <- post_IDs[i]
      updateCollapse(session,"collapseMain","Details")
      updateTabItems(session, "tabs", selected="mainTab")
      # updateTabItems(session, "tabs","peopleTab")
    })
    
    paste0('<a id="',link_id,i,'">', post_names[i], '</a>')
  }) %>% unlist() 
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

