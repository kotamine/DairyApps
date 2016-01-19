## some part borrowed from:
# https://github.com/daattali/shiny-server/persistent-data-storage/storage.R
# https://github.com/daattali/shiny-server/persistent-data-storage/helpers.R

library(dplyr)
library(digest)
library(googlesheets)


sort2 <- function(x,...) ifelse(is.null(x),NA,sort(x,...))
div2 <- function(x,y) ifelse(y==0 | length(y)==0, NA, x/y)

vars_category <- c("Milk","Forage","Labor","Social")
vars_profession <- c("Student", "Producer", "Industry", "Extension", "Other")
vars_interests <- c("Generating ideas",  "Collaborating",  "Dairy Productivity",  "Producer Outreach",  
  "Youth Education",  "Public Outreach",  "Networking",  "Other")

# mandatory fields in the form
fields_post_mandatory <- c(
  "post_name", "post_category", "user_name", "email_address", "post"
)

fields_comment_mandatory <- c(
  "novelty", "app_link", "interest", "comment_user_name", "comment_email_address", "comment" 
)


# all fields in the form we want to save
fields_post <- c(
  "timestamp", "postID", "status", "edits", "current_views", "cumulative_views", "current_comments",
  "cumulative_comments", "timestamp_comment", "average_interest", fields_post_mandatory
)

fields_comment <- c(
  "timestamp2", "commentID", "postID", "post_name", fields_comment_mandatory
)

fields_user <- c(
  "timestamp", "email_address", "user_name","profession","interests","last_logged_in", "profile_views", "about"
)

fields_follow_posts <- c(
  "timestamp", "email_address", "user_name", "postID","post_name"
)

fields_follow_people <- c(
  "timestamp", "email_address_following", "user_name_followwing", "email_address_followed", "user_name_followed"
)


# get current Epoch time
get_time_epoch <- function() {
     return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
get_time_human <- function() {
     format(Sys.time(), "%Y-%m-%d-%H:%M:%OS")
}


# #### Method 5: Google Sheets ####
# save_data_gsheets <- function(data, TABLE_NAME) {
#  TABLE_NAME %>% gs_title %>% gs_add_row(input = data)
# }
# 
# load_data_gsheets <- function(TABLE_NAME,...) {
#   TABLE_NAME %>% gs_title %>% gs_read_csv(...)
# }


inputTextarea <- function(inputId, value="", nrows, ncols) {
     tagList(
          singleton(tags$head(tags$script(src = "textarea.js"))),
          tags$textarea(id = inputId,
                        class = "inputtextarea",
                        rows = nrows,
                        cols = ncols,
                        as.character(value))
     )
}


actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
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
    (p("Leave the first comment on this idea!"))
  }
}    

