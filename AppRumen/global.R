## some part borrowed from:
# https://github.com/daattali/shiny-server/persistent-data-storage/storage.R
# https://github.com/daattali/shiny-server/persistent-data-storage/helpers.R

library(shiny)
library(shinyBS)
library(shinydashboard)
library(rmarkdown)
library(DT)
library(googlesheets)
library(mongolite)
library(data.table)
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(digest)


zeros <- function(r,c) matrix(c(mat.or.vec(r,c)),nrow=r,ncol=c) 
ones <- function(r,c) matrix(c(rep(1,r*c)),nrow=r,ncol=c) 
nulls <- function(r,c) matrix(c(rep(NA,r*c)),nrow=r,ncol=c)

sort2 <- function(x,...) ifelse(is.null(x),NA,sort(x,...))
div2 <- function(x,y) ifelse(y==0 | length(y)==0, NA, x/y)

vars_category <- c("Milk","Forage","Labor","Social")
vars_profession <- c("Student", "Producer", "Industry", "Extension", "Other")
vars_interests <- c("Generating ideas",  "Collaborating",  "Dairy Productivity",  "Producer Outreach",  
                    "Youth Education",  "Public Outreach",  "Networking",  "Other")

# mandatory fields in the form
fields_post_mandatory <- c(
  "post_name", "post_category", "post"
)

# All fields in  mongo_posts
fields_post <- c(
  "timestamp", "postID", "status", fields_post_mandatory,  
  "user_name", "email_address", 
  "edits", "current_views", "cumulative_views", "current_comments",
  "cumulative_comments", "timestamp_comment", 
  "likes", "n_followers", "completeness"
)

fields_comment_mandatory <- c(
  "novelty", "app_link", "comment" 
)

# All fields in  mongo_comments
fields_comment <- c(
  "timestamp2", "commentID", "postID", "post_name", "comment_status",
  "comment_user_name", "comment_email_address", 
  fields_comment_mandatory, "viewed_by_owner"
)

user_fields <- c(
  "timestamp", "email_address", "user_name", "profession", "interests", "location",        
  "last_logged_in", "profile_views","about", "linked_in", 
  "n_followed_posts", "n_followers", "n_log_in" 
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


# Larger Text Area than Shiny's textInput() 
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

# Add styles to actionButton()
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

