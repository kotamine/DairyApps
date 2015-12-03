## largely borrowed from:
# https://github.com/daattali/shiny-server/persistent-data-storage/storage.R
# https://github.com/daattali/shiny-server/persistent-data-storage/helpers.R


library(dplyr)
library(digest)
# library(DBI)
# library(RMySQL)
# library(RSQLite)
# library(rmongodb)
library(googlesheets)
# library(RAmazonS3)
# library(rdrop2)

# 
# 
# # decide which function to use to save based on storage type
# get_save_fxn <- function(type) {
#      fxn <- sprintf("save_data_%s", type)
#      stopifnot(existsFunction(fxn))
#      fxn
# }
# save_data <- function(data, type) {
#      fxn <- get_save_fxn(type)
#      do.call(fxn, list(data))
# }
# 
# # decide which function to use to load based on storage type
# get_load_fxn <- function(type) {
#      fxn <- sprintf("load_data_%s", type)
#      stopifnot(existsFunction(fxn))
#      fxn
# }
# load_data <- function(type) {
#      fxn <- get_load_fxn(type)
#      data <- do.call(fxn, list())
#      
#      # Just for a nicer UI, if there is no data, construct an empty
#      # dataframe so that the colnames will still be shown
#      if (nrow(data) == 0) {
#           data <-
#                matrix(nrow = 0, ncol = length(fields_all),
#                       dimnames = list(list(), fields_all)) %>%
#                data.frame
#      }
#      data %>% dplyr::arrange(desc(timestamp))
# }




# mandatory fields in the form
fields_post_mandatory <- c(
    "post_name", "post_category", "user_name", "email_address", "post"
)

fields_comment_mandatory <- c(
  "postID", "post_name", "novelty", "app_link", "interest", "comment_user_name", "comment_email_address", "comment" 
)

# fields_post_ed_mandatory <- c(
#   "post_name_ed", "post_category_ed", "user_name", "email_address", "post_ed"
# )

# all fields in the form we want to save
fields_post <- c(
  "timestamp", "postID", "edits", "currnet_views", "cumulative_views", "current_comments",
  "cumulative_comments", "timestamp_comment", "average_interest", fields_post_mandatory
)

fields_comment <- c(
  "timestamp2", "commentID", fields_comment_mandatory
)

# fields_post_ed <- c(
#   "timestamp", "postID", "edits", "currnet_views", "cumulative_views", "current_comments",
#   "cumulative_comments", "average_interest", fields_post_ed_mandatory
# )

# get current Epoch time
get_time_epoch <- function() {
     return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
get_time_human <- function() {
     format(Sys.time(), "%Y-%m-%d-%H:%M:%OS")
}


#### Method 5: Google Sheets ####
save_data_gsheets <- function(data, TABLE_NAME) {
 TABLE_NAME %>% gs_title %>% gs_add_row(input = data)
}

load_data_gsheets <- function(TABLE_NAME,...) {
  TABLE_NAME %>% gs_title %>% gs_read_csv(...)
}


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
retrieveComments <- function(N_comments, tmp_comments) {
  if (N_comments>0) {
    lapply(1:N_comments, function(i) {
      tmp_com_item  <- tmp_comments[i,] 
      wellPanel(
        p(tmp_com_item$comment,br(),
          " - ",tmp_com_item$comment_user_name, " posted on ",
          strtrim(tmp_com_item$timestamp2,10))
      )
    })
  } else { 
    (p("Leave the first comment for this idea!"))
  }
}    

