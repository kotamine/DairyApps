
source("global.R")

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(view = NULL, edit_auth = FALSE, edit_user_auth=FALSE,
                       selectedPost = NULL, selectedComments = NULL,
                       comment_reset =0, post_reset=0, 
                       reply_reset=0, msg_rest=0,
                       message_content = NULL,
                       back_to_selected_post = 0, 
                       back_to_selected_user = 0,
                       view_rest=0) 
  
  user_session <- reactiveValues(info = NULL, guest=FALSE)
  
  tables <- reactiveValues()
  
  browser()
  

  ## Connect to mongodb from a mongolab location
  host <- "ds061954.mongolab.com:61954"
  username <- "user1"
  password <- "user1"
  db <- "app_rumen"
  url <- paste0("mongodb://",username,":",password,"@", host, "/",db)
  
#   stop('restructuring mongo db') 
#   
  mongo_posts <- mongo(collection="posts", db=db, url = url)
  mongo_comments <- mongo(collection="comments", db=db, url = url)
  # mongo_archive_posts <- mongo(collection="archive_posts", db=db, url = url)
  # mongo_archive_comments <- mongo(collection="archive_comments", db=db, url = url)
  # mongo_completed_posts <- mongo(collection="completed_posts", db=db, url = url)
  # mongo_resolved_posts <- mongo(collection="resolved_posts", db=db, url = url)
  # mongo_discontinued_posts <- mongo(collection="discontinued_posts", db=db, url = url)
  mongo_users <- mongo(collection="users", db=db, url = url)
  mongo_likes <- mongo(collection="likes", db=db, url = url)
  mongo_follow_post <- mongo(collection="follow_post", db=db, url = url)
  mongo_follow_user <- mongo(collection="follow_user", db=db, url = url)
  mongo_messages <-   mongo(collection="messages", db=db, url = url)
  # mongo_system_use <-   mongo(collection="system_use", db=db, url = url)
#   
#   # browser()
  mongo_posts$index('{"post_category":1}')
  
  source(file.path("session_files","helpers.R"), local=TRUE)
  
  source(file.path("session_files","session_populate.R"), local=TRUE)
  
#   source(file.path("session_files","session_post_comment.R"), local=TRUE)
#   
  source(file.path("session_files","session_details_edit.R"), local=TRUE)
#   
#   source(file.path("session_files","session_user_edit.R"), local=TRUE)
#   
#   source(file.path("session_files","session_notice.R"), local=TRUE)
#   
#   source(file.path("session_files","session_notification.R"), local=TRUE)
#   
   source(file.path("session_files","session_misc.R"), local=TRUE)
#   
#   
  
}) 


