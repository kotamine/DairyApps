library(mongolite)


get_time_epoch <- function() {
  return(as.integer(Sys.time()))
}

get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d-%H:%M:%OS")
}

host <- "ds019980.mlab.com:19980" 
username <- "user1"
password <- "user1"
db <- "robot_parlor"

url <- paste0("mongodb://",username,":",password,"@", host, "/",db)

mongo_test <-  mongo(collection="test", db=db, url = url)


new_row <- data.frame(
  id = get_time_epoch(),
  timestamp = get_time_human() 
)

new_row$user <- data.frame(username="new user",
                           email="test@test.org")

mongo_test$insert(new_row)

mongo_test$find('{}')


