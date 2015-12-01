
library(rmongodb)

host <- "mongolab.com:39484"
username <- "AppRumen"
password <- "12345"
db <- "posts"

mongo <- mongo.create(host=host , db=db, username=username, password=password)


