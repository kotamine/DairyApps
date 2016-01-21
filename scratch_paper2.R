
library(rmongodb)
library(mongolite)

library(rmongodbHelper)

host <- "ds039484.mongolab.com:39484"
username <- "newuser"
password <- "newuser"
db <- "posts"

## Connect to mongodb
mongo <- mongo.create(host=host, db=db, 
                      username=username, password=password)


url <- "mongodb://newuser:newuser@ds039484.mongolab.com:39484/posts" 

mongo <- mongo(db=db, 
               url = "mongodb://newuser:newuser@ds039484.mongolab.com:39484/posts")


mongo <- mongo.create(host=host, db=db)
mongo.is.connected(mongo)


mongo.authenticate(mongo,db=db, 
                      username=username, password=password)


mongo.get.databases(mongo)

mongo.get.database.collections(mongo, db)



library("rmongodb")
demo(teachers_aid)

tmp = mongo.find.one(mongo, ns ="test")
tmp

mongo <- mongo.create(host="dbs001.mongosoup.de",db="cc_JwQcDLJSYQJb", "JwQcDLJSYQJb", "RSXPkUkXXXXX")


mongo <- mongo.create(host="ds039484.mongolab.com:39484", db="posts", username="newuser", password="newuser")



loadData <- function() {
  # Connect to the database
  db <- mongo.create(db = databaseName, host = options()$mongodb$host, 
                     username = options()$mongodb$username, password = options()$mongodb$password)
  # Get a list of all entries
  data <- mongo.find.all(db, collectionName)
  # Read all entries into a list
  data <- lapply(data, data.frame, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame 
  data <- do.call(rbind, data)
  # Remove the ID variable
  data <- data[ , -1, drop = FALSE]
  # Disconnect
  mongo.disconnect(db)
  data
}





library(devtools)
install_github(repo = "mongosoup/rmongodb")

library(rmongodb)
# connect to MongoDB
mongo = mongo.create(host = "localhost")
mongo.is.connected(mongo)

mongo.get.databases(mongo)
mongo.get.database.collections(mongo, db = "rmongodb_sample")


DBNS = "rmongodb_sample.students"
mongo.count(mongo, ns =DBNS)


install.packages("jSonarR")
install.packages('rjson')
install.packages('lsa')

library('jSonarR') 
library(rjson)
library(lsa)

con <- jSonarR::new.SonarConnection('https://localhost:8443', 'localhost',
                                    'steam', port=47017, username="qa1", pwd="qa1")





game_list = jSonarR::sonarJSON(con, 'distinct_game_id', 'steam1', type='agg')
columns = length(game_list)
lines = jSonarR::sonarJSON(con, 'steam_data', 'steam1', type='agg')
rows = length(lines)

game_vector = integer(columns)
for (i in 1:columns){
  current_game = game_list[[i]]
  game_vector[i] = current_game['_id']
}
m = matrix(nrow=rows,ncol=columns)
rownames(m) = 1:rows
colnames(m) = game_vector

for (i in 1:rows) {
  current_row = lines[[i]]
  rownames(m)[i] = current_row[['_id']]
  total_games = length(current_row[['games']])
  for (j in 1:total_games) {
    m[toString(current_row[['_id']]),toString(
      current_row[['games']][[j]][['game']])] =
      current_row[['games']][[j]][['weighted_playtime']]
  }   
}
m[is.na(m)]=0


# Calculates the cosine measure between the columns of a matrix;
# We transpose so that we get the measure between users
# This is from the Latent Semantic Analysis (lsa) package
rec = cosine(t(m))
rec[is.nan(rec)] = 0
# This returns an NxN matrix - i.e. it compares each user with every other user
# Cell X,Y for example contains the cosine similarity between user X and user Y
# So now let's get the recommendations for the first user as an example
# Use the similarity values for the first user
# (i.e. how similar that user is to every other user)
# Multiply it with the full game matrix - so similar users
# will have a larger effect on recommendations
first_row_rec = rec[1,] %*% m
ones = matrix(nrow=1,ncol=dim(m)[1])
ones[is.na(ones)] = 1
total_rec = ones %*% m
weighted_first_row_rec = first_row_rec / total_rec
weighted_first_row_rec[is.nan(weighted_first_row_rec)] = 0
# So these are the top 10 recommended games for our first user
top_10_recs = colnames(weighted_first_row_rec)[
  tail(order(weighted_first_row_rec),10)]

top_10_recs



con <- jSonarR::new.SonarConnection('http://media.mongodb.org/zips.json?_ga=1.82342934.1055147906.1448982651',
                                    'localhost')

con <- jSonarR::new.SonarConnection('http://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20130808 15:00&end_date=20130808 15:06&station=8454000&product=water_temperature&units=english&time_zone=gmt&application=ports_screen&format=json') 





con <- jSonarR::new.SonarConnection("https://example.com","localhost","test") 
ny_by_day <- jSonarR::sonarAgg(con, 'delays_by_day','NYCFlights')

summary(ny_by_day)

ipo <- jSonarR::sonarAgg(con, 'ipo_funded_companies', 'companies', idCol='ipo.stock_symbol')

ipo2 <- jSonarR::sonarAgg(con, 'ipo_funded_companies', 'companies') 

large <- jSonarR::sonarFind(con, 'find1', 'companies', bind=list(number_of_employees=6000), idCol='_id')

nyc_by_day <- jSonarR::sonarAgg(con, 'delays_by_day', 'NYCFlights',
                                colClasses=c(X_avg_AirTime='numeric', X_avg_ArrDelay='numeric',X_avg_DepDelay='numeric'))



connection <- jSonarR::new.SonarConnection("http://media.mongodb.org/zips.json?_ga=1.82342934.1055147906.1448982651","localhost","test") 

length(connection)




##-----------------------
library(mongolite)
m <- mongo(collection = "diamonds")

# Insert test data
data(diamonds, package="ggplot2")
m$insert(diamonds)

# Check records
m$count()
nrow(diamonds)

# Perform a query and retrieve data
out <- m$find('{"cut" : "Premium", "price" : { "$lt" : 1000 } }')

# Compare
nrow(out)
nrow(subset(diamonds, cut == "Premium" & price < 1000))

# Cross-table
tbl <- m$mapreduce(
  map = "function(){emit({cut:this.cut, color:this.color}, 1)}",
  reduce = "function(id, counts){return Array.sum(counts)}"
)
# Same as:
data.frame(with(diamonds, table(cut, color)))

# Stream jsonlines into a connection
tmp <- tempfile()
m$export(file(tmp))

# Stream it back in R
library(jsonlite)
mydata <- stream_in(file(tmp))

# Or into mongo
m2 <- mongo("diamonds2")
m2$count()
m2$import(file(tmp))
m2$count()

# Remove the collection
m$drop()
m2$drop()


# Insert some data
data(flights, package = "nycflights13")
m <- mongo(collection = "nycflights")
m$insert(flights)

# Basic queries
m$count('{"month":1, "day":1}')
jan1 <- m$find('{"month":1, "day":1}')

# Sorting
jan1 <- m$find('{"month":1,"day":1}', sort='{"distance":-1}')
head(jan1)

# Sorting on large data requires index
m$index(add = "distance")
allflights <- m$find(sort='{"distance":-1}')
head(allflights)

# Select columns
jan1 <- m$find('{"month":1,"day":1}', fields = '{"_id":0, "distance":1, "carrier":1}')

# List unique values
m$distinct("carrier")
m$distinct("carrier", '{"distance":{"$gt":3000}}')

# Tabulate
m$aggregate('[{"$group":{"_id":"$carrier", "count": {"$sum":1}, "average":{"$avg":"$distance"}}}]')

# Map-reduce (binning)
hist <- m$mapreduce(
  map = "function(){emit(Math.floor(this.distance/100)*100, 1)}", 
  reduce = "function(id, counts){return Array.sum(counts)}"
)

# Dump to bson
dump <- tempfile()
m$export(file(dump), bson = TRUE)

# Remove the collection
m$drop()

# Restore
m$count()
m$import(file(dump), bson = TRUE)
m$count()



library(jsonlite)
library(mongolite)

# Stream from url into mongo
m <- mongo("zips")
stream_in(url("http://media.mongodb.org/zips.json"), handler = function(df){
  m$insert(df)
})

# Check count
m$count()

# Import. Note the 'location' column is actually an array!
zips <- m$find()
head(zips)

## Huge data
m <- mongo("weather")
stream_in(gzcon(url("http://78.46.48.103/sample/daily_14.json.gz")), handler = function(df){
  m$insert(df)  
}, pagesize = 50)

berlin <- m$find('{"city.name" : "Berlin"}')
print(berlin$data)

all_cities <- m$find()
head(all_cities$city[2]); count(all_cities$city[2])
head(all_cities$time); length(all_cities$time)
head(all_cities[[1]]);length(all_cities$data)

### ----------------------

library(mongolite) 

host <- "ds039484.mongolab.com:39484" 
username <- "newuser" 
password <- "newuser" 
db <- "posts" 
collection="restaurants"

## Connect to mongodb from a mongolab location 
url <- paste0("mongodb://",username,":",password,"@", host, "/",db) 
data1 <- mongo(collection=collection, db=db, url = url)
data1$count()

## Convert all data into a dataframe 
m0 <- data1$find()
head(m0)

## Convert some query into a dataframe
m0b <- data1$find('{"borough" : "Manhattan", "cuisine" : "Indian" }')  
head(m0b)

data1$index(add = "name")
m0c <- data1$find('{"borough" : "Manhattan", "cuisine" : "Indian" }',
                  sort='{"name":1}', 
                  fields = '{"_id":0, "name":1, "borough":1, "cuisine":1}')
head(m0c)


# Cross-table: summing up counts
tbl1 <- data1$mapreduce(
  map = "function(){emit({borough:this.borough, cuisine:this.cuisine}, 1)}",
  reduce = "function(id, counts){return Array.sum(counts)}"
)
head(tbl1)


# Insert: This will overwrite the dataset in the mongolab location
dim(m0)
data1$insert(m0b)
m0d <- data1$find()
dim(m0d)

## Reset the data from Terminal: 
## mongoimport -h ds039484.mongolab.com:39484 -d posts  -c restaurants -u newuser -p newuser --file /data/db/primer-dataset.json --drop



## --------------------------------------------------
# This goes into the Terminal

## csv file must be saved as "Windows Comma Separated(.csv)" format 
# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c posts -u user1 -p user1 --file /data/db/app_rumen/posts.csv --headerline --type csv --drop 

# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c comments -u user1 -p user1 --file /data/db/app_rumen/comments.csv --headerline --type csv --drop 

# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c archive_posts -u user1 -p user1 --file /data/db/app_rumen/archive_posts.csv --headerline --type csv --drop 

# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c archive_comments -u user1 -p user1 --file /data/db/app_rumen/archive_comments.csv --headerline --type csv --drop 

# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c system_use -u user1 -p user1 --file /data/db/app_rumen/system_use.csv --headerline --type csv --drop 

#  mkdir /Volumes/iMacSATA/data/db/app_rumen
mongoexport -h ds061954.mongolab.com:61954 -d app_rumen -u user1 -p user1 -c posts 
-f timestamp,postID  
-o /Volumes/iMacSATA/data/db/app_rumen/posts.csv 

mongoexport -h ds061954.mongolab.com:61954 -d app_rumen -u user1 -p user1 -c messages -f "message_id,timestap" -o /Volumes/iMacSATA/data/db/app_rumen/messages.csv 

$ mongoexport -h ds061954.mongolab.com:61954 -d app_rumen -c domain -o domain-bk.json


# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c posts -u user1 -p user1 --file /Volumes/iMacSATA/data/db/app_rumen/posts.csv --headerline --type csv --drop 

# mongoimport -h ds061954.mongolab.com:61954 -d app_rumen  -c comments -u user1 -p user1 --file /Volumes/iMacSATA/data/db/app_rumen/comments.csv --headerline --type csv --drop 

mongoimport -h ds061954.mongolab.com:61954 -d app_rumen -c users -u user1 -p user1 --file /Users/kota/Dropbox/working_projects/app_rumen/users.csv --headerline --type csv --drop 

mongoimport -h ds061954.mongolab.com:61954 -d app_rumen -c likes -u user1 -p user1 --file /Users/kota/Dropbox/working_projects/app_rumen/likes.csv --headerline --type csv --drop 

mongoimport -h ds061954.mongolab.com:61954 -d app_rumen -c posts -u user1 -p user1 --file /Users/kota/Dropbox/working_projects/app_rumen/posts.csv --headerline --type csv --drop 

mongoimport -h ds061954.mongolab.com:61954 -d app_rumen -c comments -u user1 -p user1 --file /Users/kota/Dropbox/working_projects/app_rumen/comments.csv --headerline --type csv --drop 


mongo_messages$update("{}",paste0('{"$set": { "title": "some title"}}'),multiple=TRUE) 

mongo_posts$update("{}",'{"$set": {"completeness": 50} }',multiple=TRUE)
mongo_posts$update("{}",'{"$set": {"likes": 0} }',multiple=TRUE) 
mongo_comments$update("{}",'{"$set": {"viewed": 0} }',multiple=TRUE) 

mongo_follow_post$update("{}",'{"$set": {"viewed": 0} }',multiple=TRUE) 

mongo_posts$find()[1,]


# --------------------------------

host <- "ds061954.mongolab.com:61954" 
username <- "user1" 
password <- "user1" 
db <- "app_rumen" 

## Connect to mongodb from a mongolab location 
url <- paste0("mongodb://",username,":",password,"@", host, "/",db) 
data1 <- mongo(collection="posts", db=db, url = url)$find()


data1 <- mongo(collection="posts", db=db, url = url)
#data1$update('{"postID":1445138342}', update='{"$set":{ "current_views": 6 }}' )
data1$find('{"postID":1445138342}')  

data1_all <- data1$find()

write.table(data1_all, "/Volumes/iMacSATA/data/db/app_rumen/posts.csv", sep=",", row.names = FALSE) 

myMongoExportCsv <- function(db, url, location, collection_name, field=NULL) {

  if (is.null(field)) d1 <- mongo(collection=collection_name, db=db, url = url)$find()
  else                d1 <- mongo(collection=collection_name, db=db, url = url)$find(field)
  
  file <- paste0(location,collection_name, ".csv")
  write.table(d1, file, sep=",",row.names = FALSE)
} 

loc_app_rumen <- "/Volumes/iMacSATA/data/db/app_rumen/"
myMongoExportCsv(db, url, loc_app_rumen, "posts")
myMongoExportCsv(db, url, loc_app_rumen, "comments")
myMongoExportCsv(db, url, loc_app_rumen, "archive_posts")
myMongoExportCsv(db, url, loc_app_rumen, "archive_comments")

myMongoExportCsv(db, url, loc_app_rumen, "completed_posts")
myMongoExportCsv(db, url, loc_app_rumen, "resolved_posts")
myMongoExportCsv(db, url, loc_app_rumen, "discontinued_posts")



data_users <- mongo(collection="users", db=db, url = url)$find()






new_row <- data1[1,] %>% rbind() %>% as.data.frame() 
colnames(new_row) <- colnames(data1)

new_row <- data1[1,] %>% matrix(nrow=1) %>% c() %>% as.data.frame() 

colnames(new_row) <- colnames(data1) 

new_row <-data.frame(lapply(new_row, function(x) t(data.frame(x))))

new_row2 <- data1[1,] 


mongo(collection="posts", db=db, url = url)$insert(new_row)


postID <- paste0('{"postID":', 1445138342, '}')
data1$find(postID)


