---
title: "Get Tweets"
author: "Larissa Peixoto Gomes"
last revised: "30-10-2023"
---
  

# Get Tweets script with AcademicTwitter

##### X, formerly Twitter, has put academic access behind a paywall
##### This code was run before this took place

##### Packages

require(academictwitteR) 
require(jsonlite) 
require(purrr)
require(data.table) 
require(dplyr) 
require(stringr)


##### Establishes keyword, date for search query

query <- paste0('(','"brexit"',')', ' has:geo')

brexit <- get_all_tweets(query,
                         country = "GB",
                         start_tweets = "2016-06-01T00:00:00Z",
                         end_tweets = "2016-06-30T00:00:00Z",
                         n = Inf,
                         data_path = "brexit1",
                         bind_tweets = TRUE)



##### Joins tweets together into a data.frame

files <- list.files("brexit1", pattern = "^users")
user_content <- jsonlite::read_json(file.path("brexit1", files[1]))
places_content <- user_content$places
places_content[[1]]


tweets <- bind_tweets("brexit1")

##### Ensures locations are visible in data.frame 

dt_list <- map(places_content, as.data.table)
places_df <- rbindlist(dt_list, fill = TRUE, idcol = T)
places.df <-subset(places_df, geo!="Feature" & geo!="list()")


brexit$id <- unlist(brexit$geo$place_id)


brexit <- full_join(brexit, places.df, by = "id")

author <- subset(brexit, select = c(author_id, country))

author <- author[is.na(author$country), ]    

author <- distinct(author)

##### To find locations other than those geotagged users 

users <- as.list(author$author_id)

users1 <- get_user_profile(users)

users2 <- subset(users1, select = c(id, location, description))

author <- rename(author, id = author_id)

author2 <- full_join(author, users2)
author2 <- subset(author2, select = c(id, location, description))
author2 <- rename(author2, author_id = id)

brexit1 <- full_join(brexit, author2)

##### Categorise non-geotagged locations as either Wales or England
##### 
brexit1 <-  mutate(brexit1, Wales = if_else(str_detect(location, 'Wales'), 'Wales', NA_character_))
brexit1 <-  mutate(brexit1, England = if_else(str_detect(location, 'England'), 'England', NA_character_))
brexit1$nation <- with(brexit1, coalesce(Wales, England))

locations <- subset(brexit1, select = c(location, nation))
locations <- locations[is.na(locations1$nation), ]  
locations <- locations[!is.na(brexit1$location), ] 
locations <- distinct(locations)
fwrite(locations, "locations.csv", bom = TRUE)

locations1 <- locations
locations1$nation <- NULL
brexit1$location <- toupper(brexit1$location)
brexit1$location <- gsub("/", "", brexit1$location)



##### Example of how to categorise GPS coordinates and other location names 
##### 
brexit2 <- brexit1 %>% 
  mutate(nation2 = case_when(str_detect(brexit1$location,"51.073145 1.115688")~"England",
                             str_detect(brexit1$location,"51.211739 0.763813")~"England",
                             str_detect(brexit1$location,"51.27819 1.081325")~"England",
                             str_detect(brexit1$location,"FFOS Y GERDDINEN")~"Wales",
                             str_detect(brexit1$location,"TALACHARN")~"Wales",
                             str_detect(brexit1$location,"ABEROGWR")~"Wales",
                             str_detect(brexit1$location,"GILFACH")~"Wales"	))

