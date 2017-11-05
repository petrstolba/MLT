#####################
## Seminar 3       ##
## Michal Kubista  ##
## 23 January 2018 ##
#####################

setwd("w2/data")
lapply(c("data.table","dplyr","magrittr","ggplot2", "purrr"), require, character.only = T)

#-- PART 1 - ETL ##############################################################

#--- 1.1 DATA DOWNLOAD --------------------------------------------------------
url <- "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip"
download.file(url, "data.zip")
unzip("data.zip")
rm(url)

list.files()
newFiles <- list.files("ml-latest-small", full.names = TRUE)

for(i in newFiles){
      new <- gsub("ml-latest-small/","",i)
      file.rename(i, new)
}

rm(i)
rm(newFiles)

file.remove(c("data.zip","links.csv","README.txt"))
unlink("ml-latest-small", recursive = TRUE)

input <- map(list.files(),fread, data.table = F)
names(input) <- list.files() %>% gsub(".csv","",.) 

#--- 1.2 TAGS EXPLORATION -----------------------------------------------------
tags <- input$tags

## basic overview
dim(tags)
tags %>% str
tags %>% summary

tags$timestamp %<>% as.POSIXct(origin = "1970-01-01")

map(tags, ~length(unique(.)))

## table & frequency
tags$userId %>% 
      table %T>%
      hist() %>%
      as.data.frame() %>% 
      select(Freq) %>% 
      summary()

tags$tag %>% head()

tags$tag %>% 
      table() %>% 
      as.data.frame() %>% View()

input$tags <- NULL
rm(tags)

#--- 1.3 MOVIES EXPLORATION ---------------------------------------------------
movies <- input$movies

## basic overview
dim(movies)
movies %>% str
movies %>% summary

movies$movieId %<>% as.character()

map(movies, ~length(unique(.)))

## find non-unique titles
movies$title %>% 
      table %>% 
      as.data.frame() %>%
      arrange(desc(Freq)) %>% 
      head(2)

movies %>% 
      filter(title == "Hamlet (2000)")

movies %>% 
      filter(title == "War of the Worlds (2005)")

## set for later
reset_ID <- function(ids){
      ids[ids == "65665"] <- "3598"
      ids[ids == "64997"] <- "34048"
      return(ids)
}

movies %<>%
      filter(movieId != "65665" & movieId != "64997")

## feature engineering
movies$genres %>% 
      strsplit("\\|") %>%
      unlist() %>% 
      unique() -> genres

## helper function 
set_genre <- function(x){
      x %>% 
            strsplit("\\|") %>% 
            unlist -> 
            genreCache
      
      genres %in% genreCache %>% 
            as.numeric()
}

map(movies$genres, set_genre) %>% 
      unlist() %>% 
      matrix(nrow(movies),length(genres), byrow = T) -> genreTable

colnames(genreTable) <- genres

movies <- cbind(movies, genreTable)

rm(genres, genreTable, set_genre)

#--- 1.4 RATING EXPLORATION ---------------------------------------------------
ratings <- input$ratings

## basic overview
dim(ratings)
ratings %>% str
ratings %>% summary

ratings[,1:2] <-  apply(ratings[,1:2],2, as.character)

ratings$timestamp %<>% as.POSIXct(origin = "1970-01-01")
summary(ratings$timestamp)

map(ratings, ~length(unique(.)))

ratings$movieId %<>% 
      reset_ID

anti_join(ratings, movies, by = "movieId")

rm(reset_ID)

#-- PART 2 - kNN: CONTENT BASED RECOMMENDER ###################################

#--- 2.1 PREPARE DATA ---------------------------------------------------------
ratings$userId %>% 
      table() %>% 
      as.data.frame() %>% View()

ratings %>% 
      filter(userId == "547") -> rat547

rat547 %>% summary()
seq(0.5,5,by=0.5) %>% summary()

qqplot(rat547$rating, seq(0.5,5,by=0.5))
lines(0:5,0:5)

ggplot(rat547, aes(x = timestamp, y = rating))+
      geom_line()+
      geom_smooth(method = "loess")

rat547$class <- ifelse(rat547$rating > 4, 1,0)
mean(rat547$class)

if(!require(class)){
      install.packages("class")
}

movies547 <- left_join(movies,rat547[,c("movieId","class")])
colnames(movies547)

moviesTrain <- movies547 %>% filter(!is.na(class))
moviesTest <- movies547 %>% filter(is.na(class))

#--- 2.2 CROSS-VALIDATION: FIND THE RIGHT "k" ---------------------------------
set.seed(1234)
indexTrain1<- sample(1:nrow(moviesTrain),1500)

moviesTrain1 <- moviesTrain[indexTrain1,]
moviesTrain2 <- moviesTrain[-indexTrain1,]

result <- c()
for(i in 1:160){
      moviesTrain2$new <- knn(moviesTrain1[,4:22], moviesTrain2[,4:22], moviesTrain1[,"class"], i)
      
      result[i] <-mean(moviesTrain2$new == moviesTrain2$class)
      print(result[i])
}

plot(1:i, result, type = "l")
k <- which.max(result)
k

rm(result, moviesTrain1, moviesTrain2, indexTrain1, i)

##--- 2.3 RECOMMEND -----------------------------------------------------------

moviesTest$new<- knn(moviesTrain[,4:22], moviesTest[,4:22], moviesTrain[,"class"], k)

movies547[,4:22] %>% map_dbl(mean) %>% .[order(., decreasing = T)]

moviesTest[moviesTest$new ==1,] %>% View()

rm(moviesTest, moviesTrain, movies547, rat547, k)

##-- PART 3 - COLLABORATIVE FILTERING #########################################

##--- 3.1 - PREPARATIONS ------------------------------------------------------
if(!require(tidyr)){
      install.packages("tidyr")
}

ratings[c(99041,99132),]
ratings <- ratings[-99132,]

ratings %>% 
      mutate(class = ifelse(rating > 4, 1, 0)) %>% 
      select(userId, movieId, class) %>%  
      spread(key = userId, value = class, fill = 0) -> ratM 

rownames(ratM) <- ratM$movieId
ratM$movieId <- NULL

ratM %<>% as.matrix()

##--- 3.2 - ITEM-ITEM ---------------------------------------------------------
ratMR <- ratM[1:1000,]
## way too much for my PC

item <- ratMR %*% t(ratMR) 
diag(item) <- 0

user <- ratings[sample(1:100000,1), "userId"]
# 152

ratings %>% 
      filter(userId == user) %>% 
      select(movieId) -> userMov

userMov <- userMov$movieId

rownames(item) %in% userMov %>% which() -> indexCR

itemChoice <- item[indexCR,indexCR] 

apply(itemChoice,1,which.max) %>% table -> bestPicks
bestPicks

bestPicks %>% which.max() -> no1
movies %>% 
      filter(movieId == no1)

movies %>%
      filter(movieId %in% userMov) %>% View()

rm(ratMR, item, userMov, indexCR, itemChoice, bestPicks, no1, user)

##--- 3.3 - USER - USER -------------------------------------------------------
user <- t(ratM) %*% ratM
diag(user) <- 0

userID <- rownames(user)[sample(1:600,1)]
#323

user[,userID] %>% table() -> bestPicks
bestPicks

user[,userID] %in% c(6,7) %>% which() -> userSim

ratings %>% 
      filter(userId %in% userSim) %>%
      select(movieId) %>%
      table() %>%
      as.data.frame(stringsAsFactors = FALSE) %>% 
      arrange(desc(Freq)) -> recom
      
View(movies %>% 
      filter(movieId %in% recom[1:10,1]))


movies[movies$movieId %in% ratings[ratings$userId == userID,"movieId"],] %>% View()