#####################
## Seminar 3       ##
## Michal Kubista  ##
## 20 January 2020 ##
#####################

path2data = "w3/data"

if (!dir.exists(path2data)) {
    dir.create(path2data, recursive = T)
}

setwd("w3/data")
## very, very bad practise!

sapply(
    c("data.table","dplyr","magrittr","ggplot2", "purrr"),
    require, character.only = T)

#-- PART 1 - ETL ##############################################################

#--- 1.1 DATA DOWNLOAD --------------------------------------------------------
# url = "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip"
# download.file(url, "data.zip")
# unzip("data.zip")
# rm(url)
# 
# list.files()
# newFiles = list.files("ml-latest-small", full.names = TRUE)
# 
# ## move the files
# for (i in newFiles) {
#     new = gsub("ml-latest-small/","",i)
#     file.rename(i, new)
# }
# 
# readLines("README.txt") %>% 
#     gsub("-*","",.) %>% 
#     gsub("=*","",.) %>% 
#     .[!. == ""] 
# 
# ## clean the directory
# rm(i, newFiles, new)
# file.remove(c("data.zip","links.csv", "README.txt"))
# unlink("ml-latest-small", recursive = TRUE)

#########################################################################
# DOWNLOAD DATA AT
# https://drive.google.com/open?id=1t4OJgz9c2d_ki0YS0owBC8Ic_kCHCR6v
#########################################################################

## read input
input = map(list.files(), fread, data.table = F)
names(input) = list.files() %>% gsub(".csv","",.)

#--- 1.2 TAGS EXPLORATION -----------------------------------------------------
tags = input$tags

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

input$tags = NULL
rm(tags)

#--- 1.3 MOVIES EXPLORATION ---------------------------------------------------
movies = input$movies

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
reset_ID = function(ids){
    ids[ids == "65665"] = "3598"
    ids[ids == "64997"] = "34048"
    return(ids)
}

movies %<>%
    filter(movieId != "65665" & movieId != "64997")

## list of genres
movies$genres %>% 
    strsplit("\\|") %>%
    unlist() %>% 
    unique() -> genres

## helper function 
set_genre = function(x){
    x %>% 
        strsplit("\\|") %>% 
        unlist -> 
        genreCache
    
    genres %in% genreCache %>% 
        as.numeric()
}

## genres incidence matrix
map(movies$genres, set_genre) %>% 
    unlist() %>% 
    matrix(nrow(movies),length(genres), byrow = T) -> genreTable

colnames(genreTable) = genres

## add genres to movies
movies = cbind(movies, genreTable)

rm(genres, genreTable, set_genre)

#--- 1.4 RATING EXPLORATION ---------------------------------------------------
ratings = input$ratings

## basic overview
dim(ratings)
ratings %>% str
ratings %>% summary

## column format
ratings[,1:2] =  apply(ratings[,1:2],2, as.character)

ratings$timestamp %<>% as.POSIXct(origin = "1970-01-01")
summary(ratings$timestamp)

## unique per column
map(ratings, ~length(unique(.)))

## reset ID of duplicated movies
ratings$movieId %<>% 
    reset_ID

## check integrity
anti_join(ratings, movies, by = "movieId")

rm(reset_ID)

#-- PART 2 - kNN: CONTENT BASED RECOMMENDER ###################################

#--- 2.1 PREPARE DATA ---------------------------------------------------------
## rating frequency
ratings$userId %>% 
    table() %>% 
    as.data.frame() %>% View()

## choose user
ratings %>% 
    filter(userId == "547") -> rat547

## user's rating overview
rat547 %>% summary()
seq(0.5,5,by = 0.5) %>% summary()

qqplot(seq(0.5,5,by = 0.5), rat547$rating)
lines(0:5,0:5)

## rating over time
ggplot(rat547, aes(x = timestamp, y = rating)) +
    geom_line() +
    geom_smooth(method = "loess")

## choose threshold
## this is a parametr!!!
    ## it's choice needs discussion
rat547$class = ifelse(rat547$rating > 4, 1, 0)
mean(rat547$class)

if (!require(class)) {
    install.packages("class")
    library(class)
}

movies547 = left_join(movies,rat547[,c("movieId","class")])
colnames(movies547)

moviesTrain = movies547 %>% filter(!is.na(class))
moviesTest = movies547 %>% filter(is.na(class))

#--- 2.2 CROSS-VALIDATION: FIND THE RIGHT "k" ---------------------------------
set.seed(1234)
indexTrain1 = sample(1:nrow(moviesTrain),1500)

moviesTrain1 = moviesTrain[indexTrain1,]
moviesTrain2 = moviesTrain[-indexTrain1,]

result = c()
for (i in 1:100) {
    moviesTrain2$new =
        knn(moviesTrain1[,4:22],
            moviesTrain2[,4:22],
            moviesTrain1[,"class"],
            i)
    
    result[i] = mean(moviesTrain2$new == moviesTrain2$class)
    print(result[i])
}

plot(1:i, result, type = "l")
k = which.max(result)
k = 2

rm(result, moviesTrain1, moviesTrain2, indexTrain1, i)

##--- 2.3 RECOMMEND -----------------------------------------------------------

moviesTest$new =
    knn(moviesTrain[,4:22], moviesTest[,4:22], moviesTrain[,"class"], k)

movies547[,4:22] %>% map_dbl(mean) %>% .[order(., decreasing = T)]

moviesTest[moviesTest$new == 1,] %>% View()

movies547[moviesTest$new == 1,4:22] %>%
    map_dbl(mean) %>%
    .[order(., decreasing = T)]

rm(moviesTest, moviesTrain, movies547, rat547, k)

##-- PART 3 - COLLABORATIVE FILTERING #########################################

##--- 3.1 - PREPARATIONS ------------------------------------------------------
if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
}

ratings[c(99041,99132),]
ratings = ratings[-99132,]

ratings %>% 
    mutate(class = ifelse(rating > 4, 1, 0)) %>% 
    select(userId, movieId, class) %>%  
    spread(key = userId, value = class, fill = 0) -> ratM 

rownames(ratM) = ratM$movieId
ratM$movieId = NULL

ratM %<>% as.matrix()

##--- 3.2 - ITEM-ITEM ---------------------------------------------------------
## way too much for my PC, need to filter
ratMR = ratM[1:1000,]

## factorisation
item = ratMR %*% t(ratMR) 

## erase self-incidence
diag(item) = 0

## choose an user (287 works well)
user = ratings[sample(1:100000,1), "userId"]

## user movies
ratings %>% 
    filter(userId == user) %>% 
    select(movieId) %>% 
    pull(movieId) -> userMov

## find movies in matrix and filter
rownames(item) %in% userMov %>% which() -> indexCR
itemChoice = item[indexCR,]

## for each movie, find the most similar one
apply(itemChoice, 1, which.max) %>% table -> bestPicks
bestPicks

## find the best recommendation
bestPicks %>% which.max() %>% rownames(bestPicks)[.] -> no1
movies %>% 
    filter(movieId == no1)

movies %>%
    filter(movieId %in% userMov) %>% 
    .[,4:22] %>% 
    map_dbl(mean) %>% 
    {.[order(., decreasing = T)]}

## what has the user already seen?
movies %>%
    filter(movieId %in% userMov) %>% View()

ratings[ratings$userId == user & ratings$movieId == no1,]


rm(ratMR, item, userMov, indexCR, itemChoice, bestPicks, no1, user)

##--- 3.3 - USER - USER -------------------------------------------------------
user = t(ratM) %*% ratM
diag(user) = 0

## choose one user (323)
userID = rownames(user)[sample(1:600,1)]

## most similar users
user[userID,] %>% table() %>% rev() -> bestPicks
bestPicks

## at least 10 users
    ## parameter again!
index = which.max(cumsum(bestPicks) >= 10)
names(bestPicks) %>% as.numeric() %>% .[seq_len(index)] -> index

user[userID,] %in% index %>% which() %>% rownames(user)[.] -> userSim

## movies watched by similar users 
(ratings %>% 
    filter(userId %in% userSim) %>%
    select(movieId) %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE) %>% 
    arrange(desc(Freq)) -> recom)

## recommended movies
View(movies %>% 
         filter(movieId %in% recom[1:10,1]))

## what has the user watched
movies[movies$movieId %in% ratings[ratings$userId == userID,"movieId"],] %>%
    View()
