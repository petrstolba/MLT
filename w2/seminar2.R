#####################
## Seminar 2       ##
## Michal Kubista  ##
## 15 January 2018 ##
#####################

lapply(
      c("data.table","tidyverse","magrittr",
        "arules","arulesViz","readxl"),
      require,
      character.only = T
      )
#-- PART 1 - NAIVE BAYES #######################################################

#--- 1.1 ETL -------------------------------------------------------------------
prodTab <- fread("w2/data/prod_structure.csv")

## overview
str(prodTab)
# View(prodTab)
table(prodTab$category_name)
map(prodTab, ~length(unique(.)))


inject <- function(x){
      x %<>%
            strsplit(split = " ") %>% 
            unlist()
}

## splitting the product names
newCols <- apply(prodTab[,2], 1 ,inject) %>%
      do.call(rbind.data.frame,.)
colnames(newCols) <- c("desc1","desc2","desc3")

## binding together, all factors!
prodTab <- cbind(prodTab,newCols)
prodTab <- apply(prodTab,2,as.factor) %>% as.data.frame()

map(prodTab, ~length(unique(.)))

## train & test division
set.seed(123)
nrow(prodTab) %>% {sample(.,. * 0.50)} -> index
train <- prodTab[index,]
test <-prodTab[-index,]
rm(newCols, index)

#--- 1.2 LABELLING -------------------------------------------------------------
if(!require("e1071")){
      install.packages("e1071")
}

## training
bayes <- naiveBayes(category_name ~ ., train)

## prediction
test$lab <- predict(bayes, test)

## changin the columns
test <- test[,c(1,6,2:5)]
test$ok <- test$category_name == test$lab

## accuracy statistics
sum(test$ok)/nrow(test) * 100
table(test$category_name, test$lab)

rm(bayes, prodTab, test, train, inject)

#-- PART 2 - APRIORI ###########################################################

#--- 2.1 STRING INPUT ----------------------------------------------------------
##--- 2.1.1 ETL ----------------------------------------------------------------
download.file("http://fimi.ua.ac.be/data/retail.dat.gz", "w2/data/retail.dat.gz")

transRaw <- read.delim("w2/data/retail.dat.gz",
                stringsAsFactors = FALSE)

colnames(transRaw) <- "items"

## find unique items
strsplit(transRaw$items, split = " ") %>% 
      unlist() -> items
itemsUn <- unique(items)

## this will not work :/
mat <- matrix(0,nrow(transRaw),length(itemsUn))

## item frequencies
table(items) %>%
      as.data.frame() %>% 
      arrange(desc(Freq)) -> itemsFreq

summary(itemsFreq$Freq)

nrow(transRaw)/100

itemsFreq %>% 
      filter(Freq > 100) %>%
      .$items -> itemsCh

##____Since we will limit the support of the rules later in the training phase,
##____we can already omit some items. By omitting, I mean, not including them
##____as variables, not removing them from transactions (or even removing the
##____transactions). 
rm(items, itemsUn, itemsFreq)


inject <- function(raw){
      raw %>%
            strsplit(split = " ") %>%
            unlist() -> nonList
      
      index <- itemsCh %in% nonList %>% which()
      out <- rep(0, length(itemsCh))
      out[index] <- 1
      return(out)
}
#

# WHAT NOT TO DO! ----
# transMat2 <- matrix(0,nrow(transRaw), length(itemsCh))
# colnames(transMat2) <- itemsCh
# timeFor <- system.time({
#       for(i in seq_along(transRaw$items)){
#             transRaw$items[1] %>%
#                   strsplit(split = " ") %>%
#                   unlist() -> non_list
#             index <- itemsCh %in% non_list %>% which()
#             transMat2[i,index] <- 1
#             
#             print(paste(round(i/nrow(transRaw)*100,3),"%"))
#             flush.console()
#       }
# })
# rm(transMat2, i , index, non_list, timeFor, timeApply)
# ----

system.time({transMat <- t(sapply(transRaw$items, inject))})

colnames(transMat) <- itemsCh
rownames(transMat) <- 1:nrow(transMat)

## are the dimensions ok?
dim(transMat) == c(nrow(transRaw), length(itemsCh))

rm(transRaw, itemsCh, inject)

##--- 2.1.2 ASSOCIATIONS -------------------------------------------------------
model <- apriori(transMat, parameter = list(support = 0.01, confidence = 0.5))

inspect(model) %>%
      as.data.frame() -> ruleTab

plot(model)
plot(model, interactive = TRUE)

rm(model, ruleTab, transMat)

#--- 2.2 DATAFRAME INPUT -------------------------------------------------------
##--- 2.2.1 ETL ----------------------------------------------------------------
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx" 

download.file(url, "w2/data/online_retail.xlsx", mode = 'wb' )

transRaw <- read_xlsx("w2/data/online_retail.xlsx")
str(transRaw)

transRaw %<>% 
      select(InvoiceNo, Description)

## create a product table
prodUn <- transRaw$Description %>% unique()

prodTable <- cbind(Description = prodUn,
                   prodID = seq_along(prodUn)) %>%
      as.data.frame(stringsAsFactors = F)
rm(prodUn)

## create a transaction table 
transUn <- transRaw$InvoiceNo %>% unique()

transTable <-  cbind(InvoiceNo = transUn,
                     transID = seq_along(transUn)) %>%
      as.data.frame(stringsAsFactors = F)
rm(transUn)

## bind to the original table
transRaw %<>% 
      right_join(prodTable, by = "Description") %>% 
      right_join(transTable, by = "InvoiceNo")

## IDs as numeric
transRaw[,3:4] <- apply(transRaw[,3:4],2,as.numeric)

## create sparse matrix based on IDs
transMat <- sparseMatrix(i = transRaw$prodID,
                         j = transRaw$transID)

rownames(transMat) <- prodTable$Description
colnames(transMat) <- transTable$InvoiceNo

##--- 2.2.2 ASSOCIATIONS -------------------------------------------------------
model <- apriori(transMat, parameter = list(support = 0.02, confidence = 0.25))

model %>% inspect() %>% as.data.frame() -> test
rm(test)

## extract data manually
rules <- cbind(labels = labels(model), model@quality)
rules$lhs <- gsub("=>.*","", rules$labels)
rules$rhs <- gsub(".*=>","", rules$labels)

rules <- rules[,c("lhs","rhs","support","confidence","lift", "count")]
#View(rules)