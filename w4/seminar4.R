#####################
## Seminar 4       ##
## Michal Kubista  ##
## 29 January 2018 ##
#####################

lapply(
      c("data.table","dplyr","magrittr","ggplot2",
        "purrr", "GGally", "cluster", "readxl", "tidyr"),
      require, character.only = T
      )

#-- PART 1 - kMeans vs hclust #################################################

#--- 1.1 ETL ------------------------------------------------------------------
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"
wholesale <- fread(url)
rm(url)

str(wholesale)
summary(wholesale)

wholesale %>%
      select(Channel, Region) %>% 
      map(unique)
      ## no need to recode

table(wholesale[,1:2])
ggpairs(wholesale[,-1:-2])

## LOG SCALLING
wholesale %>% 
      select(Fresh : Delicassen) %>% 
      map_df(log) %>% 
      cbind(wholesale[,1:2],.) -> wholeLg

ggpairs(wholeLg[,-1:-2])

## aggregation
wholeLg[,
          lapply(.SD, mean),
          by = .(Region),
          .SDcols = colnames(wholesale)[-1:-2]
          ]

wholeLg[,
        lapply(.SD, mean),
        by = .(Channel),
        .SDcols = colnames(wholesale)[-1:-2]
        ]

# OUTLIERS BEFORE AND AFTER
for (i in colnames(wholesale)[-1:-2]) {
      boxplot(wholesale[,i, with = FALSE], main = i)
}

for (i in colnames(wholeLg)[-1:-2]) {
      boxplot(wholeLg[,i, with = FALSE], main = i)
}

# outlier stalker
detect_outliers <- function(x){
      upperBound <- median(x) + 1.5 * IQR(x)
      lowerBound <- median(x) - 1.5 * IQR(x)
      which(x > upperBound | x < lowerBound)
}

detect_outliers(wholeLg$Milk)

## go get them
wholesale %>% 
      select(Fresh : Delicassen) %>% 
      map(detect_outliers) %>% 
      unlist -> outliers

outliers %>% table() %>%
      as.data.frame() %>% 
      .[order(.$Freq, decreasing = T),] %T>%
      View() -> outliers

outliers %>% 
      filter(Freq > 4) %>%
      .[,1] %>% 
      as.numeric() -> outIndex

wholeC <- wholeLg[-outIndex,]
rm(outliers, outIndex, detect_outliers)

#--- 1.2 kMeans ----------------------------------------------------------------
# elbow method
clusters <- c()
for (i in 1:10) {
     clusters <- c(clusters, kmeans(wholeC[,-1:-2],i)$tot.withinss)
      
}
plot(1:i, clusters, type = "b")

lines(c(3,10), clusters[c(3,10)], col = "green")
lines(c(4,10), clusters[c(4,10)], col = "red")
lines(c(5,10), clusters[c(5,10)], col = "blue")

(clusters / clusters[1]) %>% subtract(lag(.)) * 100
k <- 2

rm(i,clusters)

# visualisation
kModel <- kmeans(wholeC[,-1:-2],k)
kModel$centers
wholeC$clust <- kModel$cluster

ggplot(wholeC, aes(x = Channel, y = Region, color = as.factor(clust))) +
      geom_jitter()

for (i in 1:k) {
      print(wholeC[wholeC$clust == i,.(Channel, Region)] %>% table())
}

clusplot(wholeC, kModel$cluster, color = TRUE, shade = T, labels = 1)

#--- 1.2 hClust ----------------------------------------------------------------
wholeMat <- dist(wholeC[,-1:-2])

hModel <- hclust(wholeMat, method = "complete")
plot(hModel)

heights <-
      hModel$height %>% 
      .[order(.)]
      
(diff(heights,lag = 1) %>% which.max() %>% add(1) %>% heights[.] -> h)

rect.hclust(hModel, h = h)

ggplot(wholeC, aes(x = Channel, y = Region, color = as.factor(hcl))) +
      geom_jitter()

groups <- cutree(hModel, h = h - 0.1)

clusplot(wholeC[,-1:-2], groups , color = TRUE, 
         shade = T, labels = 1, lines = 0)

groups %>% table

#-- PART 2 - hclust & basket ##################################################
purchase <- read_xlsx("w2/data/online_retail.xlsx")
str(purchase)
summary(purchase)

purchase %<>% 
      filter(Quantity > 0, UnitPrice > 0, !is.na(CustomerID))

summary(purchase)

itemFreq <- as.data.table(purchase)[,.(freq = .N), by = .(CustomerID, StockCode)]
View(head(itemFreq, 500), "itemFreq")

custItem <- spread(itemFreq, StockCode, freq, fill = 0)

# colnames(custItem)[1:10]
rownames(custItem) <- custItem$CustomerID
custItem$CustomerID <- NULL

custItem <- as.matrix(custItem)
incidence <- custItem
incidence[incidence > 0] <- 1

items <- t(custItem) %*% incidence
# View(items[1:10,1:10])

items[items == 0] <- 0.1
items <- 1/items
diag(items) <- 0

itemSample <- items[1:500,1:500]

distMat <- dist(itemSample)
tree <- hclust(distMat, method = "complete")

pdf("dendogram.pdf", width = 120, height = 75)
plot(tree)
abline(h = 40, col = "red")
dev.off()

purchase %>% 
      # filter(StockCode %in% c("20667","21310","17028J","17001","20816")) %>% 
      filter(StockCode %in% c("20861", "16169P","17007B")) %>%
      select(StockCode, Description) %>%
      unique()

as.data.table(purchase)[,.(Qty = sum(Quantity)), by = StockCode] -> itemSales

