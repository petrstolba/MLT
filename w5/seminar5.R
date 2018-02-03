#####################
## Seminar 5       ##
## Michal Kubista  ##
## 5 February 2018 ##
#####################

# install.packages("rpart")
# install.packages("rattle")

sapply(
    c("data.table","dplyr","magrittr","ggplot2",
      "purrr", "GGally", "readxl", "rpart", "rattle"),
    require, character.only = T
)


# BASKET ANALYSIS BASICS

# ETL
# data from https://community.tableau.com/docs/DOC-1236
url <- "https://community.tableau.com/servlet/JiveServlet/downloadBody/1236-102-2-15278/Sample%20-%20Superstore.xls"
dir.create("w5/data")
download.file(url, "w5/data/transac.xls", mode = "wb")
rm(url)

raw <- read_excel("w5/data/transac.xls") %>% as.data.table()
str(raw)
summary(raw)

map(raw, ~length(unique(.)))

colnames(raw)

# CUSTOMER TABLE
cust <- raw[,.(cust_name = unique(`Customer Name`),
               segment = unique(Segment)
               ),
            by = "Customer ID"]
setnames(cust, "Customer ID", "cust_ID")

# PRODUCTS TABLE
prod <- raw[,.(cat = unique(Category),
               subcat = unique(`Sub-Category`),
               desc = unique(`Product Name`)),
            by = "Product ID"]
setnames(prod, "Product ID", "prod_ID")

# LOCATIONS TABLE
loc <- raw[,.(city = unique(City),
              state = unique(State),
              region = unique(Region)),
           by = "Postal Code"]

setnames(loc, "Postal Code", "zip")

   ## ZIP duplicates handling
loc$zip %>% unique() %>% length()

loc[duplicated(loc$zip),zip] %>% 
    filter(.data = loc, zip == .)

loc[zip == "92024" & city == "Encinitas", zip := 92028]

# TRANSACTIONS TABLE
pur <- raw[,.(pur_ID = unique(`Order ID`),
              date = unique(`Order Date`),
              trans_mode = unique(`Ship Mode`),
              cust_ID = unique(`Customer ID`),
              prod_ID = unique(`Product ID`),
              sales = Sales,
              items = Quantity,
              price = Sales/Quantity,
              promo = Discount),
           by = "Row ID"]

# WHY SPLITTING?
object.size(raw) %>% format(units = "Mb")

(object.size(cust) + object.size(loc) + object.size(prod) +
    object.size(pur)) %>% format(units = "Mb")

    # more realistic proportions
(10 * object.size(raw)) %>% format(units = "Mb")

(object.size(cust) + object.size(loc) + object.size(prod) +
        10 * object.size(pur)) %>% format(units = "Mb")

rm(raw)

# ANALYTICS!
pur[,
    promoC := ifelse(promo == 0, 0, 1)
    ][,
      .(cust_ID = unique(cust_ID),
       sales = sum(sales),
       items = sum(items),
       price = mean(sales/items),
       promoC = sum(promoC),
       noSKU = .N),
    by = "pur_ID"
    ][,
      promoC := promoC/noSKU
      ] -> bas

# sales histogram
ggplot(bas, aes(x = sales)) +
    geom_histogram(binwidth = 1000)

ggplot(bas[sales < 1000,], aes(x = sales)) +
    geom_histogram(binwidth = 100)

# the lower the prices, the more the items?
ggplot(bas, aes(x = items, y = price)) +
    geom_point() +
    geom_smooth(method = "loess")

# basket promo share
ggplot(bas[order(bas$promoC),], aes(x = 1:nrow(bas), y = promoC)) +
    geom_line()

# basket unique SKU
ggplot(bas[order(bas$noSKU),], aes(x = 1:nrow(bas), y = noSKU)) +
    geom_line()

# relation between unique SKU and number of items bought
ggplot(bas, aes(x = noSKU, y = items)) +
    geom_point()
    # BAD IDEA!

ggplot(bas, aes(x = as.factor(noSKU), y = items)) +
    geom_boxplot()

ggplot(bas, aes(x = noSKU, y = items)) +
    geom_jitter()

# customer segmentation
pur[,
    .(unSKU = length(unique(prod_ID)),
      itemsSKU = mean(items),
      priceSKU = mean(price),
      promoSKU = mean(promo)),
    by = "cust_ID"] -> custStats

bas[,
    .(basSKU = mean(noSKU),
      valueBas = mean(sales),
      itemsBas = mean(items),
      priceBas = mean(price)),
    by = "cust_ID"] -> basStats

custStats[basStats, on = "cust_ID"
          ][cust[,.(cust_ID, segment)],, on = "cust_ID"] -> custStats
rm(basStats)

custStats$segment %>% unique

custStats %<>% 
    mutate(Corp = ifelse(segment == "Corporate", 1, 0)) %>% 
    mutate(HO = ifelse(segment == "Home Office", 1, 0)) %>% 
    select(-segment)

custStats %>% summary()
    
custStatsS <- custStats[,-1]
rownames(custStatsS) <- custStats[,1]
custStatsS <- apply(custStatsS, 2, scale)

set.seed(567)
ss <- rep(NA, 20)
for (i in 1:20) {
    ss[i] <- kmeans(custStatsS, i)$tot.withinss
}
plot(1:20, ss, type = "b")
k = 5

custStats$group <- kmeans(custStatsS, k)$cluster
rm(custStatsS)
table(custStats$group)

tree <- rpart(as.factor(group)~., custStats[,-1])
fancyRpartPlot(tree)

custStats$cust_ID <- rownames(custStats)
cust <- cust[custStats, on = "cust_ID"]

# product segmentation
pur[,
    .(custPen = length(unique(cust_ID))/nrow(cust),
      basPen = length(unique(pur_ID))/nrow(bas),
      items = mean(items),
      price = mean(price),
      promo = mean(promo)),
    by = "prod_ID"] -> prodStats

prodStats[prod[,.(prod_ID, cat)], on = "prod_ID"
          ][,furn := ifelse(cat == "Furniture", 1, 0)
            ][,tech := ifelse(cat == "Technology", 1, 0)
              ][!duplicated(prod_ID), !"cat"] -> prodStats

rownames(prodStats) <- prodStats$prod_ID

prodStatsS <- apply(prodStats[,-1], 2, scale)
rownames(prodStatsS) <- prodStats$prod_ID

cluster <- hclust(dist(prodStatsS), method = "average")
plot(cluster)

res <- rep(NA, 20)
for (i in 1:20) {
    res[i] <- kmeans(prodStatsS, i)$tot.withinss
}
plot(1:20, res, type = "b")
k = 4

prodStats$group <- kmeans(prodStatsS, k)$cluster

tree <- rpart(as.factor(group) ~ ., prodStats[,-1])
fancyRpartPlot(tree)

# which customers which items?
pur[cust[,.(cust_ID, group)], on = "cust_ID"
    ][prodStats[,.(prod_ID, group)], on = "prod_ID"] -> purGrp

setnames(purGrp, c("group", "i.group"), c("custG", "prodG"))

purGrp[,.(custG, prodG)
       ][,.(freq = .N),
         by = .(custG, prodG)] -> purGrp

custSum <- purGrp[,.(sum = sum(freq)), by = "custG"]

purGrp[custSum, on = "custG"
       ][,share := freq/sum
         ][,.(custG, prodG, share)] -> purGrp

ggplot(purGrp, aes(x = custG, y = share , fill = as.factor(prodG))) +
    geom_col()
