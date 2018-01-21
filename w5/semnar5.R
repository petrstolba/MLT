#####################
## Seminar 5       ##
## Michal Kubista  ##
## 5 February 2018 ##
#####################

sapply(
    c("data.table","dplyr","magrittr","ggplot2",
      "purrr", "GGally", "readxl"),
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
pur[,.(sales = sum(sales),
       items = sum(items),
       price = mean(sales/items)),
    by = "pur_ID"] -> bas

# sales histogram
ggplot(bas#[sales < 1000,]
       , aes(x = sales)) +
    geom_histogram(binwidth = 1000)

# the lower the prices, the more the items?
ggplot(bas, aes(x = items, y = price)) +
    geom_point()

# customer segmentation

# product segmentation

# 