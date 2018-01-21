sapply(
  c("data.table","tidyverse","magrittr",
    "arules","arulesViz","readxl"),
  require,
  character.only = T
)

transRaw <-
    as.data.table(read_xlsx("w2/data/online_retail.xlsx"))

summary(transRaw)

transRaw <- transRaw[Quantity > 0
                     ][UnitPrice > 0
                       ][!is.na(CustomerID)
                         ]

transRaw[,.(count = length(unique(UnitPrice))), by = "StockCode"
        ][order(count, decreasing = TRUE)
           ][count >= 6, StockCode
             ] %>% 
  .[-1:-3] -> prodID

trans <- transRaw[StockCode %in% prodID]  

transCoef <- trans[,.(elas = lm(log(Quantity)~log(UnitPrice))$coefficients[2]), by = StockCode
                   ][order(elas, decreasing = T)]

transCoef$elas %>% summary()

transCoef$elasG <- cut(transCoef$elas,
                      breaks = c(-14, -3, -1.2, 0, 6),
                      labels = c("H", "M", "L", "N"))



