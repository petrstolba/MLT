sapply(
  c("data.table","tidyverse","magrittr","readxl"),
  require,
  character.only = T
)

transRaw <-
    as.data.table(read_xlsx("w2/data/online_retail.xlsx"))

summary(transRaw)

transRaw <- transRaw[Quantity > 0 & UnitPrice > 0]

transRaw[,.(count = length(unique(UnitPrice))), by = "StockCode"
        ][order(count, decreasing = TRUE)
           ][count >= 6, StockCode
             ] %>% 
  .[-1:-3] -> prodID

trans <- transRaw[StockCode %in% prodID]  
trans$InvoiceDate %<>% as.Date

elas_table <- trans[,
                    .(elas = lm(log(Quantity)~log(UnitPrice))$coefficients[2]),
                    by = StockCode
                   ][order(elas, decreasing = T)]

elas_table$elas %>% summary()

elas_table %<>% 
    filter(elas < 0) %>% 
    as.data.table()

elas_table$elasG <- cut(elas_table$elas,
                      breaks = c(-14, -2, -1, 0),
                      labels = c("H", "M", "L"))
summary(elas_table$elasG)


find_price_index <- function(code, day) {
    
    trans %>% 
        filter(StockCode == code) %>% 
        select(StockCode, InvoiceDate, UnitPrice) -> price
    
    price %>% 
        filter(InvoiceDate == day) %>% 
        .$UnitPrice %>% 
        mean(na.rm = T) -> target
    
    mean_price <- median(price$UnitPrice, na.rm = TRUE)
    return(target / mean_price)
}

sapply(prodID, find_price_index, day = "2011-12-05") -> pi_table

pi_table <- data.table(StockCode = names(pi_table), pi = pi_table)[!is.na(pi)]
pi_table[, piG := ifelse(pi > 0.9, ifelse(pi > 1.1, "H", "M"),"L")]
summary(pi_table$piG)

elas_table[pi_table, on = "StockCode"][!is.na(elas)] -> pie_table

pie_table %<>% 
    mutate(group = paste0(piG, elasG)) %>% 
    mutate(piG = ordered(piG, c("L","M","H"))) %>% 
    mutate(elasG = ordered(elasG, c("L","M","H")))

ggplot(pie_table, aes(x = piG, y = elasG, color = group)) +
    geom_jitter() +
    geom_hline(yintercept = 1.5) +
    geom_hline(yintercept = 2.5) +
    geom_vline(xintercept = 1.5) +
    geom_vline(xintercept = 2.5)

hh_items <- pie_table[pie_table$group == "HH", "StockCode"]

base <- trans[,.(baseprice = quantile(UnitPrice, 0.75)), by = "StockCode"]

trans[base, on = "StockCode"
      ][,diff := UnitPrice/baseprice
        ][,promo := ifelse(diff < 0.95, 1, 0)] -> trans

pene_table <- trans[StockCode %in% hh_items,
                    .(elas = lm(log(Quantity)~log(UnitPrice))$coefficients[2]),
                    by = .(StockCode, promo)
                    ][order(elas, decreasing = T)]

pene_table %>% 
    dcast(StockCode~promo) %>% 
    .[complete.cases(.)] -> pene_table

setnames(pene_table, c("0","1"), c("N","P"))

pene_table$PG <- ordered(ifelse(pene_table$P < -1.7, "H", "L"), c("L", "H"))
pene_table$NG <- ordered(ifelse(pene_table$N < -1.2, "H", "L"), c("L", "H"))
pene_table %<>% 
    mutate(group = paste0(PG, NG))


ggplot(pene_table, aes(x = PG, y = NG, color = group)) +
    geom_jitter() +
    geom_hline(yintercept = 1.5) +
    geom_vline(xintercept = 1.5)
    
pene_table %>% 
    filter(PG == "L" & NG == "L")

pie_table %>% filter(StockCode == "22608")

trans %>% 
    filter(StockCode == "22608") -> SKU

ggplot(SKU, aes(x = InvoiceDate, y = UnitPrice, color = as.factor(promo))) +
    geom_line() +
    geom_point()
