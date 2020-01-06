####################
## Seminar 1      ##
## Michal Kubista ##
## 6 January 2020 ##
####################

sapply(c("data.table","dplyr","magrittr","ggplot2"),
       require,
       character.only = T)
path2data = "w1/data/seminar"

if (!dir.exists(path2data)) {
    dir.create(path2data, recursive = T)
}

#-- PART 1 - LOADING TIPS & TRICKS ############################################

## download data @ https://drive.google.com/drive/folders/1Dyyk2HCaoq4VLQJ3rRnYqmcjFABc_Ssk?usp=sharing

#--- 1.1 SEVERAL SIMILAR FILES ------------------------------------------------

## data origin https://www.kaggle.com/onlineauctions/online-auctions-dataset
## script used to create our data:
# auction = data.table::fread(file.path(path2data,"auction.csv"), dec = ".",
# data.table = F)
# str(auction)
# 
# auction[,c("bid", "bidtime", "bidderrate", "openbid", "price")]  %<>%
#       apply(., 2, as.numeric)
# str(auction)
# 
# data.table::fwrite(auction, file.path(path2data,"auction.csv"))
# 
# auction %<>% dplyr::mutate(days = ceiling(bidtime))
# 
# g_write = function(x){
#       write.csv(x,file.path(path2data,
#                           paste0("auction_day_",unique(x$days),".csv")),
#                 row.names = F)
#       return(x)
# }
# 
# auction %>%
#       group_by(days) %>%
#       do(g_write(.))
# 
# rm(auction, g_write)
#----

auction_Names = function(){
      files = list.files(path2data, full.names =  T)
      files = files[grep("auction_day",files)]
      return(files)

}()

#---- 1.1.1 FOR LOOP ----------------------------------------------------------
auction_Names()

input = list()

# vector growing - BAD IDEA!!!
for (i in auction_Names()) {
      input[[length(input) + 1]] = read.csv(i)
}

sapply(input, colnames)
inputTable = do.call(rbind.data.frame, input)

rm(input, inputTable, i)

#---- 1.1.2. APPLY ------------------------------------------------------------

input = lapply(auction_Names(), read.csv)

inputTable = do.call(rbind.data.frame,input)

rm(input, inputTable)

#---- 1.1.3. DATA.TABLE -------------------------------------------------------

dtNames = data.table::data.table(file = auction_Names())
dtTable = dtNames[, fread(file), by = file]

## shorter version
dtTable = 
    data.table::data.table(file = auction_Names())[,fread(file), by = file]

rm(dtNames, dtTable, auction_Names)

#--- 1.2 SEVERAL DIFFERENT (CONNECTED) FILES ----------------------------------
## script used to create our data:
# auction = fread(file.path(path2data,"auction.csv"), dec = ".", data.table = F) %>%
#       mutate(ID = paste0(auctionid,bidtime,bidder))
# 
# apply(auction, 2, function(x) length(unique(x)))
# 
# auction %>% select(ID, item, auction_type) -> itemTable
# auction %>% select(ID, price) -> priceTable
# auction %>% select(-c(item, auction_type, price)) -> auctionTable
# 
# dtOut = data.table(file = c("itemTable","priceTable", "auctionTable"))
# dtOut[,fwrite(get(file),file.path(path2data,paste0(file,".csv"))),
#       by = file
#       ]
# 
# rm(auction, auctionTable, priceTable, itemTable, dtOut)
#----

(list.files(path2data) %>%
     grep("Table",.) %>%
     list.files(path2data, full.names = TRUE)[.] -> inFileNames)

#---- 1.2.1 DATA.TABLE JOIN --------------------------------------------------

input = lapply(inFileNames, fread)
names(input) = inFileNames %>% gsub(".*/","",.) %>% gsub(".csv","",.)

dtTable = input$auctionTable[input$itemTable, on = "ID"
                            ][input$priceTable, on = "ID"]

rm(dtTable, input)

#---- 1.2.2 (D)PLYR JOIN ----------------------------------------------------

dplyr::full_join(input$auctionTable, input$itemTable, by = "ID") %>%
      full_join(input$priceTable) -> inputTable

inputTable2 = plyr::join_all(input)

rm(input, inputTable, inputTable2, inFileNames)

#--- 1.3 READING WITH COL.CLASSES ---------------------------------------------

## data origin: https://www.kaggle.com/rtatman/universal-product-code-database/data
## script used to create our data:
# upc = data.table::fread(file.path(path2data,"upc_corpus.csv"))
# 
# upc$ean %>% as.numeric() %>% is.na() -> numIndex
# upc$ean[numIndex] = "0"
# 
# upcLength = nchar(upc$ean)
# maxLength = max(upcLength)
# 
# # system.time({
# #       for(i in seq_along(upc$ean)){
# #             upc$ean[i] =
# #
# #                   paste0(
# #                         stringr::str_dup("0",maxLength - upcLength[i]),
# #                         upc$ean[i])
# #
# #
# #             print(paste(i/nrow(upc),"%"))
# #
# #       }
# # })
# 
# library(stringr)
# library(purrr)
# 
# system.time({
#      upc$ean =
#          purrr::map2_chr(upc$ean, upcLength,
#                          function(x,y) paste0(
#                              stringr::str_dup("0", maxLength - y
#                                               ),x)
#          )
# })
# 
# data.table::fwrite(upc, file.path(path2data, "upc.csv"))
# 
# rm(upc, upcLength, maxLength, numIndex)
#----

upc = data.table::fread(file.path(path2data, "upc.csv"))

upc %>% head(1000) %>% View()
upc[9740:9744,]

upc = data.table::fread(file.path(path2data, "upc.csv"),
                         colClasses = c("character", "character"))

## or in case of large amount of columns
upc = data.table::fread(file.path(path2data, "upc.csv"),
             colClasses = c(ean = "character"))

rm(upc)

#-- PART 2 - CLEANING TIPS & TRICKS ###########################################

#--- 2.1 REGEX ----------------------------------------------------------------
#---- 2.1.1 TO BE, OR NOT TO BE
hamlet = readLines("http://www.gutenberg.org/files/1524/1524-0.txt")#, encoding = "UTF-8")
hamlet[1:50]

grep("hamlet.*prince.*denmark", hamlet[1:500], ignore.case = TRUE)

hamlet = hamlet[-1:-78]

hamlet = hamlet[-which(hamlet == "")]

hamHead = hamlet[1:25]
hamText = hamlet[26:length(hamlet)]
rm(hamlet)

sum(grepl("rosencrantz", hamText, ignore.case = TRUE))
length(grep("rosencrantz", hamText, ignore.case = TRUE))


finder = function(name){
    a = length(grep(name, hamText, ignore.case = TRUE))
    root = stringr::str_sub(name,1,nchar(name) - 3)
    b = length(grep(root,hamText, ignore.case = TRUE))
    return(c(a,b))
}

finder("hamlet")
finder("claudius")

rm(hamText, hamHead, finder)

#---- 2.1.2 DATA & REGEX
## data origin: https://www.datazar.com/project/p9d520430-ab0a-4f26-a44a-39b08d0e41bb/overview

unzip(
    file.path(path2data,
              "fixed-broadband-speeds-postcode-london-2016.xlsx.zip"),
    exdir = path2data
    )

broadband = 
    readxl::read_excel(
        file.path(path2data,"fixed-broadband-speeds-postcode-london-2016.xlsx")
    )

str(broadband)

broadband = broadband[,c(1,8:11,16:19,35,36)]
colnames(broadband) =
      c("place", "downAvg", "downMed", "downMin", "downMax", "upAvg", "upMed",
        "upMin","upMax", "lat","lon")

summary(broadband)

broadband$downAvg %>%
      stringr::str_replace("[0-9]*","") %>%
      stringr::str_replace("[.,][0-9]*","") %>% unique()

# how to improve this?
removeStuff = function(x){
      x %>% stringr::str_replace("<4","2") %>%
            stringr::str_replace(",","\\.") %>%
            stringr::str_replace("N/A","") %>%
            as.numeric()
}

broadband[,-1] = apply(broadband[,-1], 2, removeStuff)

summary(broadband)

#--- 2.2 MANIPULATION ---------------------------------------------------------
#---- 2.2.1 DPLYR -------------------------------------------------------------

broadband %>% dplyr::filter(lon > 0, lat > 51.6)
      
broadband %>%
      dplyr::select(lon, lat) %>% 
      dplyr::filter(lon > 0, lat > 51.6)

broadband %>% 
      dplyr::arrange(downAvg)

broadband %>% dplyr::mutate(downRange = downMax - downMin,
                            upRange = upMax - upMin)

broadband %>%
      dplyr::group_by(downAvg) %>% 
      dplyr::summarise(meanMax = mean(downMax),
                       meanMin = mean(downMin)) %>% 
      dplyr::mutate(meanDif = meanMax - meanMin) %>% 
      dplyr::arrange(-meanDif)

#---- 2.2.2 DATA.TABLE --------------------------------------------------------

data.table::as.data.table(broadband)[
      ,.(meanMax = mean(downMax),
         meanMin = mean(downMin)),
      by = downAvg
      ][,.(downAvg, meanMax, meanMin,
           meanDif = meanMax - meanMin)
        ][order(-meanDif)]

#--- 2.3 LONG-WIDE -----------------------------------------------------------

broadbandLong = tidyr::gather(broadband[,1:5],
                              key = "metric",
                              value = "measure",
                              downAvg:downMax)

broadbandWide = tidyr::spread(broadbandLong, metric, measure)

rm(broadband, broadbandWide, broadbandLong, removeStuff)

#-- PART 3 - EXPLORING TIPS & TRICKS ##########################################

auction = data.table::fread(file.path(path2data,"auction.csv"),
                             dec = ".", data.table = F)

#--- 3.1 BASE R

colSelect =
    !colnames(auction) %in% c("auctionid","bidder","item","auction_type")
plot(auction[,colSelect])

#--- 3.2 GGally

GGally::ggpairs(auction[,colSelect])

#--- 3.3 ORLY?
url =
      "http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt"

orly = read.table(url, header = FALSE)

fit = lm(V1 ~ . - 1, data = orly)

summary(fit)

orlyRes = data.frame(fitted = fit$fitted.values, resid = fit$residuals)

ggplot(orlyRes, aes(x = fitted, y = resid)) + 
      geom_point(size = 2)
