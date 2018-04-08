library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(magrittr)
setwd("C:\\RData")
ffinal <- read.csv("shiny2/data/finaldata.csv", fileEncoding = "big5")
#畫圖用資料
station<-read.csv("shiny2/data/加油站資訊.csv",fileEncoding = "big5") %>% filter(縣市=="台北市")
# load("~/Downloads/shiny3/server_data.RData")
 # x <- read.csv("file:///C:/Users/User/Documents/x.csv")

ffinal$Lat <- jitter(ffinal$Lat)
ffinal$lLong <- jitter(ffinal$lLong)
ffinal$站代號 <- formatC(ffinal$站代號, width=5, format="d", flag="0")
# row.names(ffinal) <- ffinal$站代號
# 我不知道你在幹嘛

cleantable <- ffinal %>%
    select(
        Date = 加油日期,
        Car_num = 車牌號碼,
        How_much = 加油公升數,
        Loca = 站名,
        Time = 時間,
        Long = 經度,
        Lat = 緯度
    ) %>% 
    mutate(
        Hour = substr( as.character(Time), 1, 2) %>% as.numeric(),
        Min = substr( as.character(Time), 4, 5) %>% as.numeric(),
        Date = as.character(Date),
        Eng = substr( as.character(Car_num), 1, 2),
        Num = substr( as.character(Car_num), 4, 7)
    ) %>% 
    select(
        Date, Car_num, How_much, Loca, 
        Long , Lat, 
        Hour, Min ,
        Eng , Num 
    )

cleantable$Min %>% class()
ffinal %<>% mutate(日期 = paste(加油日期,時間))

det_time <- function(date,hour,mins){
  time<-paste0(date," ",hour,":",mins)
  timee <- strptime(time, "%Y-%m-%d %H:%M") #eg 2017-12-04 08:09
  
  output <- ffinal[difftime(timee, ffinal$日期, units="mins")<10 &
                    difftime(timee,ffinal$日期, units="mins")>0,] %>%  
  dplyr::select(站名) %>% `[`(,1) %>% as.character  %>% table %>%  as.data.frame()
  colnames(output) <- c("站名","交易次數")
  station.name <- data.frame(站名= unique(station$站名))
  output2<- merge(station.name,output,by="站名",all.x = T)
  output2[is.na(output2$交易次數),"交易次數"] <- 0
  output3 <- merge(output2, unique(ffinal[,c("經度","緯度","站名")]),by="站名", all.x=T)
  
  return(output3)
  
}



create <- function(map2){
mmap <- cbind(map2 , grade =1 , 擁擠程度 = "順暢")
mmap$擁擠程度 %<>% as.character()
mmap[which(mmap$交易次數>3),"grade"] %<>% `+`(1)
mmap[which(mmap$交易次數>6),"grade"] %<>% `+`(1)
mmap[which(mmap$交易次數>3),"擁擠程度"] <- "中等"
mmap[which(mmap$交易次數>6),"擁擠程度"] <- "擁擠"
return(mmap)
}
getColor <- function(data) {
    sapply(data$grade, function(grade) {
        if( grade== "3") {
            "#d7191c"
        } else if(grade == "2") {
            "#fdae61"
        } else {
            "#a6d96a"
        } })
}

# cleantable$Car_num %>% class()

####讓我們謝謝葉哥<3
# hour <- NULL
# min <- NULL
# for(i in 1:length(cleantable$time)){
#      hourmin <- strsplit( as.character(cleantable$time[i]) ,split=" ")[[1]][2] 
#      hour <- c(hour,strsplit( hourmin ,split=":")[[1]][1])
#      min <- c(min,strsplit( hourmin ,split=":")[[1]][2])
# }
# 
# cleantable2 <-cbind(cleantable,hour,min) 





