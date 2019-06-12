#=============================================2014年=============================================
# 2014年
library(dplyr)
library(tidyr)

#資料清理
Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
before1 <- read.csv("/Users/Apple/Desktop/judy/R/test/before_TW_itny.csv")

colnames(before1)[7] <- "South Korea"
View(before1)
before_TW_itny <- gather(before1, next_stop, num_traveler, HongKong, Japan, "South Korea", China, Philippines, Thailand)
View(before_TW_itny)

#翔媛修改欄位部分
v <- c("Taiwan", mode="character", length = 768)
v[1:768] <- "Taiwan"

before_TW_itny2 <- cbind(before_TW_itny,v)
View(before_TW_itny2)
temp <- before_TW_itny2$num_traveler
before_TW_itny2$num_traveler <- before_TW_itny2$v
before_TW_itny2$v <- temp
colnames(before_TW_itny2) <- c("Year","居住地","細分","Residence","next_stop","Taiwan","num_traveler")
before_TW_itny2
before_TW_itny3 <- before_TW_itny2 %>%
  filter(Year=="2014")
View(before_TW_itny3)

# 將資料從factor轉成character
before_TW_itny3$Residence <- as.character(before_TW_itny3$Residence)
before_TW_itny3$Taiwan<- as.character(before_TW_itny3$Taiwan)

# 修改資料名字
n <- length(before_TW_itny3$Residence)
for (i in (1:n)){
  print(before_TW_itny3$Residence[i])
  if (before_TW_itny3$Residence[i] == "U.K."){
    before_TW_itny3$Residence[i] <- "United Kingdom"
    print(before_TW_itny3$Residence[i])
  }else if (before_TW_itny3$Residence[i] == "U.S.A."){
    before_TW_itny3$Residence[i] <- "United States of America"
    print(before_TW_itny3$Residence[i])
  }
}
#View(before)
#開始畫圖
library(devtools)
library(rnaturalearth) 
library(rgdal)
library(sp)
#plot(ne_countries())

countries <- ne_countries()
#countries
#View(countries)
#states <- ne_states(iso_a2 = 'US')
#View(states)

#獲得世界城市所有的坐標
countries$longitude <- coordinates(countries)[,1]
countries$longitude

countries$latitude <- coordinates(countries)[,2]
countries$latitude

countries_xyresi <- countries@data %>%
  select(admin, longitude, latitude)
countries_xynext <- countries@data %>%
  select(admin, longitude, latitude)

View(countries_xyresi)
#開始匹配
be3_match <- before_TW_itny3 %>%
  left_join(countries_xyresi, by = c('Residence' = 'admin')) %>%
  left_join(countries_xynext, by = c('next_stop' = 'admin')) %>%
  left_join(countries_xynext, by = c('Taiwan' = 'admin')) 

#接下來需要做的：凱均填充NA部分和三個經緯度的欄位名稱更改，完成後上傳檔案到github

#HongKong
hk_x <- 114.1784
hk_y <- 22.3165
be3_match$longitude.x[c(1,25,33,37,49,61,65,97,129,161)] <- hk_x
be3_match$longitude.y[c(1:12, 13:32)] <- hk_x
be3_match$latitude.x[c(1,25,33,37,49,61,65,97,129,161)] <- hk_y
be3_match$latitude.y[c(1:12, 13:32)] <- hk_y

#Middle East
me_x <- 43.7134
me_y <- 23.9812
be3_match$longitude.x[c(6,18,38,42,54,66,70,102,134,166)] <- me_x
be3_match$latitude.x[c(6,18,38,42,54,66,70,102,134,166)] <- me_y

#Singapore
s_x <- 103.8678
s_y <- 1.3555
be3_match$longitude.x[c(8,20,32,40,44,56,72,104,136,168)] <- s_x
be3_match$latitude.x[c(8,20,32,40,44,56,72,104,136,168)] <- s_y
be3_match$longitude.x[c(8,20,32,44,56)] <- s_x
be3_match$latitude.x[c(8,20,32,44,56)] <- s_y

View(be3_match)

library(geosphere)

flows <- gcIntermediate(be3_match[,8:9], be3_match[,10:11], sp = TRUE, addStartEnd = TRUE, breakAtDateLine = FALSE)

flows$counts <- (be3_match$num_traveler / 10000)

flows$origins <- be3_match$Residence

flows$destinations <- be3_match$next_stop

library(leaflet)
library(RColorBrewer)

hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~counts, label = hover, 
               group = ~origins, color = ~pal(origins)) %>%
  addLayersControl(overlayGroups = unique(flows$origins), 
                   options = layersControlOptions(collapsed = FALSE))

