install.packages("RODBC")
library(RODBC)
install.packages("DBI")
library(DBI)
install.packages("RMySQL")
library(RMySQL)
install.packages("leaflet")
library(leaflet)
library(dplyr)


con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'mysql',
                 host = 'localhost',
                 user = 'root',
                 password = '0801')
data <- dbGetQuery(con, "select * from yeongju_bus yb, yeongju_bus_num2 ybn
	where yb.정류장ID = ybn.정류장ID;")

yeongju_num2 <- data
dbDisconnect(con)
print(yeongju_num2)

map <- leaflet() %>%
  setView(lng = 128.616795, lat=36.820041, zoom=14) %>% 
  addTiles() %>% addCircles(lng = yeongju_num2$경도, lat = yeongju_num2$위도, popup = yeongju_num2$정류장명)

for (i in 1:(nrow(yeongju_num2) - 1)) {
  # 현재 점과 다음 저장된 점의 경도와 위도를 가져옵니다.
  current_lng <- yeongju_num2[i, "경도"]
  current_lat <- yeongju_num2[i, "위도"]
  next_lng <- yeongju_num2[i + 1, "경도"]
  next_lat <- yeongju_num2[i + 1, "위도"]
  
# 선을 추가합니다.
  map <- map %>%
    addPolylines(lng = c(current_lng, next_lng), lat = c(current_lat, next_lat))
}
map
#--------------------------------------------------

# 2번 버스 노선 그림림

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'mysql',
                 host = 'localhost',
                 user = 'root',
                 password = '0801')
data <- dbGetQuery(con, "select * from yeongju_bus_num2 ybn left outer join yeongju_bus yb 
                   on ybn.정류장ID = yb.정류장ID;")


yeongju_num2 <- data
dbDisconnect(con)
print(yeongju_num2)

map1 <- leaflet() %>%
  setView(lng = 128.616795, lat=36.820041, zoom=14) %>% 
  addTiles() %>% addCircles(lng = yeongju_num2$경도, lat = yeongju_num2$위도, popup = yeongju_num2$정류장명)

for (i in 1:(nrow(yeongju_num2) - 1)) {
  # 현재 점과 다음 저장된 점의 경도와 위도를 가져옵니다.
  current_lng <- yeongju_num2[i, "경도"]
  current_lat <- yeongju_num2[i, "위도"]
  next_lng <- yeongju_num2[i + 1, "경도"]
  next_lat <- yeongju_num2[i + 1, "위도"]
  
  # 선을 추가합니다.
  map1 <- map1 %>%
    addPolylines(lng = c(current_lng, next_lng), lat = c(current_lat, next_lat))
}
map1

#---------------------------------------------------

# 8-1번 버스 노선선

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'mysql',
                 host = 'localhost',
                 user = 'root',
                 password = '0801')
data <- dbGetQuery(con, "select * from yeongju_bus_num8_1 ybn left outer join yeongju_bus yb
	on ybn.정류장ID = yb.정류장ID; ;")

yeongju_num8_1 <- data
dbDisconnect(con)
print(yeongju_num8_1)

map2 <- leaflet() %>%
  setView(lng = 128.616795, lat=36.820041, zoom=14) %>% 
  addTiles() %>% addCircles(lng = yeongju_num8_1$경도, lat = yeongju_num8_1$위도, popup = yeongju_num8_1$정류장명, color = 'red')

for (i in 1:(nrow(yeongju_num8_1) - 1)) {
  # 현재 점과 다음 저장된 점의 경도와 위도를 가져옵니다.
  current_lng <- yeongju_num8_1[i, "경도"]
  current_lat <- yeongju_num8_1[i, "위도"]
  next_lng <- yeongju_num8_1[i + 1, "경도"]
  next_lat <- yeongju_num8_1[i + 1, "위도"]
  
  # 선을 추가합니다.
  map2 <- map2 %>%
    addPolylines(lng = c(current_lng, next_lng), lat = c(current_lat, next_lat), color = 'red')
}
map2

#--------------------------------------------------------

# 겹쳐서 그림 

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'mysql',
                 host = 'localhost',
                 user = 'root',
                 password = '0801')

# 첫 번째 데이터셋 조회
data1 <- dbGetQuery(con, "select * from yeongju_bus_num2 ybn left outer join yeongju_bus yb 
                   on ybn.정류장ID = yb.정류장ID;")

# 두 번째 데이터셋 조회
data2 <- dbGetQuery(con, "select * from yeongju_bus_num8_1 ybn left outer join yeongju_bus yb
    on ybn.정류장ID = yb.정류장ID; ;")

# 데이터베이스 연결 해제
dbDisconnect(con)

# leaflet 맵 초기화
map <- leaflet() %>%
  setView(lng = 128.616795, lat = 36.820041, zoom = 14) %>% 
  addTiles()

# 첫 번째 데이터셋의 원과 선 추가
map <- map %>% 
  addCircles(data = data1, lng = data1$경도, lat = data1$위도, popup = data1$정류장) %>%
  addPolylines(data = data1, lng = data1$경도, lat = data1$위도)

# 두 번째 데이터셋의 원과 선 추가가

map <- map %>% 
  addCircles(data = data2, lng = data2$경도, lat = data2$위도, popup = data2$정류장명, color = "green") %>%
  addPolylines(data = data2, lng = data2$경도, lat = data2$위도, color = "green")

# 맵 출력
map


#-----------------------------------------------------------

# Google Maps Directions API를 사용하여 경로 가져오는 함수
get_directions <- function(origin, destination, api_key) {
  url <- paste0("https://maps.googleapis.com/maps/api/directions/json?origin=", origin[1], ",", origin[2],
                "&destination=", destination[1], ",", destination[2], "&key=", api_key)
  response <- jsonlite::fromJSON(url)
  if (response$status == "OK") {
    return(response$routes$overview_polyline$points)
  } else {
    return(NULL)
  }
}



# Google API 키 설정
api_key <- 'AIzaSyBVsv8fiam8oMfQ_b0F4iSS-D74IvWoQwY'

for (i in 1:(nrow(yeongju_num2) - 1)){
# 출발지와 목적지 지정
  origin <- c(yeongju_num2[i, "위도"], yeongju_num2[i, "경도"])  # 출발지 위도, 경도
  destination <- c(yeongju_num2[i+1, "위도"], yeongju_num2[i+1, "경도"])  # 목적지 위도, 경도

# 경로 가져오기
  path <- get_directions(origin = origin, destination = destination, key = api_key)

# 경로를 디코딩하여 좌표로 변환
  decoded_path <- decode_pl(path)

# leaflet으로 경로 그리기
  map <- leaflet() %>%
    addTiles() %>%
    addPolylines(data = decoded_path)
}
map
