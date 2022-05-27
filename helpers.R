source("../support.R")
library(dplyr)

# Reaalizuje zapytanie

x <- data.frame()
y<- data.frame()
i <- 0
cat("0%")
# wczytuje, wybiera i skleja dane
for (m in c("05", "06", "07")) {
  name = paste("2016", m, sep="")
  d <- get_data(name)
  e<- get_data(name, TRUE)
  d %>% select(tripduration, starttime)->d
  e %>% select(Trip.Duration, Start.Time)->e
  x <- merge(x, d, all=TRUE)
  y <- merge(y, e, all=TRUE)
  i <- i+1
  cat(sprintf("\r%.2f%%", i*100/3))  # progress
}
# zmiana formatu daty
x$starttime <- format(as.Date(x$starttime, format = "%m/%d/%Y %H:%M:%S"), "%Y-%m-%d")
# grupowanie po datach i liczenie śrredniego czasu wycieczki
x %>%
  group_by(starttime)%>%
  summarise(Mean = mean(tripduration))->x

# to samo
y$Start.Time <- format(as.Date(y$Start.Time, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
y %>%
  group_by(Start.Time)%>%
  summarise(Mean = mean(Trip.Duration))->y

# funkcja wywoływana z zwenątrz, dostaje czy Nowy Jork i zakres dat (maj nie działa??)
# filtruje dane i rysuje wykres
getThePlot<-function(NYC, mini = as.Date("2016-05-01"), maxi = as.Date("2016-07-31")){
  if(NYC){
    x %>%
      filter(starttime>=mini & starttime<=maxi)->a
    barplot(
      a$Mean,
      main = "średnia długość wypożyczenia dla dnia",
      ylim = c(0, 2500),
      xlab = "data",
      ylab = "średni czas",
      names.arg = a$starttime,
      col = terrain.colors(24)
    )
  }
  else{
    y %>%
      filter(Start.Time>=mini & Start.Time<=maxi)->a
    barplot(
      a$Mean,
      main = "średnia długość wypożyczenia dla dnia",
      ylim = c(0, 2500),
      xlab = "data",
      ylab = "średni czas",
      names.arg = a$Start.Time,
      col = terrain.colors(24)
    )
  }
}




