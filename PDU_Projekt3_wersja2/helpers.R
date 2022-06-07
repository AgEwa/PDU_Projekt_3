source("../support.R")
library(dplyr)

# Skrypt reaalizuje zapytanie do wyświetlenia interaktywnego

x <- data.frame(integer(1),"")
names(x)<-c("tripduration","starttime")
y <- data.frame(integer(1),"")
names(y)<-c("Trip.Duration","Start.Time")
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
x %>%
  filter(tripduration>60)->x
# zmiana formatu daty
x$starttime <- format(as.Date(x$starttime, format = "%m/%d/%Y %H:%M:%S"), "%Y-%m-%d")
# grupowanie po datach i liczenie śrredniego czasu wycieczki
x %>%
  group_by(starttime)%>%
  summarise(Mean = mean(tripduration), Mediana = median(tripduration))->x

y %>%
  filter(Trip.Duration>60)->y
# to samo
y$Start.Time <- format(as.Date(y$Start.Time, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
y %>%
  group_by(Start.Time)%>%
  summarise(Mean = mean(Trip.Duration), Mediana = median(Trip.Duration))->y

# funkcja wywoływana z zwenątrz, dostaje czy Nowy Jork i zakres dat
# filtruje dane i rysuje wykres
getThePlot<-function(NYC, mini, maxi){
  par(mar=c(5, 4, 4, 8), xpd=TRUE)
  if(NYC){
    x %>%
      filter(starttime>=mini & starttime<=maxi)%>%
      arrange(starttime)->a
    a
    barplot(
      t(as.matrix(a[,2:3])),
      beside = T,
      main = enc2utf8("Average length of a bike rental for the day in given data range"),
      xlab = "Date",
      ylab = enc2utf8("Avarage trip time"),
      names.arg = a$starttime,
      col = c("blue", "gray28")
    )
    
  }
  else{
    y %>%
      filter(Start.Time>=mini & Start.Time<=maxi)%>%
      arrange(Start.Time)->a
    barplot(
      t(as.matrix(a[,2:3])),
      beside = T,
      main = "Average length of a bike rental for the day in given data range",
      xlab = "Date",
      ylab = "Avarage trip time",
      names.arg = a$Start.Time,
      col = c("blue", "gray28")
    )
    
  }
  legend("topright", legend=c("mean","median"), title="Legend", fill = c("blue", "gray"), inset=c(-0.1, 0))
}




