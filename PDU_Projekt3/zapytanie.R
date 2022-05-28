source("../support.R")


godziny_weekend <- function(frame, weekend){
  "
  Funkcja oblicza średnią liczbę rozpoczętych w danym miesiącu przejazdów dla
  każdej godziny w weekend, jeśli weekend=TRUE, lub w dniach roboczych gdy weekend=FALSE.
  "
  if(weekend){
    frame <- frame[frame$weekday=="7"|frame$weekday=="6",]
  } else{
    frame <- frame[frame$weekday!="7"&frame$weekday!="6",]
  }
  frame <- aggregate(frame$bikeid, frame[,c("starthour", "starttime")], length)
  frame <- aggregate(frame$x, frame[,"starthour", drop=FALSE], mean)
  return(frame)
}

x <- data.frame()
y <- data.frame()

for (m in c("05", "06", "07")) {
  name = paste("2014", m, sep="")
  d <- get_data(name)
  d[,"starthour"] <- substr(d$starttime, 12, 13)
  d[,"weekday"] <- strftime(as.Date(d$starttime), format="%u")
  d[,"starttime"] <- substr(d$starttime, 1, 10)
  m_data1 <- godziny_weekend(d, weekend = TRUE)
  m_data2 <- godziny_weekend(d, weekend = FALSE)
  x <- merge(x, m_data1, all=TRUE)
  y <- merge(y, m_data2, all=TRUE)
}
x <- aggregate(x$x, x[,"starthour", drop=FALSE], mean)
y <- aggregate(y$x, y[,"starthour", drop=FALSE], mean)

getThePlot<-function(weekend){
  if(weekend){
    barplot(
      x$x,
      main = "Ruch rowerowy w weekend",
      ylim = c(0, 3500),
      xlab = "godzina",
      ylab = "średnia liczba rowerzystów",
      names.arg = x$starthour,
      col = terrain.colors(24)
    )
  }
  else{
    barplot(
      y$x,
      main = "Ruch rowerowy w dniach roboczych",
      ylim = c(0, 3500),
      xlab = "godzina",
      ylab = "średnia liczba rowerzystów",
      names.arg = y$starthour,
      col = terrain.colors(24)
    )
  }
  
}


