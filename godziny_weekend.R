# obliczenia dotyczące liczby wypożyczonych rowerów w weekend i w dniach roboczych

source("support.R")


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

weekend_true <- data.frame()
weekend_false <- data.frame()
i <- 0
cat("0%")   # progress bar

for (m in c("05", "06", "07")) {
  name = paste("2014", m, sep="")
  d <- get_data(name)
  
  d[,"starthour"] <- substr(d$starttime, 12, 13)              # zamiana daty wykonana tylko raz przed wejściem do funkcji
  d[,"weekday"] <- strftime(as.Date(d$starttime), format="%u")    # bo jest czasochłonna, a funkcję wywołujemy 2-krotnie
  d[,"starttime"] <- substr(d$starttime, 1, 10)
  
  m_data1 <- godziny_weekend(d, weekend = TRUE)
  m_data2 <- godziny_weekend(d, weekend = FALSE)
  weekend_true <- merge(weekend_true, m_data1, all=TRUE)
  weekend_false <- merge(weekend_false, m_data2, all=TRUE)
  
  i <- i+1
  cat(sprintf("\r%.2f%%", i*100/3))  # progress bar
}
weekend_true <- aggregate(weekend_true$x, weekend_true[,"starthour", drop=FALSE], mean)
weekend_false <- aggregate(weekend_false$x, weekend_false[,"starthour", drop=FALSE], mean)

png()
par(mfrow=c(2, 1))

barplot(
  weekend_true$x,
  main = "Ruch rowerowy w weekend",
  ylim = c(0, 3500),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = weekend_true$starthour,
  col = terrain.colors(24)
  )
barplot(
  weekend_false$x,
  main = "Ruch rowerowy w dniach roboczych",
  ylim = c(0, 3500),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = weekend_false$starthour,
  col = terrain.colors(24)
  )

dev.off()
