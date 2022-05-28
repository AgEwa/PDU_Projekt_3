# obliczenia dotyczące liczby wypożyczonych rowerów w weekend i w dniach roboczych

source("support.R")

godziny_weekend <- function(frame, weekend, subscriber){
  "
  Funkcja oblicza średnią liczbę rozpoczętych w danym miesiącu przejazdów dla
  każdej godziny w weekend, jeśli weekend=TRUE, lub w dniach roboczych gdy weekend=FALSE.
  Dane dla stałych klientów, gdy subscriber=TRUE, w przeciwnym razie dla pozostałych klientów.
  "
  if(weekend){
    frame <- frame[frame$weekday=="7"|frame$weekday=="6",]
  } else{
    frame <- frame[frame$weekday!="7"&frame$weekday!="6",]
  }
  if(subscriber){
    frame <- frame[frame$member_casual=="member",]
  }else{
    frame <- frame[frame$member_casual=="casual",]
  }
  frame <- aggregate(frame$ride_id, frame[,c("starthour", "starttime")], length)
  frame <- aggregate(frame$x, frame[,"starthour", drop=FALSE], mean)
  return(frame)
}


subscriber_weekend <- data.frame()
subscriber_no <- data.frame()
customer_weekend <- data.frame()
customer_no <- data.frame()
i <- 0
cat("0%")       # progress
for (m in c("05", "06", "07")) {
  name = paste("2021", m, sep="")
  d <- get_data(name)
  
  d[,"starthour"] <- substr(d$started_at, 12, 13)
  d[,"weekday"] <- strftime(as.Date(d$started_at), format="%u")
  d[,"starttime"] <- substr(d$started_at, 1, 10)
  
  m_data_sw <- godziny_weekend(d, weekend = TRUE, subscriber = TRUE)
  m_data_sn <- godziny_weekend(d, weekend = FALSE, subscriber = TRUE)
  m_data_cw <- godziny_weekend(d, weekend = TRUE, subscriber = FALSE)
  m_data_cn <- godziny_weekend(d, weekend = FALSE, subscriber = FALSE)
  
  subscriber_weekend <- merge(subscriber_weekend, m_data_sw, all=TRUE)
  subscriber_no <- merge(subscriber_no, m_data_sn, all=TRUE)
  customer_weekend <- merge(customer_weekend, m_data_cw, all=TRUE)
  customer_no <- merge(customer_no, m_data_cn, all=TRUE)
  
  i <- i+1
  cat(sprintf("\r%.2f%%", i*100/3))  # progress
}
subscriber_weekend <- aggregate(
  subscriber_weekend$x,
  subscriber_weekend[,"starthour", drop=FALSE],
  mean
  )
subscriber_no <- aggregate(
  subscriber_no$x,
  subscriber_no[,"starthour", drop=FALSE],
  mean
  )
customer_weekend <- aggregate(
  customer_weekend$x,
  customer_weekend[,"starthour", drop=FALSE],
  mean
  )
customer_no <- aggregate(
  customer_no$x,
  customer_no[,"starthour", drop=FALSE],
  mean
  )

# png 1

png(filename = "member-2021-weekend.png")
par(mfrow=c(2, 1))

barplot(
  subscriber_weekend$x,
  main = "Ruch rowerowy w weekend",
  ylim = c(0, max(c(subscriber_weekend$x, subscriber_no$x))),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = subscriber_weekend$starthour,
  col = terrain.colors(24)
  )
barplot(
  subscriber_no$x,
  main = "Ruch rowerowy w dniach roboczych",
  ylim = c(0, max(c(subscriber_weekend$x, subscriber_no$x))),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = subscriber_no$starthour,
  col = terrain.colors(24)
  )

dev.off()

# png 2

png(filename = "casual-2021-weekend.png")
par(mfrow=c(2, 1))

barplot(
  customer_weekend$x,
  main = "Ruch rowerowy w weekend",
  ylim = c(0, max(c(customer_weekend$x, customer_no$x))),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = customer_weekend$starthour,
  col = terrain.colors(24)
)
barplot(
  customer_no$x,
  main = "Ruch rowerowy w dniach roboczych",
  ylim = c(0, max(c(customer_weekend$x, customer_no$x))),
  xlab = "godzina",
  ylab = "średnia liczba rowerzystów",
  names.arg = customer_no$starthour,
  col = terrain.colors(24)
)

dev.off()

