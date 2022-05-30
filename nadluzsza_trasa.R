# obliczenia dotyczące najdłuższej trasy w linii prostej

source("support.R")


d <- get_data("201405")

d[,"distance"] <- sqrt((d$start.station.latitude-d$end.station.latitude)**2 +
  (d$start.station.longitude-d$end.station.longitude)**2)
d <- d[order(-d$distance),][1:10,]

d[,"tripduration"] = d[,"tripduration"]/60

d2 <- get_data("202105")

d2[,"distance"] <- sqrt((d2$start_lat-d2$end_lat)**2 +
                         (d2$start_lng-d2$end_lng)**2)
d2 <- d2[order(-d2$distance),][1:10,]

d2[,"tripduration"] = (as.numeric(as.POSIXlt(d2$ended_at)) - as.numeric(as.POSIXlt(d2$started_at)))/60
