source("../support.R")
library(dplyr)


x <- data.frame()
#y<- data.frame()
i <- 0
cat("0%")
for (m in c("05", "06", "07")) {
  name = paste("2014", m, sep="")
  d <- get_data(name)
  d %>% 
    filter(birth.year != "\\N") %>% 
   select(tripduration, birth.year)->d
  x <- merge(x, d, all=TRUE)
  i <- i+1
  cat(sprintf("\r%.2f%%", i*100/3))  # progress
}
x %>%
  group_by(birth.year)%>%
  summarise(Mean = mean(tripduration))->x
getThePlot<-function(NYC, mini, maxi){
  if(NYC){
    x %>%
      filter(birth.year>=mini & birth.year<=maxi)->a
    barplot(
      a$Mean,
      main = "średnia gługość wypożyczenia dla wieku",
      ylim = c(0, 2500),
      xlab = "wiek",
      ylab = "średni czas",
      names.arg = a$birth.year,
      col = terrain.colors(24)
    )
  }
}
