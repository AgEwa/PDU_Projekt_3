source("support.R")
library(dplyr)


x <- data.frame()
i <- 0
cat("0%")
"
 blok z forem wczytuje po kolei dane z 3 miesięcy 2014 roku i po odrzuceniu wierszy z początkową i końcową stacją tą samą oraz wybraniu
 odpowiednich kolumn scala wszytkie miesiące w jedną ramkę danych x
  "
for (m in c("05", "06", "07")) {
  name = paste("2014", m, sep="")
  d <- get_data(name)
  d %>% 
    filter(start.station.id != end.station.id) %>%
    select(tripduration, start.station.id,start.station.name, end.station.id, end.station.name)->d
  x <- merge(x, d, all=TRUE)
  i <- i+1
  cat(sprintf("\r%.2f%%", i*100/3))  # progress
}
"
 a - zawiera informacje o stacjach i ilokrotnie trasa została przebyta
 b - zawiera informacje o długości podróży dla najpopularniejszej trasy (pobranej z a)
  "

x %>%
  count(start.station.id, start.station.name, end.station.id, end.station.name)%>%
  arrange(desc(n))-> a
names(a)<-c("start.station.id", "start.station.name", "end.station.id", "end.station.name", "popularity")
head(a)
x %>%
  filter(start.station.id == a[1,1] & end.station.id == a[1,3])%>%
  arrange(tripduration)->b
head(b)
