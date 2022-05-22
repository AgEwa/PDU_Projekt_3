get_data <- function(num){
  name <- paste(num, "-citibike-tripdata", sep="")
  if (as.integer(substr(num, 1, 4)) < 2015){
    d <- c(substr(num, 1, 4), substr(num, 5, 6))
    f_name <- paste("./data/", d[1], "-", d[2], " - Citi Bike trip data.csv", sep="")
  } else {
    f_name <- paste("./data/", name, ".csv", sep="")
  }
  if(!file.exists(f_name)){
    download.file(
      paste("https://s3.amazonaws.com/tripdata/", name, ".zip", sep=""),
      paste("./data/", name, ".zip", sep="")
    )
    unzip(paste("./data/", name, ".zip", sep=""), exdir = "./data")
  }
  return(read.csv(f_name))
}
