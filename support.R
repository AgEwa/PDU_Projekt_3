get_data <- function(num, jc=FALSE){
  "Funkcja wczytuje z pliku ramkę danych. Plik powinien znajdować się w folderze
  data w katalogu projektu. Jeśli plik nie istnieje, zostaje automatycznie pobrany.
  Argument to jednoelementowy wektor typu character postaci RokMiesiąc,
  np. '201405' (dane z maja 2014 r.). Parametr jc określa, czy dane mają być pobierane dla
  Jersey City, czy Nowego Jorku - domyślnie ma wartość FALSE (Nowy Jork)"
  stopifnot(length(num)==1)
  if(!dir.exists("./data")){      # tworzenie katalogu na dane, jeśli nie istnieje
    dir.create("./data")
  }
  name <- paste(num, "-citibike-tripdata", sep="")
  if(jc){
    name <- paste("JC-", name, sep="")
  }
  if (as.integer(substr(num, 1, 4)) < 2015 & !jc){        # określ nazwę pliku w zależności od roku
    d <- c(substr(num, 1, 4), substr(num, 5, 6))
    f_name <- paste("./data/", d[1], "-", d[2], " - Citi Bike trip data.csv", sep="")
  } else {
    f_name <- paste("./data/", name, ".csv", sep="")
  }
  if(jc | as.integer(substr(num, 1, 4)) >= 2017){
    name <- paste(name, ".csv", sep="")
  }
  if(!file.exists(f_name)){       # pobierz, jeśli plik nie istnieje
    download.file(
      paste("https://s3.amazonaws.com/tripdata/", name, ".zip", sep=""),
      paste("./data/", name, ".zip", sep="")
    )
    unzip(paste("./data/", name, ".zip", sep=""), exdir = "./data")  # rozpakowanie archiwum
  }
  return(read.csv(f_name))    # zwraca data.frame
}
