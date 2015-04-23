list.item.files <- function (item) {
  list.files ("data/raw/", paste ("^", as.character (item), ".+csv$", sep=""))
}

get.by.date <- function (list, date) {
  # get current date and substract the required amount of days
  date <- Sys.Date() + date
  
  # get date in my format: remove dashes and the two first digits
  date <- substr (gsub("-", "", date), 3, 8)
  
  list [grep (date, list)]
}

get.item.date <- function (item, date) {
  get.by.date (list.item.files (item), date)
}