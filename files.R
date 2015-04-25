# lists files for a specific item, across all dates
list.item.files <- function (dir, item) {
  list.files (dir, paste0 ("^", as.character (item), ".+csv$"))
}

# within a file list, finds the file that corresponds to a specific date
# 'date' must be a negative integer representing days back from the current day
# TODO: allow passing an arbitrary date to 'date'
get.by.date <- function (list, date) {
  
  # get current date and substract the required amount of days
  date <- Sys.Date() + date
  
  # get date in my format: remove dashes and the two first digits
  date <- substr (gsub("-", "", date), 3, 8)
  
  list [grep (date, list)]
}

# returns the path to the data file for a specific item and date
get.item.date.path <- function (item, date) {
  dir <- "data/raw/"
  filename <- get.by.date (list.item.files (dir, item), date)
  if (filename == "") {
    filename
  } else {
    paste0 (dir, filename)
  }
}

# finds the file corresponding to the specific item and date and reads it
read.item.date <- function (item, date) {
  path <- get.item.date.path (item, date)
  if (path == "") {
    NULL
  } else {
    read.csv (path, header = FALSE,
              col.names = c ("name", "location", "amount", "distance", "ppu", "ppu.adjusted"),
              colClasses = c ("character", "character", "numeric", "numeric", "numeric", "numeric"))
  }
}