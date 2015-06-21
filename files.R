library (stringr)

# lists files for a specific item and a specific date range
# start and end are either dates or offsets to the past
# in the latter case, they are represented by negative integers
list.item.files <- function (item, start, end) {
  dir <- "data/raw/"
  if (start <= 0) {
    start.date <- Sys.Date() + start
  } else {
    start.date <- start
  }
  
  if (end <= 0) {
    end.date <- Sys.Date() + end
  } else {
    end.date <- end
  }
  
  list <- paste0 (dir, list.files (dir, paste0 ("^", as.character (item), "_.+csv$")))
  
  # returns only the files whose dates match given bounds
  dates <- dates (list)
  list[ dates >= start.date & dates <= end.date ]
}

# extracts available dates from the file list
dates <- function (path) {
  unname (sapply (path, function (x) {
    as.Date (str_extract (x, "_.*_"), "_%y%m%d_")
  }))
}

# finds the file corresponding to the specific item and date and reads it
read.item <- function (path) {
  read.csv (path, header = FALSE,
            col.names = c ("name", "location", "amount", "distance", "ppu", "ppu.adjusted"),
            colClasses = c ("character", "character", "numeric", "numeric", "numeric", "numeric"))
}

# returns given price quantiles from a set of files
price.quantiles <- function (path, probs = 0.5) {
  unname (sapply (path, function (x) {
    data <- read.item (x)
    wtd.quantile (data$ppu, weights=data$amount, probs = probs)
  }))
}

# returns total amounts from a set of files
amounts <- function (path) {
  unname (sapply (path, function (x) {
    data <- read.item (x)
    sum (data$amount)
  }))
}

# returns summary for a given item by date
item.summary <- function (item, start, end) {
  path <- list.item.files (item, start, end)
  
  prices <- price.quantiles (path, c(0.5, 0.25, 0.75))
  amounts <- amounts (path)
  dates <- dates (path);
  
  item.data <- data.frame (dates, prices[1,], prices[2,], prices[3,], amounts)
  colnames(item.data) <- c ("Day", "Median", "Lower.Quantile", "Upper.Quantile", "Amount")
  item.data$Day <- as.Date (item.data$Day, origin = "1970-01-01")
  item.data
}

# exports summarized data to files
export.summary <- function (item, start, end) {
  data <- item.summary (item, start, end)
  write.csv (data, file = paste0 ("data/items/", as.character (item), ".csv"), row.names=FALSE)
}

#export summarized data for all items
export.summary.all <- function (start, end) {
  id <- goods.names()$id
  sapply (id, function (x) {
    export.summary (x, start, end)
  })
  "Done"
}

# gets the proper names of the goods
goods.names <- function () {
  read.csv ("goods.csv", header = FALSE,
            col.names = c ("id", "name"),
            colClasses = c ("numeric", "character"))
}

# gets the proper names of the good with a given id
goods.name <- function (id) {
  names <- goods.names ();
  names$name[id == names$id]
}