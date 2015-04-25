# enclose username with quotes
enclose.username <- function (str) {
  gsub ('^(.+)((,.*){5})$', '\"\\1\"\\2', str)
}

# correct quotes in a single file
correct.quotes <- function (path) {
  strings <- read.csv (path, header = FALSE, sep = "\n", colClasses = "character")
  strings [,1] <- sapply (strings[,1], enclose.username)
  write.table (strings, path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# correct quotes in all files
correct.quotes.all <- function (dir) {
  paths <- list.files (dir, ".+csv$")
  paths <- paste0 (dir, paths)
  sapply (paths, correct.quotes)
}