library (ggplot2)
library (ENmisc)

source ("files.R")

print.single.day <- function (item, date) {
  data <- read.item.date (item, date)
  
  bpstats <- wtd.boxplot (data$ppu, data$amount)$stats
  plot <- ggplot (data, aes (x = c(1), y = ppu, weight = amount)) +
    geom_boxplot(outlier.size = 0) +
    coord_cartesian (ylim = c(0, bpstats[5] * 1.05))
  print (plot)
}
