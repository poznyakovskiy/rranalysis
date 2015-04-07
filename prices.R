library (ggplot2)
library (ENmisc)

data <- read.csv ("data.csv",
                  header = FALSE,
                  col.names = c ("name", "location", "amount", "distance", "ppu", "ppu.adjusted"),
                  colClasses = c ("character", "character", "numeric", "numeric", "numeric", "numeric"))

bpstats <- wtd.boxplot (data$ppu, data$amount)$stats
plot <- ggplot (data, aes (x = c(1), y = ppu, weight = amount)) +
        geom_boxplot(outlier.size = 0) +
        coord_cartesian (ylim = c(0, bpstats[5] * 1.05))
print (plot)