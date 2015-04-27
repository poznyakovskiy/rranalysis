library (ggplot2)
library (ENmisc)
library (scales)

source ("files.R")

plot.single.day <- function (item, date) {
  path <- get.item.date.path (item, date)
  data <- read.item (path)
  
  bpstats <- wtd.boxplot (data$ppu, data$amount)$stats
  plot <- ggplot (data, aes (x = c(1), y = ppu, weight = amount)) +
    geom_boxplot(outlier.size = 0) +
    coord_cartesian (ylim = c(0, bpstats[5] * 1.05))
  print (plot)
}

get.timescale.price <- function (item, date) {
  data <- item.summary (item, date)
  names <- goods.names ();
  
  plot <- ggplot(data) + theme_bw() +
          geom_ribbon(aes (x = Day, ymin = Lower.Quantile, ymax = Upper.Quantile),
                      alpha=0.1, color="grey") +
          geom_line(aes(x=Day, y=Median), color="steelblue", size=2) +
          annotate ("text", label = names$name[item == names$id],
                            x = data$Day[1], y = max (data$Upper.Quantile),
                            hjust = -0.1, vjust = 1, fontface = 2, size = 12) +
          annotate ("text", label = data$Median[1],
                            x = data$Day[1], y = data$Median[1],
                            hjust = -0.1, vjust = -1, color = "steelblue", fontface = 2) +
          annotate ("text", label = data$Median[length(data$Median)],
                            x = data$Day[length(data$Day)], y = data$Median[length(data$Median)],
                            hjust = 1.1, vjust = -1, color = "steelblue", fontface = 2) +
          scale_x_date(labels = date_format("%d.%m"),
                       limits = c(data$Day[1], data$Day[length(data$Day)]),
                       expand = c(0,0)) +
          labs(x = "", y = "Цена, R")
  
  print (plot)
}

get.timescale.amount <- function (item, date) {
  data <- item.summary (item, date)
  names <- goods.names ();
  
  plot <- ggplot(data) + theme_bw() +
    geom_area(aes(x=Day, y=Amount), color="grey", alpha = 0.1) +
    annotate ("text", label = names$name[item == names$id],
              x = data$Day[1], y = max (data$Amount) * 1.25,
              hjust = -0.1, vjust = 1.3, fontface = 2, size = 12) +
    annotate ("text", label = data$Amount[1],
              x = data$Day[1], y = data$Amount[1],
              hjust = -0.1, vjust = -1, color = "grey", fontface = 2) +
    annotate ("text", label = data$Amount[length(data$Amount)],
              x = data$Day[length(data$Day)], y = data$Amount[length(data$Amount)],
              hjust = 1.1, vjust = -1, color = "grey", fontface = 2) +
    scale_x_date(labels = date_format("%d.%m"),
                 limits = c(data$Day[1], data$Day[length(data$Day)]),
                 expand = c(0,0)) +
    scale_y_continuous (limits = c(0, max (data$Amount) * 1.25), expand = c(0,0)) +
    labs(x = "", y = "Объем рынка, ед.")
  
  print (plot)
}

price.quantiles <- function (item, date, probs = 0.5) {
  path <- get.item.date.path (item, date)
  
  unname (sapply (path, function (x) {
    data <- read.item (x)
    wtd.quantile (data$ppu, weights=data$amount, probs = probs)
  }))
}

market.size <- function (item, date) {
  path <- get.item.date.path (item, date)
  
  unname (sapply (path, function (x) {
    data <- read.item (x)
    sum (data$amount)
  }))
}

item.summary <- function (item, date) {
 prices <- price.quantiles (item, date, c(0.5, 0.25, 0.75))
 amounts <- market.size (item, date)
 dates <- as.character(Sys.Date() + date);
 
 item.data <- data.frame (dates, prices[1,], prices[2,], prices[3,], amounts)
 colnames(item.data) <- c ("Day", "Median", "Lower.Quantile", "Upper.Quantile", "Amount")
 item.data$Day <- as.Date (item.data$Day)
 item.data
}