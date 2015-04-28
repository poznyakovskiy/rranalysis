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

# print all plots to image files
print.all.images <- function (date, width, height) {
  names <- goods.names ();
  
  for (i in 1:nrow(names)) {
    item <- names$id [i]
    
    # prices
    filename <- paste0 ("out/img/", item, "_price_", Sys.Date(), ".png")
    png (filename, width = width, height = height)
    plot <- get.timescale.price (item, date)
    print (plot)
    dev.off()
    
    # market size
    filename <- paste0 ("out/img/", item, "_amount_", Sys.Date(), ".png")
    png (filename, width = width, height = height)
    plot <- get.timescale.amount (item, date)
    print (plot)
    filename <- paste0 ("out/img/", item, "_amount_", Sys.Date(), ".png")
    dev.off ()
  }
}

print.timescale.price <- function (item, date) {
  plot <- get.timescale.price (item, date)
  print (plot)
}

print.timescale.amount <- function (item, date) {
  plot <- get.timescale.amount (item, date)
  print (plot)
}

# create timescape price plot but do not print it
get.timescale.price <- function (item, date) {
  data <- item.summary (item, date)
  names <- goods.names ();
  
  # scale data for better representation
  factor <- get.scaling.factor (data$Upper.Quantile)
  data$Median <- data$Median / factor$value
  data$Lower.Quantile <- data$Lower.Quantile / factor$value
  data$Upper.Quantile <- data$Upper.Quantile / factor$value
  
  plot <- ggplot(data) + theme_bw() +
          geom_ribbon(aes (x = Day, ymin = Lower.Quantile, ymax = Upper.Quantile),
                      alpha=0.1, color="grey") +
    
          geom_line(aes(x=Day, y=Median), color="steelblue", size=2) +
    
          annotate ("text", label = names$name[item == names$id],
                            x = data$Day[1], y = max (data$Upper.Quantile),
                            hjust = -0.1, vjust = 1, color = "grey", fontface = 2, size = 8) +
    
          annotate ("text", label = sprintf ("%.1f", data$Median[1]),
                            x = data$Day[1], y = data$Median[1],
                            hjust = -0.1, vjust = -1, color = "steelblue", fontface = 2) +
    
          annotate ("text", label = sprintf ("%.1f", data$Median[length(data$Median)]),
                            x = data$Day[length(data$Day)], y = data$Median[length(data$Median)],
                            hjust = 1.1, vjust = -1, color = "steelblue", fontface = 2) +
    
          scale_x_date(labels = date_format("%d.%m"),
                       limits = c(data$Day[1], data$Day[length(data$Day)]),
                       expand = c(0,0)) +
    
          labs(x = "", y = paste0 ("Цена, ", factor$name, "R"))
  
  plot
}

# create timescale market size plot but do not print it
get.timescale.amount <- function (item, date) {
  data <- item.summary (item, date)
  names <- goods.names ();
  
  # scale data for better representation
  factor <- get.scaling.factor (data$Amount)
  data$Amount <- data$Amount / factor$value
  
  plot <- ggplot(data) + theme_bw() +
    geom_area(aes(x=Day, y=Amount), color="grey", alpha = 0.1) +
    
    annotate ("text", label = names$name[item == names$id],
              x = data$Day[1], y = max (data$Amount) * 1.25,
              hjust = -0.1, vjust = 1.3, color = "grey", fontface = 2, size = 8) +
    
    annotate ("text", label = sprintf ("%.f", data$Amount[1]),
              x = data$Day[1], y = data$Amount[1],
              hjust = -0.1, vjust = -1, color = "grey", fontface = 2) +
    
    annotate ("text", label = sprintf ("%.f", data$Amount[length(data$Amount)]),
              x = data$Day[length(data$Day)], y = data$Amount[length(data$Amount)],
              hjust = 1.1, vjust = -1, color = "grey", fontface = 2) +
    
    scale_x_date(labels = date_format("%d.%m"),
                 limits = c(data$Day[1], data$Day[length(data$Day)]),
                 expand = c(0,0)) +
    
    scale_y_continuous (limits = c(0, max (data$Amount) * 1.25), expand = c(0,0)) +
    
    labs(x = "", y = paste0("Объем рынка, ", factor$name,"ед."))
  
  plot
}

get.scaling.factor <- function (x) {
  maxval <- max (x)
  res <- c (0, "")
  names (res) <- c ("value", "name")
  if (maxval >= 1e+13) {
    res$value <- 1e+12
    res$name <- "трлн. "
  } else if (maxval >= 1e+10) {
    res$value <- 1e+9
    res$name <- "млрд.  "
  } else if (maxval >= 1e+7) {
    res$value <- 1e+6
    res$name <- "млн. "
  } else if (maxval >= 1e+4) {
    res$value <- 1e+3
    res$name <- "тыс. "
  } else {
    res$value <- 1
    res$name <- ""
  }
  res
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