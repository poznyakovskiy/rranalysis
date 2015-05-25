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

# prints all plots to image files
print.all.images <- function (start, end, width, height) {
  names <- goods.names ();
  
  for (i in 1:nrow(names)) {
    item <- names$id [i]
    
    # prices
    filename <- paste0 ("out/img/", item, "_price_", Sys.Date(), ".png")
    png (filename, width = width, height = height)
    plot <- get.timescale.price (item, start, end)
    print (plot)
    dev.off()
    
    # market size
    filename <- paste0 ("out/img/", item, "_amount_", Sys.Date(), ".png")
    png (filename, width = width, height = height)
    plot <- get.timescale.amount (item, start, end)
    print (plot)
    dev.off ()
  }
}

# exports summarized data for all items
export.all.summaries <- function (start, end) {
  names <- goods.names ();
  
  for (i in 1:nrow(names)) {
    export.summary (names$id [i], start, end) 
  }
}

print.timescale.price <- function (item, start, end) {
  plot <- get.timescale.price (item, start, end)
  print (plot)
}

print.timescale.amount <- function (item, start, end) {
  plot <- get.timescale.amount (item, start, end)
  print (plot)
}

# create timescape price plot but do not print it
get.timescale.price <- function (item, start, end) {
  name <- goods.name (item);
  data <- item.summary (item,  start, end)
  
  # scale data for better representation
  factor <- get.scaling.factor (data$Upper.Quantile)
  data$Median <- data$Median / factor$value
  data$Lower.Quantile <- data$Lower.Quantile / factor$value
  data$Upper.Quantile <- data$Upper.Quantile / factor$value
  
  last <- dim (data)[1];
  
  plot <- ggplot(data) +
    
    geom_ribbon(aes (x = Day, ymin = Lower.Quantile, ymax = Upper.Quantile),
                    alpha=0.1, color="grey") +
    
    geom_line(aes(x=Day, y=Median), color="steelblue", size=2)
  
  plot <- add.common.plot.elements (plot, name = name,
                        x0 = data$Day[1], xmax = data$Day[length(data$Day)],
                        maxval = max (data$Upper.Quantile),
                        ylab = paste0("Цена, ", factor$name, "R"))
  
  plot <- add.value.annotation (plot, x = data$Day[1], y = data$Median[1], hjust = -0.1, color = "steelblue")
  plot <- add.value.annotation (plot, x = data$Day[last], y = data$Median[last], hjust = 1.1, color = "steelblue")
  
  plot
}

# create timescale market size plot but do not print it
get.timescale.amount <- function (item, start, end) {
  name <- goods.name (item);
  data <- item.summary (item, start, end)
  
  # scale data for better representation
  factor <- get.scaling.factor (data$Amount)
  data$Amount <- data$Amount / factor[["value"]]
  
  last <- dim (data)[1];
  
  plot <- ggplot(data) + 
    
    geom_area(aes(x=Day, y=Amount), color="grey", alpha = 0.1) +
    
    scale_y_continuous (limits = c(0, max (data$Amount) * 1.25), expand = c(0,0)) 
  
  plot <- add.common.plot.elements (plot, name = name,
                                x0 = data$Day[1], xmax = data$Day[last],
                                maxval = max (data$Amount),
                                ylab = paste0("Объем рынка, ", factor[["name"]],"ед."))
  
  plot <- add.value.annotation (plot, x = data$Day[1], y = data$Amount[1], hjust = -0.1, color = "grey")
  plot <- add.value.annotation (plot, x = data$Day[last], y = data$Amount[last], hjust = 1.1, color = "grey")
  
  plot
}

# computes the scaling factor for plot representations
get.scaling.factor <- function (x) {
  maxval <- max (x)
  res <- c (0, "")
  res <- as.data.frame (t (res))
  colnames (res) <- c ("value", "name")
  res$value <- as.numeric (res$value)
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

# adds common plot elements and styles to plot
add.common.plot.elements <- function (plot, name, x0, xmax, maxval, ylab) {
  plot + theme_bw() +
    
  annotate ("text",
            label = name,
            x = x0,
            y = maxval * 1.25,
            hjust = -0.1,
            vjust = 1.3,
            color = "grey",
            fontface = 2,
            size = 8) +
  
  scale_x_date(labels = date_format("%d.%m"),
                limits = c(x0, xmax),
                expand = c(0,0)) +
  
  labs(x = "", y = ylab)
}

# adds a specific value as annotation
add.value.annotation <- function (plot, x, y, color, hjust) {
  plot + annotate ("text", label = sprintf ("%.f", y), x = x, y = y,
            hjust = hjust, vjust = -1, color = color, fontface = 2)
}