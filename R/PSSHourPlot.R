#' A function that plots the peak signal size (PSS) by hour of Logie counter data
#'
#' This function plots the PSS by hour for Logie counter data
#' @param dataset This is the dataset used to create the plots.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param low_thresh is the counters lower threshold PSS value.
#' @param up_thresh is the counters upper threshold PSS value.
#' @keywords Events
#' @export

plot_pss_hour<-function(dataset, day_one, site, year, low_thresh, up_thresh) {
  
  if(missing(low_thresh)) {
    low_thresh <- 0
  }
  if(missing(up_thresh)) {
    up_thresh <- 130
  }
  if(missing(site)) {
    site <- ""
  }
  if(missing(year)) {
    year <- ""
  }
  
  dataset$date.alt <- strptime(dataset$date, '%Y-%m-%d')
  dataset$jday     <- dataset$date.alt$yday
  
  if(missing(day_one)) {
    day_one <- min(dataset$jday)
  }
  
  dataset          <- subset(dataset, jday >= day_one)
  dataset$jday     <- NULL
  dataset$date.alt <- NULL
  dataset$hour     <- strptime(dataset$time, format = "%H:%M:%S")
  dataset$hour     <- as.POSIXct(round(dataset$hour, "mins"))
  
  pdf(paste(getwd(), "PeakSignalSizebyHour", site, year, ".pdf", sep = ""), 
      height = 10,
      width = 10)
  
  par(mfrow = c(1, 1), 
      mar = c(2, 2, 2, 2), 
      oma = c(2, 2, 2, 2),
      cex = 1.5)
  
  plot(signal ~ hour, data = subset(dataset, description == "U"),
       col = "#00000010", pch = 19, cex = 1.5, axes = FALSE, las = 1, 
       xlab = "", ylab = "", ylim = c(low_thresh, up_thresh))
  
  axis.POSIXct(1, dataset$hour, format = "%H:%M", cex.axis = 1, col = "grey60")
  
  axis(2, las = 1, col = "grey60")
  
  box(col = "grey60")
  
  mtext("Peak signal", 
        side = 2, 
        line = 2.5, 
        outer = FALSE, 
        cex = 1.5)
  
  mtext("Time of Day", 
        side = 1, 
        line = 3, 
        outer = FALSE, 
        cex = 1.5)
  
  dev.off()
}