#' plot_abundance
#' A function to bind and process Logie signal data
#' This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
#' @param dataset This is the object containing the data for plotting.
#' @param day_one This is the first day the data should start on.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Logie
#' @export

plot_abundance <- function(dataset, day_one, site, year) {
  
  library(plyr)
  library(dplyr)
  
  if(missing(site)) {
    site <- ""
  }
  if(missing(year)) {
    year <- ""
  }
  
  dataset$date.alt  <- strptime(dataset$date, '%Y-%m-%d')
  dataset$jday      <- dataset$date.alt$yday
  
  if(missing(day_one)) {
    day_one <- min(dataset$jday)
  }
  
  dataset           <- subset(dataset, jday>=day_one)
  dataset$jday      <- NULL
  dataset$date.alt  <- NULL
  dataset$date.time <- strptime(paste(dataset$date, dataset$time, sep=" "), 
                                "%Y-%m-%d %H:%M:%S")
  
  dataset$date.time <- as.POSIXct(round(dataset$date.time, "day"))
  
  updata <- data.frame(filter(dataset, description == "U"), count = 1) 
  updata$cummulative.count <- cumsum(updata$count)
  
  up <- ddply(updata, c("date.time"), summarize, 
              daily.count = sum(count), 
              count = max(cummulative.count))
  
  pdf(paste(getwd(), "AbundancebyDay", site, year, ".pdf", 
            sep = ""), height = 10, width = 10)       
  par(mfrow = c(2, 1), 
      mar = c(0, 2, 0, 2), 
      oma = c(4, 2, 2, 2))
  
  plot(daily.count ~ date.time, data = up, 
       pch = 19, 
       cex = 1.5, 
       axes = FALSE, 
       las = 1, 
       xlab = "", 
       ylab = "",
       type = "b")
  
  mtext("Fish per day", side = 2, line = 2.5, outer = FALSE, cex = 1.5)  
  
  axis(2, las=1)
  box()
  
  plot(count ~ date.time, data = up, 
       ylim = c(0, max(up$count) * 1.1), 
       typ = "b", 
       xlab = "", 
       ylab = "", 		
       axes = FALSE, 
       lwd = 2, 
       pch = 19)
  
  mtext("Total no. of fish", side=2, line = 2.5, cex = 1.5, las = 0)
  mtext("Date", side=1, line=4,cex=1.5)
  
  r <- as.POSIXct(range(up$date.time))
  axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=0.85)
  axis(2, las=1)
  box()
  
  dev.off()
}