#' A function that plots the peak signal size (PSS) by date and time per hour of Logie counter data
#'
#' This function plots the PSS by day and time for Logie counter data
#' @param dataset This is the dataset used to create the plots.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param low_thresh is the counters lower threshold PSS value.
#' @param up_thresh is the counters upper threshold PSS value.
#' @keywords Events
#' @export

plot_pss_date <- function(dataset, day_one, site, year, low_thresh, up_thresh) {
  
  if(missing(low_thresh)) {
    low.thresh <- 0
  }
  if(missing(up_thresh)) {
    up.thresh <- 130
  }
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
  
  dataset$date.time <- as.POSIXct(round(dataset$date.time, "hours"))
  
  pdf(paste(getwd(), "Peak Signal Size by Date and Time", site, year, ".pdf", 
            sep=""), height=10, width=10)       
  
  par(mfrow=c(1, 1), 
      mar=c(2, 2, 2, 2), 
      oma=c(2, 2, 2, 2))
  
  plot(signal ~ date.time, data=subset(dataset, description=="U"), 
       col="#00000010", 
       pch=19, 
       cex=1.5, 
       axes=FALSE, 
       las=1, 
       xlab="", 
       ylab="", 
       ylim=c(low_thresh, up_thresh))
  
  par(new=TRUE)
  
  mean.signal <- ddply(subset(dataset, description=="U"), c("date"), 
                       summarize, mean.signal=mean(signal))
  
  mean.signal$date.alt <- as.POSIXct(strptime(mean.signal$date, "%Y-%m-%d"))
  
  plot(mean.signal ~ date.alt, data=mean.signal, typ="p", col="red", pch=19, 
       cex=1.5, axes=FALSE, las=1, xlab="", ylab="", 
       ylim=c(low.thresh, up.thresh))
  
  r <- as.POSIXct(range(mean.signal$date.alt))
  
  axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=0.85, 
               col="grey60")
  
  axis(2, las=1, col="grey60")
  
  box(col="grey60")
  
  mtext("Peak signal", side=2, line=2.5, outer=FALSE, cex=1.5)
  
  mtext("Date", side=1, line=3, outer=FALSE, cex=1.5)
  
  dev.off()
}