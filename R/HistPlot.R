#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param dataset This is the dataset used to create the histograms.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Histogram
#' @export

hist_records <- function(dataset, day_one, site, year) {
  library(plyr)
  library(dplyr)
  
  if(missing(site)) {
    site <- ""
  }
  if(missing(year)) {
    year <- ""
  }
  
  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(missing(day_one)) {
    day_one <- min(dataset$jday)
  }
  d1 <- subset(dataset, jday >= day_one)
  d <- select(d1, channel, description, signal)
  
  fig.events <- paste(getwd(), site, year, "EventsbyChannel.pdf", sep="")
  pdf(fig.events, height=10, width=10)
  par(mfrow=c(length(unique(d$channel)), 1), 
      mar=c(4, 3, 3, 1), 
      oma=c(2, 2, 1, 0), 
      las=1, 
      xaxs="i", 
      yaxs="i")
  
  no.events <- ddply(filter(d, description=="E"), c("channel"), function(x) {
    hist(x$signal, breaks=seq(0,130,5), xlim=c(0, 130), main="", ylab="", 
         xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.events<-length(x$signal)
    data.frame(no.events)
  }
  )
  
  mtext("Frequency of EVENT signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.events)
  
  fig.up <- paste(getwd(), site, year, "UpsbyChannel.pdf", sep="")
  pdf(fig.up, height=10, width=10)
  par(mfrow=c(length(unique(d$channel)), 1), 
      mar=c(4, 3, 3, 1), 
      oma=c(2, 2, 1, 0), 
      las=1, 
      xaxs="i", 
      yaxs="i")
  
  no.up <- ddply(filter(d, description=="U"), c("channel"), function(x) {
    hist(x$signal, breaks=seq(0,130,5), xlim=c(0, 130), main="", ylab="", 
         xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.up <- length(x$signal)
    data.frame(no.up)
  }
  )
  
  mtext("Frequency of UP signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.up)
  
  fig.down <- paste(getwd(), site, year, "DownsbyChannel.pdf", sep="")
  pdf(fig.down, height=10, width=10)
  par(mfrow=c(length(unique(d$channel)), 1), 
      mar=c(4, 3, 3, 1), 
      oma=c(2, 2, 1, 0), 
      las=1, 
      xaxs="i", 
      yaxs="i")
  
  no.down <- ddply(filter(d, description=="D"), c("channel"), function(x) {
    hist(x$signal, breaks=seq(0,130,5), xlim=c(0, 130), main="", ylab="", 
         xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.down <- length(x$signal)
    data.frame(no.down)
  }
  )
  
  mtext("Frequency of DOWN signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.down)
}
