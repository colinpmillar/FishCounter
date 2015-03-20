#' A function that plots the peak signal size (PSS) by date and time per hour of Logie counter data
#'
#' This function plots the PSS by day and time for Logie counter data
#' @param data This is the dataset used to create the plots.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param low.thresh is the counters lower threshold PSS value.
#' @param up.thresh is the counters upper threshold PSS value.
#' @keywords Events
#' @export

######################################################
pss.date.plot<-function(data, first.day, site, year, low.thresh, up.thresh){
  
  data$date.time<-strptime(paste(data$date, data$time, sep=" "), "%Y-%m-%d %H:%M:%S")
  data$date.time <- as.POSIXct(round(data$date.time, "hours"))
  
  pdf(paste(getwd(), "Peak Signal Size by Date and Time", site, year, ".pdf", sep=""), height=10, width=10)       
  par(mfrow=c(1,1), 
      mar=c(2,2,2,2), 
      oma=c(2,2,2,2))
  
  plot(signal~date.time, data=subset(data, description=="U"), 
       col="#00000010", 
       pch=19, 
       cex=1.5, 
       axes=FALSE, 
       las=1, 
       xlab="", 
       ylab="", 
       ylim=c(low.thresh, up.thresh))
  par(new=TRUE)
  
  mean.signal<-ddply(subset(data, description=="U"), c("date"), summarize, mean.signal=mean(signal))
  mean.signal$date.alt<-as.POSIXct(strptime(mean.signal$date, "%Y-%m-%d"))
  plot(mean.signal~date.alt, data=mean.signal, typ="p",col="red", pch=19, cex=1.5, axes=FALSE, las=1, xlab="", ylab="", ylim=c(low.thresh, up.thresh))
  r<-as.POSIXct(range(mean.signal$date.alt))
  axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=0.85, col="grey60")
  axis(2, las=1, col="grey60")
  box(col="grey60")
  mtext("Peak signal", side=2, line=2.5, outer=FALSE, cex=1.5)
  mtext("Date", side=1, line=3, outer=FALSE, cex=1.5)
  
  dev.off()
}

#########
# Check to see that the function works

#setwd("/Users/doug/Desktop/Example Data/Counter/")
#data<-read.csv("Scotland2014.csv")
  
#first.day<-220
#site<-"NotSure"
#year<-2200
#low.thresh<-40
#up.thresh<-127
#pss.date.plot(data, first.day, site, year, low.thresh, up.thresh)