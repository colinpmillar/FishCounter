#' A function that plots the peak signal size (PSS) by hour of Logie counter data
#'
#' This function plots the PSS by hour for Logie counter data
#' @param dataset This is the dataset used to create the plots.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param low.thresh is the counters lower threshold PSS value.
#' @param up.thresh is the counters upper threshold PSS value.
#' @param sunrise is the aproximate time of sunrise needs to be an integer value between 0 and 12
#' @param sunset is the aproximate time of sunrise needs to be an integer value 12 and 24
#' @keywords Events
#' @export


######################################################
sunrise<-8
sunset<-16
pss.date.plot<-function(dataset, first.day, site, year, low.thresh, up.thresh, sunrise, sunset){
  
  dataset$hour<-strptime(dataset$time, format="%H:%M:%S")
  dataset$hour <- as.POSIXct(round(dataset$hour, "mins"))
  
  pdf(paste(getwd(), "Peak Signal Size by Hour", site, year, ".pdf", sep=""), height=10, width=10)
  #quartz()
  par(mfrow=c(1,1), 
      mar=c(2,2,2,2), 
      oma=c(2,2,2,2))
  
  plot(signal~hour, data=subset(dataset, description=="U"), col="#00000010", pch=19, cex=1.5, axes=FALSE, las=1, xlab="", ylab="", ylim=c(0,130))
  axis.POSIXct(1, dataset$hour, format="%H:%M", cex.axis=1, col="grey60")
  axis(2, las=1, col="grey60")
  box(col="grey60")
  mtext("Peak signal", side=2, line=2.5, outer=FALSE, cex=1.5)
  mtext("Time of Day", side=1, line=3, outer=FALSE, cex=1.5)
  par(new=TRUE)
  plot(1, xlim=c(0,24), ylim=c(60,120), col="#00000000", axes=FALSE)
  rect(xleft=-1, ybottom=55, xright=sunrise, ytop=130, col="#00000025", border=NA)
  rect(xleft=sunset, ybottom=55, xright=25, ytop=130, col="#00000025", border=NA)
  
  dev.off()
  
}

#########
# Check to see that the function works

#setwd("/Users/doug/Desktop/Example Data/Counter/")
#dataset<-read.csv("Scotland2014.csv")

#first.day<-220
#site<-"NotSure"
#year<-2200
#low.thresh<-40
#up.thresh<-127
#sunrise<-8
#sunset<-16

#pss.date.plot(dataset, first.day, site, year, low.thresh, up.thresh, sunrise, sunset)