#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param data This is the dataset used to create the histograms.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Histogram
#' @export

######################################################
#setup parameters for the functions
h<-10
w<-10

op <- list(mfrow=c(length(unique(d$channel)),1), 
        mar=c(4,3,3,1), 
        oma=c(2,2,1,0), 
        las=1, 
        xaxs="i", 
        yaxs="i")

######################################################

record.hist<-function(data, first.day, site, year){
  data$jday<-strptime(data$date, '%Y-%m-%d')$yday
  d1<-subset(data, jday>=first.day)
  d<-select(d1, channel, description, signal)
  
  fig.events<-paste(getwd(), site, year, "EventsbyChannel.pdf", sep="")
  pdf(fig.events, h=h, w=w)
  par(op)
  
  no.events<-ddply(filter(d, description=="E"), c("channel"), function(x){
    hist(x$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.events<-length(x$signal)
    data.frame(no.events)
  })
  
  mtext("Frequency of EVENT signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.events)
  
  name.up<-"UpsbyChannel.pdf"
  fig.up<-paste(getwd(), site.name, study.year, name.up, sep="")
  pdf(fig.up, h=h, w=w)
  par(op)
  
  no.up<-ddply(filter(d, description=="U"), c("channel"), function(x){
    hist(x$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.up<-length(x$signal)
    data.frame(no.up)
  })
  
  mtext("Frequency of UP signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.up)
  
  name.down<-"DownsbyChannel.pdf"
  fig.down<-paste(getwd(), site.name, study.year, name.down, sep="")
  pdf(fig.down, h=h, w=w)
  par(op)
  
  no.down<-ddply(filter(d, description=="D"), c("channel"), function(x){
    hist(x$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", x$channel[1], sep=""), col="grey60")
    no.down<-length(x$signal)
    data.frame(no.down)
  })
  
  mtext("Frequency of DOWN signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.down)
}