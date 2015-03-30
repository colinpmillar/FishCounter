#' bind_signal_data
#' A function to bind and process Logie signal data
#' This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
#' @param path_to_folder This is the file path for the folder that contains all data files for processing.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param max_signal The maximum signal size.
#' @keywords Logie
#' @export

bind_signal_data<-function(path_to_folder, site, year, max_signal){
  library(plyr)
  library(dplyr)
  
  #"\\.txt$" tells r that the files are text files.  
  signal.paths <- dir(path_to_folder, pattern = "\\.txt$", full.names = TRUE)
  
  names(signal.paths) <- basename(signal.paths)
  
  if(missing(site)) {
    site <- ""
  }
  if(missing(year)) {
    year <- ""
  }
  
  signal.data1 <- ldply(signal.paths, 
                      read.table, 
                      header=FALSE, 
                      sep="", 
                      fill=TRUE, 
                      stringsAsFactors=FALSE)
  
  signal.data2 <- subset(signal.data1[, c(1:8)], V1=="S")[, -2]
  
  signal.data3 <- droplevels(signal.data2)
  
  colnames(signal.data3) <- c("file", 
                            "date", 
                            "time", 
                            "X", 
                            "channel", 
                            "description", 
                            "signal")
  
  signal.data4 <- data.frame("file"=signal.data3$file,
                           "date.time"=as.POSIXlt(strptime(paste(signal.data3$date, signal.data3$time, sep="-"), format='%d/%m/%y-%H:%M:%S')),
                           "date"=as.character(as.POSIXlt(strptime(signal.data3$date, format="%d/%m/%y"))),
                           "time"=as.character(signal.data3$time),
                           "X"=as.numeric(signal.data3$X),
                           "channel"=as.numeric(signal.data3$channel),
                           "description"=signal.data3$description,
                           "signal"=as.numeric(signal.data3$signal))
  
  signal.data5 <- signal.data4[!duplicated(signal.data4[, c(2, 6)]), ]
  
  signal.data <- subset(signal.data5, signal <= max_signal)
  
  signal.data <- signal.data[order(signal.data$time), ]
  
  #try changing the encoding when exporting. Look at what the encoding is when using a PC. Load the graphics file onto Jan's computer.
  write.csv(x=signal.data[, -2], 
            file=paste(path_to_folder, 
                       site, 
                       year,
                       ".csv", 
                       sep=""),
                       row.names=FALSE)
}