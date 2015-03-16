\name{counter.data.cleanup}
\alias{counter.data.cleanup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
counter.data.cleanup(path.to.folder, no.channels, site.name, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path.to.folder}{
%%     ~~Describe \code{path.to.folder} here~~
}
  \item{no.channels}{
%%     ~~Describe \code{no.channels} here~~
}
  \item{site.name}{
%%     ~~Describe \code{site.name} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (path.to.folder, no.channels, site.name, year) 
{
    library(plyr)
    counter.paths <- dir(path.to.folder, full.names = TRUE)
    names(counter.paths) <- basename(counter.paths)
    counter.data1 <- ldply(counter.paths, read.table, header = FALSE, 
        sep = "", fill = TRUE, stringsAsFactors = FALSE)[, c(1:7)]
    colnames(counter.data1) <- c("file", "date", "time", "X", 
        "channel", "description", "signal")
    counter.data2 <- subset(counter.data1, description == "U" | 
        description == "D" | description == "E")
    date.alt <- strptime(counter.data2$date, "\%d/\%m/\%y")
    counter.data2$jday <- date.alt$yday
    counter.data3 <- subset(counter.data2, jday != "NA")
    counter.data4 <- data.frame(file = counter.data3$file, date.time = as.character(as.POSIXlt(strptime(paste(counter.data3$date, 
        counter.data3$time, sep = "-"), format = "\%d/\%m/\%y-\%H:\%M:\%S"))), 
        date = as.character(as.POSIXlt(strptime(counter.data3$date, 
            format = "\%d/\%m/\%y"))), time = as.character(counter.data3$time), 
        X = as.numeric(counter.data3$X), channel = as.numeric(counter.data3$channel), 
        description = counter.data3$description, signal = as.numeric(counter.data3$signal))
    counter.data5 <- subset(counter.data4, channel < (no.channels + 
        1))
    counter.data6 <- counter.data5[!duplicated(counter.data5[, 
        c(2, 6)]), ]
    counter.data <- droplevels(counter.data6)
    counter.data <- counter.data[order(counter.data$date.time), 
        ]
    write.csv(x = counter.data[, -2], file = paste(path.to.folder, 
        site.name, year, ".csv", sep = ""), row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
