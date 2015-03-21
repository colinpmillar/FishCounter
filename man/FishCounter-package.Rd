\name{FishCounter-package}
\alias{FishCounter-package}
\alias{FishCounter}
\docType{package}
\title{
FishCounter: Fish counter data summary and exploration
}
\description{
The FishCounter package is a set of tools that combine, summarize and explore fish counter data from the Logie resistivity coutner. It simplifies common tasks such as combining data from multiple downloads, exploring plots of peak signal sizes, and summarizes the number of events, ups and down counts graphically.
}
\details{
\tabular{ll}{
Package: \tab FishCounter\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-16\cr
License: \tab MIT \cr
}
SignalBind and CounterBind are functions that bind signal data or counter data, respecitively. Individual data files need to be placed in the same folder. They are imported, cleaned and then bound together to produce a single master dataset that can be used for plotting and summarizing data. HistPlot, EventsHourChPlot, PSSPlot, PSSHourPlot are used to explore errors and patterns in the data. Some of the functions also print out data summaries.
}
\author{
Douglas Braun
dbraun@instream.net
}
\references{
Fish Counter Network Final Report url:
}
\keyword{ package }
\seealso{
Aquantic website
InStream website
Other packages that might be built for other counter data - Vaki tweed example
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
None yet
}
