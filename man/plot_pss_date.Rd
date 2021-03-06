% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/PSSPlot.R
\name{plot_pss_date}
\alias{plot_pss_date}
\title{A function that plots the peak signal size (PSS) by date and time per hour of Logie counter data}
\usage{
plot_pss_date(dataset, day_one, site, year, low.thresh, up.thresh)
}
\arguments{
\item{dataset}{This is the dataset used to create the plots.}

\item{day_one}{This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.}

\item{site}{Name of the study river.}

\item{year}{Year of counter operation.}

\item{low_thresh}{is the counters lower threshold PSS value.}

\item{up_thresh}{is the counters upper threshold PSS value.}
}
\description{
This function plots the PSS by day and time for Logie counter data
}
\keyword{Events}

