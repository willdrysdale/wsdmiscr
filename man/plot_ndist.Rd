% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ndist.R
\name{plot_ndist}
\alias{plot_ndist}
\title{Plot Normal distribution}
\usage{
plot_ndist(
  df,
  columns = c(CH1_Hz, CH2_Hz),
  value_name = "Values",
  binwidth = NULL,
  pad_res = NULL,
  floor_unit = NULL,
  print_stats = FALSE
)
}
\arguments{
\item{df}{data.frame containing a date column as POSIXct and the columns to plot}

\item{columns}{column names expressed unquoted.}

\item{value_name}{x axis label on histograms and y axis label on time series}

\item{binwidth}{set binwidth argument for geom_histogram}

\item{pad_res}{default NULL. When not NULL, pad the data using a time series at the this resolution in seconds}

\item{floor_unit}{default NULL. When not NULL use \code{lubridate::floor_date()} to round the time stamp to this unit}

\item{print_stats}{when TRUE, print the mean and sd to the console}
}
\description{
Fit and plot a normal distribution histogram alongside a timeseries of the data the distribution has been fitted from
}
\author{
W. S. Drysdale
}
