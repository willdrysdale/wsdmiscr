% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_faam_mixing_ratio_to_mgm3.R
\name{faam_mixing_ratio_to_mgm3}
\alias{faam_mixing_ratio_to_mgm3}
\title{faam_mixing_ratio_to_mgm3}
\usage{
faam_mixing_ratio_to_mgm3(df, unit, pollutant, pollutant_mass)
}
\arguments{
\item{df}{faam merge file.csv}

\item{unit}{current unit of mixing ratio. supports "ppm", "ppb", "ppt" (trillion not thousand)}

\item{pollutant}{column header of pollutant to be converted}

\item{pollutant_mass}{pollutant mass in mg/mol}
}
\value{
column of converted data
}
\description{
Take a faam merge file and converts a species as a mixing ratio to mg/m3
}
\author{
W S. Drysdale

Beth Nelson
}
