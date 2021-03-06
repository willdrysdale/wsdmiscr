% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_nested_colours.R
\name{create_nested_colours}
\alias{create_nested_colours}
\title{create_nested_colours}
\usage{
create_nested_colours(df, primary_group, secondary_group, primary_cols = NULL)
}
\arguments{
\item{df}{data.frame}

\item{primary_group}{name of column containting the primary group (character)}

\item{secondary_group}{name of column containing the secondary group (character)}

\item{primary_cols}{optional set the hues of the colours manually. must be as many hues as there are primary groups (character vector)}
}
\description{
Based on a dataframe with two grouping columns, return a named vector of colours for interactions of these groups.
The hues are spaced based on the primary_group, using \code{scales::hue_pal()}, and vary in saturation based
on the secondary_group, using \code{colortools::sequential()}
}
\author{
W. S. Drysdake
}
