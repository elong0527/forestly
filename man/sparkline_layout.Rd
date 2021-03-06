% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparkline_util.R
\name{sparkline_layout}
\alias{sparkline_layout}
\title{sparkline_layout}
\usage{
sparkline_layout(
  p,
  xlim = NULL,
  vline = NULL,
  color = "#00000050",
  margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
  fixedrange = TRUE
)
}
\arguments{
\item{p}{A parameter indicate which plot to add color for scatters.}

\item{xlim}{A data frame define the range of x-axis on the plot}

\item{vline}{A data frame indicates where to draw the vertical lines on the plot}

\item{color}{A character string define the color of vertical line.}

\item{margin}{A data define the size of the plot}

\item{fixedrange}{Logical value of fixing figure range}
}
\description{
This is the function which adding color to vertical line.
}
\examples{

library(plotly)
p <- plot_ly(cars, x = ~speed, y = ~dist)
sparkline_layout(p, color='red')

}
