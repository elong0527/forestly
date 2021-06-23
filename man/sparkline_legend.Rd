% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparkline_util.R
\name{sparkline_legend}
\alias{sparkline_legend}
\title{sparkline_legend}
\usage{
sparkline_legend(p, title = "Treatment", pos = -1)
}
\arguments{
\item{p}{A parameter indicate which plot to add a tittle}

\item{tittle}{A string adding as the tittle of the plot}
}
\description{
This function is a internal function adding the title to the plot.
}
\examples{
p <- plot_ly(cars, x = ~speed, y = ~dist)
sparkline_legend <- function(p, title = "Treatment", pos = -1){
  plotly::layout(p,
                 showlegend = TRUE,
                 legend = list(
                   title = list(text = title),
                   orientation = "h",   # show entries horizontally
                   xanchor = "center",  # use center of legend as anchor
                   x = 0.5,
                   y = pos)
  )
}
sparkline_legend(p, title = "cars plot")
}