% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mk_reactable.R
\name{mk_reactable}
\alias{mk_reactable}
\title{mk_reactable}
\usage{
mk_reactable(
  data,
  resizable = TRUE,
  filterable = TRUE,
  searchable = TRUE,
  defaultPageSize = 10,
  borderless = TRUE,
  striped = TRUE,
  highlight = TRUE,
  ...
)
}
\arguments{
\item{data}{A data frame or matrix to obtain variables.such as 'iris' data set.}

\item{resizable}{A character string to define the criteria to enable columns resizing.}

\item{filterable}{A character string to define the criteria to enable columns filtering.}

\item{searchable}{A character string to define the criteria to enable global table searching.}

\item{defaultPageSize}{A character string to define the Page size options for the table.}

\item{borderless}{A character string to remove inner borders from table.}

\item{striped}{A character string to add zebra-striping to table rows.}

\item{highlight}{A character string to highlight table rows on hover.}

\item{...}{Additional arguments transfered to reactable.}
}
\value{
an interactive data table.
}
\description{
mk_reactable
}
\details{
This function is used to create a data table from tabular data with sorting and pagination by default.
The data table is an HTML widget that can be used in R Markdown documents and Shiny applications, or viewed from an R console.
}
\examples{
\dontrun{
mk_reactable(iris[1:5, ], resizable = TRUE, filterable = TRUE, 
             searchable = TRUE, defaultPageSize = 10,
             borderless = TRUE, striped = TRUE, highlight = TRUE)
}              

}
