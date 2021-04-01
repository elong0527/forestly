#' mk_reactable
#'
#' Define default behavior of reactable
#'
#' @param data data source
#' @param resizable make columns resizable by setting resizable to TRUE
#' @param filterable make columns filterable using filterable to TRUE
#' @param searchable make the entire table searchable using searchable
#' @param defaultPageSize The page size options be customized 10 through pageSizeOptions
#' @param borderless  customize table styling using Borderless
#' @param striped customize table styling using Stiped
#' @param highlight customize table styling using highlight
#' @param ...
#' @return an interactive data table with filtering,searching,pagination and custom table styling
#' using Bordered + striped + highlighting
#'
#'
#' @examples
#' mk_reacktable()
#'
#' @export
mk_reactable <- function(data,
                         resizable = TRUE,
                         filterable = TRUE,
                         searchable = TRUE,
                         defaultPageSize = 10,
                         borderless = TRUE,
                         striped = TRUE,
                         highlight = TRUE,
                         ...){

  reactable(data = data,
            resizable  = resizable,
            filterable = filterable,
            searchable = searchable,
            defaultPageSize = defaultPageSize,
            borderless = borderless,
            striped = striped,
            highlight = highlight,
            ...)

}

