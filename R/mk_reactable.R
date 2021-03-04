#' mk_reactable
#'
#' @examples
#'
#' mk_reactable(iris)
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
#try to add new code
c<-1+1
c
