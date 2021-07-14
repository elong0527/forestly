#' Sparkline Point Figure Javascript Render Using Plotly
#'
#' @param tbl A data frame
#' @param x A vector of variable names in `tbl` for value.
#' @param x_lower A vector of variable names in `tbl` for abs(lower error difference) = risk difference - lower bound.
#' @param x_upper A vector of variable names in `tbl` for abs(upper error difference) = upper bound - risk difference. 
#'                Default is the same as `x_lower`.
#' @param xlim Numeric vectors of length 2, giving the x coordinates ranges.
#' @param y Numeric vector of y-axis value.
#' @param vline Numeric value for the location to draw the vertical reference line.
#' @param text Character vector of text information displayed for each point.
#' @param height Numeric value of figure height.
#' @param width Numeric value of figure width.
#' @param color Character vector of point color name
#' @param color_errorbar Character vector of error bar color name
#' @param color_vline Character vector of vertical reference line color name
#' @examples
#' \dontrun{
#'  
#' library(reactable)
#' library(htmltools)
#' 
#' -------------------------------------
#' Examples to generate two points 
#' -------------------------------------
#' js <- sparkline_point_js(iris, "Sepal.Length")
#' js <- sparkline_point_js(iris, "Sepal.Length", text = "x")
#' js <- sparkline_point_js(iris, "Sepal.Length", text = "'count:' + x")
#' js <- sparkline_point_js(iris, "Sepal.Length", vline = 6)
#' 
#' js <- sparkline_point_js(iris, 
#'                          x = c("Sepal.Length", "Sepal.Width"), 
#'                          x_lower = "Sepal.Width",
#'                          y = c(1,2),
#'                          text = c("x[0]", "x[1]"),
#'                          color = c("gold", "purple") )
#' cat(js)
#' 
#' col <- colDef(html = TRUE, cell = JS(js), width = 150,
#'               style="font-size: 0px; padding: 0px; margin: 0px;")
#' p <- reactable(data = iris[,1:2],
#'                theme = reactableTheme(cellPadding = "0px 8px"),
#'                borderless = TRUE,
#'                highlight = TRUE,
#'                resizable = TRUE,
#'                columns = list(Sepal.Length = col))
#'                
#' plotly_js <- "https://unpkg.com/react-plotly.js@1.0.2/dist/create-plotly-component.js"
#' browsable(tagList(
#'   reactR::html_dependency_react(),
#'   htmltools::tags$script(src="https://cdn.plot.ly/plotly-latest.min.js"),
#'   htmltools::tags$script(src=plotly_js),
#'   p
#' ))
#' 
#' }
#' 
#' -------------------------------------
#' Example to generate the error bar
#' -------------------------------------
#' xx = seq(1,10)
#' test_dataframe = data.frame(my_x = xx, 
#'                             my_lower = seq(0.5, 5, 0.5),
#'                             my_upper = seq(5, 0.5, -0.5))
#' js <- sparkline_point_js(tbl = test_dataframe, 
#'                          x = "my_x", 
#'                          x_lower = "my_lower", x_upper = "my_upper",
#'                          xlim = round(range(c(test_dataframe$my_x - test_dataframe$my_lower, test_dataframe$my_x - test_dataframe$my_upper)) + c(-1, 1)))
#' 
#' col <- colDef(html = TRUE, cell = JS(js), width = 200,
#'               style="font-size: 0px; padding: 0px; margin: 0px;")
#' p <- reactable(data = test_dataframe,
#'                theme = reactableTheme(cellPadding = "0px 8px"),
#'                borderless = TRUE,
#'                highlight = TRUE,
#'                resizable = TRUE,
#'                columns = list(my_x = col))
#' 
#' plotly_js <- "https://unpkg.com/react-plotly.js@1.0.2/dist/create-plotly-component.js"
#' browsable(tagList(
#'   reactR::html_dependency_react(),
#'   htmltools::tags$script(src="https://cdn.plot.ly/plotly-latest.min.js"),
#'   htmltools::tags$script(src=plotly_js),
#'   p
#' ))
#' 
#' 
#' 
#' @export
sparkline_point_js2 <- function(tbl,
                               x,
                               x_lower = NULL,
                               x_upper = x_lower,
                               xlim = NULL,
                               y = seq_along(x),
                               vline = NULL,
                               text = NULL,
                               height = 30,
                               width = 150,
                               color = "gold",
                               color_errorbar = color,
                               color_vline = "#00000050"){

  # Input Checking
  stopifnot(x %in% names(tbl))

  if(! is.null(x_lower)){
    stopifnot(x_lower %in% names(tbl))
    stopifnot(x_upper %in% names(tbl))
  }

  stopifnot(length(xlim) %in% c(0, 2))

  stopifnot(length(vline) <= 1)
  stopifnot(length(height) == 1)
  stopifnot(length(width) == 1)

  stopifnot(length(color) %in% c(1, length(x)) )
  stopifnot(length(color_errorbar) %in% c(1, length(x)) )
  stopifnot(length(color_vline) %in% 1 )

  if(is.null(text)){
    text <- '""'
  }
  
  # Vectorize input 
  meta <- data.frame(x, y, text, color, color_errorbar)
  
  if(! is.null(x_lower)){
    meta$x_lower <- x_lower 
    meta$x_upper <- x_upper 
  }
  
  x <- meta$x
  x_lower <- meta$x_lower
  x_upper <- meta$x_upper
  y <- meta$y
  text <- meta$text
  color <- meta$color
  color_errorbar <- meta$color_errorbar
  
  # Convert x and error bar
  x_v <- tbl[, x]
  js_x <- paste(paste0("cell.row['", x, "']"), collapse = ",")

  if(is.null(x_lower)){
    js_x_lower <- 0
    js_x_upper <- 0
    x_l <- x_v
    x_u <- x_v
    color_errorbar <- "#FFFFFF00"
  }else{
    x_l <- x_v - tbl[,x_lower]
    x_u <- x_v + tbl[,x_upper]
    js_x_lower <- paste(paste0('cell.row["', x_lower, '"]'), collapse = ", ")
    js_x_upper <- paste(paste0('cell.row["', x_upper, '"]'), collapse = ", ")
  }

  # Convert y
  js_y <- paste(y, collapse = ", ")

  # Convert axis range
  if(is.null(xlim)){
    xlim <- range(c(x_v, x_l, x_u)) + c(-0.5, 0.5)
  }
  js_x_range <- paste(xlim, collapse = ", ")
  js_y_range <- paste(c(0, length(x) + 1), collapse = ", ")

  # Convert text
  if(is.null(text)){
    js_text <- '""'
  }else{
    js_text <- paste(text, collapse = ", ")
  }

  # Convert v_line
  if(is.null(vline)) vline <- "[]"
  js_vline <- as.character(vline)

  # Convert shape
  js_height <- as.character(height)
  js_width <- as.character(width)

  # Convert color
  foo <- function(x){
    rgba <- grDevices::col2rgb(x, alpha = TRUE)
    rgba[4, ] <- rgba[4, ] / 255
    paste( paste0('"rgba(', apply( rgba, 2, paste, collapse = ", " ), ')"'), collapse = ", ")
  }
  js_color <- foo(color)
  js_color_errorbar <- foo(color_errorbar)
  js_color_vline <- foo(color_vline)

  # data trace template
  data_trace_js <- function(n){
    
    template <- '
  {
    "x": [x[i]],
    "y": [y[i]],
   "error_x": {
      type: "data",
      symmetric: false,
      array: [x_upper[i]],
      arrayminus: [x_lower[i]],
      "color": color_errorbar[i]
    },
    "text": text[i],
    "hoverinfo": "text",
    "mode": "markers",
    "alpha_stroke": 1,
    "sizes": [10, 100],
    "spans": [1, 20],
    "type": "scatter",
    "marker": {
      "color": [color[i]]
    }
  }'
    
    js <- lapply(1:n - 1, function(x) gsub("[i]", paste0("[",x,"]"), template, fixed = TRUE) )
    js <- paste(js, collapse = ",")
    
  }
  
  data_trace <- data_trace_js(length(x))
  # Brew

  plotly_file <- tempfile(fileext = ".js")
  brew::brew(system.file("javascripts/sparkline.js", package = "forestly"), output = plotly_file)

  js <- paste(readLines(plotly_file), collapse = "\n")

  js

}

