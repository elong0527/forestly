#' sparkline_errorbar
#' 
#' @examples 
#' 
#' est <- c(0,1,2)
#' lower <- c(-1, -2, -1)
#' upper <- c(3, 5, 5)
#' color <- c("red", "blue", "gold")
#' sparkline_errorbar(est, lower, upper, color)
#' 
#' @export
sparkline_errorbar <- function(est, 
                               lower, 
                               upper, 
                               color = NULL, 
                               height = 30,
                               xlim = NULL, 
                               vline = NULL, 
                               margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0), 
                               ...){
  
  db <- data.frame(est = est, 
                   lower = lower, 
                   upper = upper, 
                   mid = (lower + upper) / 2)
  
  db$sd <- db$upper - db$mid
  
  db$est1   <- formatC(db$est, digits = 2, format = "f")
  db$lower1 <- formatC(db$lower, digits = 2, format = "f")
  db$upper1 <- formatC(db$upper, digits = 2, format = "f")
  
  hover_text <- with(db, paste0(est1, " (",lower1, ", ", upper1, ")") )
  
  p <- plot_ly(data = db, 
               x = ~ est,
               y = ~ 1:nrow(db),
               color = color,
               colors = color,
               text = hover_text,
               hoverinfo = 'text',
               type = 'scatter',
               mode = 'markers',
               height = height)
  
  p <- p %>% add_trace(x = ~ mid, 
                       alpha = 0, 
                       text = hover_text,
                       hoverinfo = 'text',
                       error_x =  ~list(array = sd, color = color))
  
  p %>% sparkline_layout(xlim = xlim, 
                         vline = vline, 
                         margin = margin, 
                         ...)
  
}