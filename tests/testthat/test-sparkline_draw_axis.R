plotly_snap <- function(p){
  p_js <- plotly::plotly_json(p, jsonedit = FALSE)
  p_lst <- jsonlite::fromJSON(p_js)
  
  gsub(names(p_lst$visdat), "0000", as.character(p_lst))
  
}

test_that("sparkline_deaw_axis",{
  local_edition(3) # avoid Error: `expect_snapshot_output()` requires the 3rd edition.
  color <- c("red", "blue")
  label <- c("MK9999", "Placebo")
  xlim  <- c(-2, 2)
  xlab  <- "Example"
  temp1<-sparkline_draw_axis(color, label, xlim,showlegend = FALSE, height = 30)
  
  expect_snapshot_output(plotly_snap(temp1))
})
