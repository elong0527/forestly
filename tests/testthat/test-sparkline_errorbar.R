library(testthat)
library(htmlwidgets)
library(plotly)
devtools::load_all()

plotly_snap <- function(p){
  p_js <- plotly::plotly_json(p, jsonedit = FALSE)
  p_lst <- jsonlite::fromJSON(p_js)

  gsub(names(p_lst$visdat), "0000", as.character(p_lst))

}

test_that("sparkline_errorbar",{
  local_edition(3) # avoid Error: `expect_snapshot_output()` requires the 3rd edition.
  est <- c(0,1,2)
  lower <- c(-1, -2, -1)
  upper <- c(3, 5, 5)
  color <- c("red", "blue", "gold")
  temp1<-sparkline_errorbar(est,lower,upper,color)

  expect_snapshot_output(plotly_snap(temp1))
})
