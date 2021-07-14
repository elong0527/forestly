library(testthat)
library(htmlwidgets)
library(plotly)
devtools::load_all()

ggsave_png<-function(code,width=4, hright=4){
  path<-tempfile(fileext = ".png")
  ggsave(path,plot=code)
  path
}

test_that("sparkline_errorbar",{
  est <- c(0,1,2)
  lower <- c(-1, -2, -1)
  upper <- c(3, 5, 5)
  color <- c("red", "blue", "gold")
  temp1<-sparkline_errorbar(est,lower,upper,color)

  expect_snapshot_file(ggsave_png(temp1$x),"sparkline_errorbar1.png")
})
