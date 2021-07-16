library(reactable)
library(forestly)
data(adsl)
data(adae)

test_that("mk_reactable() handles errors correctly about missing data ", {
  expect_error(mk_reactable())
})

test_that("mk_reactable() can generate a table as expected ", {

  # compare the mk_reactable result with reactable function with the related argument settings-1:
  expect_equal(mk_reactable(adsl), reactable(adsl,
                                             resizable = TRUE,
                                             filterable = TRUE,
                                             searchable = TRUE,
                                             defaultPageSize = 10,
                                             borderless = TRUE,
                                             striped = TRUE,
                                             highlight = TRUE))

  # compare the mk_reactable result with reactable function with the related argument settings-2:
  expect_equal(mk_reactable(adae, filterable = FALSE,defaultPageSize = 5), reactable(adae,
                                                                                     resizable = TRUE,
                                                                                     filterable = FALSE,
                                                                                     searchable = TRUE,
                                                                                     defaultPageSize = 5,
                                                                                     borderless = TRUE,
                                                                                     striped = TRUE,
                                                                                     highlight = TRUE))

  # compare the mk_reactable result with reactable function with the related argument settings-3:
  expect_equal(mk_reactable(adae, searchable = TRUE,highlight = FALSE), reactable(adae,
                                                                                  resizable = TRUE,
                                                                                  filterable = TRUE,
                                                                                  searchable = TRUE,
                                                                                  defaultPageSize = 10,
                                                                                  borderless = TRUE,
                                                                                  striped = TRUE,
                                                                                  highlight = FALSE))
})
