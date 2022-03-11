test_that("ui function returns a list object", {
  expect_true("shiny.tag.list" %in% class(app_ui()))
})
