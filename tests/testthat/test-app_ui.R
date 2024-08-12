test_that("ui function returns a list object", {
  expect_s3_class(app_ui(), "shiny.tag.list")
})
