test_that("report processor provides files", {
  expect_true(
    file.exists(report_processor("guide", output_type = "html",
                                title = "Unit test")
    )
  )
})
