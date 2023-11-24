test_that("report processor provides files", {
  expect_true(
    file.exists(report_processor("guide",
      output_type = "html",
      title = "Unit test"
    ))
  )
  expect_true(
    file.exists(report_processor("sample_report",
      output_type = "html",
      title = "Unit test"
    ))
  )
})

test_that("a warning is provided when no report title given", {
  expect_warning(report_processor("guide", output_type = "html"))
})
