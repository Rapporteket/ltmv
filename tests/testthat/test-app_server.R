# store current instance and set temporary config
current_config_path = Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(
  system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH")
)

registry_name = "rapbase"


# rapbase modules are already tested. For now, just make sure the server runs
# by dummy class test of auto report list
test_that("server can run", {
  shiny::testServer(app = app_server, {
    expect_equal(class(report), "list")
  })
})

# test_that("example report can be provided for ui", {
#   shiny::testServer(app = app_server, {
#     expect_true("list" %in% class(output$ex_report))
#   })
# })

# test_that("example report can be provided for download", {
#   shiny::testServer(app = app_server, {
#     session$setInputs(format_report = "html")
#     session$setInputs(download_report = 1)
#     expect_true(file.exists(output$download_report))
#   })
# })


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = current_config_path)
