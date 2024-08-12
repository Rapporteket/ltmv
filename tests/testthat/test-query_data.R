# For these tests to work locally make sure an instance of mysql server is
# available and that the necessary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# When run at Github Actions build servers [USER] must be set to 'actions' and
# with an empty password (as also assumed in the above localhost example).
# See also .github/workflows/R-CMD-check.yml

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db = function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# preserve initial state
config_path = Sys.getenv("R_RAP_CONFIG_PATH")


test_that("env vars needed for testing is present", {
  check_db()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con = RMariaDB::dbConnect(RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testDb;")
  RMariaDB::dbDisconnect(con)
}

# make temporary config
test_config = paste0(
  "testReg:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : testDb",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)
Sys.setenv(R_RAP_CONFIG_PATH = tempdir()) # nolint: undesirable_function_linter.
cf = file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)
file.copy(
  from = system.file("rapbaseConfig.yml", package = "rapbase"),
  to = file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")
)

# make queries for creating tables
fc = file(system.file("test_db.sql", package = "ltmv"), "r")
t = readLines(fc)
close(fc)
sql = paste0(t, collapse = "\n")
queries = strsplit(sql, ";", fixed = TRUE)[[1]]

registry_name = "testReg"

test_that("relevant test database and tables can be made", {
  check_db()
  con = rapbase::rapOpenDbConnection(registry_name)$con
  for (i in seq_len(length(queries))) {
    expect_type(RMariaDB::dbExecute(con, queries[i]), "integer")
  }
  rapbase::rapCloseDbConnection(con)
})

# onto main testing
session = list()
attr(session, "class") = "ShinySession"
resh_id = 999999
## simply check if data frames are returned

# ADD TEST ON SQL FUNCTIONS HERE
## simply check if data frames are returned
test_that("data frame is returned", {
  check_db()
  expect_s3_class(
    object = query_all_hospitals(registry_name, resh_id, session = session),
    class = "data.frame"
  )
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con = rapbase::rapOpenDbConnection(registry_name)$con
  RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path) # nolint: undesirable_function_linter.
