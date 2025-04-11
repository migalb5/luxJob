
test_that("DB connection is correctly established", {
  conn <- connect_db()
  expect_equal(as.character(class(conn)), "PqConnection")
  DBI::dbDisconnect(conn)
})

test_that("DB connection is not established", {
  withr::local_envvar(PG_DB = "xyz")
  withr::local_envvar(PG_HOST = "abc.abcabc")
  withr::local_envvar(PG_USER = "uuu")
  withr::local_envvar(PG_PASSWORD = "zzz")

  conn <- connect_db()
  expect_null(conn)
})
