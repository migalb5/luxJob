
test_that("Invalid user ID argument provided -- returns FALSE", {
  expect_false(log_search(user_id = "9999999999999999999999999", query = "machine learning"))
  expect_false(log_search(user_id = FALSE, query = "machine learning"))
})

test_that("User (ID) not found -- returns FALSE", {
  expect_false(log_search(user_id = 99999999999999999999999999999999999999, query = "machine learning"))
  expect_false(log_search(user_id = 99999999999999999999999999999999999999, query = "")) # test that the logging of an empty query is also OK
})

test_that("Invalid query argument -- returns FALSE", {
  expect_false(log_search(user_id = 1, query = 9999999999999999999999999999999))
  expect_false(log_search(user_id = 1, query = FALSE))
})

test_that("Valid/existing user ID and query string provided -- returns TRUE", {
  expect_true(log_search(user_id = 1, query = "computer programming")) # check how to simulate that user_id = 1 exists in DB
  # alternatively, consider removing record(s) created on DB during test execution
})

