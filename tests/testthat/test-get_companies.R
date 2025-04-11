
test_that("No argument provided -- Data frame with companies is returned with the default size limit: 100 records (at most)", {
  expect_equal(as.character(class(get_companies())), "data.frame")
  expect_lte(nrow(get_companies()), 100)
})

test_that("Invalid argument provided -- Data frame with companies is returned with size limit 10", {
  expect_equal(as.character(class(get_companies("abc"))), "data.frame")
  expect_lte(nrow(get_companies("abc")), 10)
  expect_equal(as.character(class(get_companies(0))), "data.frame")
  expect_lte(nrow(get_companies(0)), 10)
})

test_that("Valid argument provided -- Data frame with companies is returned with specified size limit", {
  expect_equal(as.character(class(get_companies(1))), "data.frame")
  expect_lte(nrow(get_companies(1)), 1)
  expect_equal(as.character(class(get_companies(10000))), "data.frame")
  expect_lte(nrow(get_companies(10000)), 10000)
})
