
test_that("No argument provided -- Data frame with skills is returned with the default size limit: 100 records (at most)", {
  expect_equal(as.character(class(get_skills())), "data.frame")
  expect_lte(nrow(get_skills()), 100)
})

test_that("Invalid argument provided -- Data frame with skills is returned with size limit 10", {
  expect_equal(as.character(class(get_skills("abc"))), "data.frame")
  expect_lte(nrow(get_skills("abc")), 10)
  expect_equal(as.character(class(get_skills(0))), "data.frame")
  expect_lte(nrow(get_skills(0)), 10)
})

test_that("Valid argument provided -- Data frame with skills is returned with specified size limit", {
  expect_equal(as.character(class(get_skills(1))), "data.frame")
  expect_lte(nrow(get_skills(1)), 1)
  expect_equal(as.character(class(get_skills(10000))), "data.frame")
  expect_lte(nrow(get_skills(10000)), 10000)
})
