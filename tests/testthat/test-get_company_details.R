
test_that("Valid argument provided -- returns list with: data frame with company details, and data frame with all vacancy details published by that company", {
  list1 <- get_company_details(2)
  expect_true(is.list(list1))
  expect_equal(as.character(class(list1[[1]])), "data.frame")
  expect_equal(as.character(class(list1[[2]])), "data.frame")
  expect_equal(nrow(list1[[1]]), 1)
  expect_gte(nrow(list1[[2]]), 0)
})

test_that("Invalid or no argument provided -- returns list with: data frame with company ID=1 details, and data frame with all vacancy details published by that company", {
  list2 <- get_company_details(1)
  expect_true(is.list(list2))
  expect_equal(as.character(class(list2[[1]])), "data.frame")
  expect_equal(as.character(class(list2[[2]])), "data.frame")
  expect_equal(nrow(list2[[1]]), 1)
  expect_gte(nrow(list2[[2]]), 0)
})

test_that("Not existing company_id provided -- returns NULL", {
  expect_null(get_company_details(99999999))
})
