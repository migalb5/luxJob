
test_that("Invalid or no skill argument provided, and books exist -- returns data frame with details of all books", {
  expect_equal(as.character(class(get_books())), "data.frame")
  expect_gte(nrow(get_books()), 1)
  expect_equal(as.character(class(get_books(999999999999999999999999))), "data.frame")
  expect_gte(nrow(get_books(999999999999999999999999)), 1)
})

test_that("Valid/existing skill argument provided, and books exist --  returns data frame with (only) books associated to skill designation/label provided", {
  expect_equal(as.character(class(get_books("communication"))), "data.frame")
  expect_gte(nrow(get_books("communication")), 1)
})

test_that("No book found (with or without valid skill argument) -- returns NULL", {
  expect_null(get_books("999999999999999999999999999999999"))
  #expect_null(get_books(9999999999999999999))
})
