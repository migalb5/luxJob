
test_that("No or invalid argument provided -- returns data frame with details corresponding to book ID=1", {
  df1 <- get_book_by_id()
  expect_equal(as.character(class(df1)), "data.frame")
  expect_equal(nrow(df1), 1)

  df2 <- get_book_by_id("999999999999999999999999999999999999999")
  expect_equal(as.character(class(df2)), "data.frame")
  expect_equal(nrow(df2), 1)

  df3 <- get_book_by_id(FALSE)
  expect_equal(as.character(class(df3)), "data.frame")
  expect_equal(nrow(df3), 1)
})

test_that("Book not found -- returns NULL", {
  expect_null(get_book_by_id(99999999999999999999999999999999999999))
})

test_that("Valid/existing book ID provided -- returns a data frame with the book details", {
  df <- get_book_by_id(1)
  expect_equal(as.character(class(df)), "data.frame")
  expect_equal(nrow(df), 1)
})

