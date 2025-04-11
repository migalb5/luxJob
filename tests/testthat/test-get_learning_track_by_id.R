
test_that("No or invalid argument provided -- returns data corresponding to learning track ID=1 (list of 2 data frames: first, with the details of the learning track; second, with the skills associated to that learning track)", {
  df1 <- get_learning_track_by_id()
  expect_equal(as.character(class(df1)), "list")
  expect_equal(as.character(class(df1[[1]])), "data.frame")
  expect_equal(as.character(class(df1[[2]])), "data.frame")
  expect_equal(nrow(df1[[1]]), 1)

  df2 <- get_learning_track_by_id("999999999999999999999999999999999999999")
  expect_equal(as.character(class(df2)), "list")
  expect_equal(as.character(class(df2[[1]])), "data.frame")
  expect_equal(as.character(class(df2[[2]])), "data.frame")
  expect_equal(nrow(df2[[1]]), 1)

  df3 <- get_learning_track_by_id(FALSE)
  expect_equal(as.character(class(df3)), "list")
  expect_equal(as.character(class(df3[[1]])), "data.frame")
  expect_equal(as.character(class(df3[[2]])), "data.frame")
  expect_equal(nrow(df3[[1]]), 1)
})

test_that("Learning track not found -- returns NULL", {
  expect_null(get_learning_track_by_id(999999999))
})

test_that("Valid/existing learning track ID provided -- returns a list with 2 data frames: first, with the learning track details; second, with the skills required by / associated to that learning track", {
  df <- get_learning_track_by_id(1)
  expect_equal(as.character(class(df)), "list")
  expect_equal(as.character(class(df[[1]])), "data.frame")
  expect_equal(as.character(class(df[[2]])), "data.frame")
  expect_equal(nrow(df[[1]]), 1)
})

