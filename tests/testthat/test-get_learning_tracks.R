
test_that("Invalid or no skill argument provided, and learning tracks exist -- returns data frame with details of all learning tracks", {
  expect_equal(as.character(class(get_learning_tracks())), "data.frame")
  expect_gte(nrow(get_learning_tracks()), 1)
  expect_equal(as.character(class(get_learning_tracks(9999999999))), "data.frame")
  expect_gte(nrow(get_learning_tracks(9999999999)), 1)
})

test_that("Valid/existing skill argument provided, and learning tracks exist --  returns data frame with (only) learning tracks associated to skill ID provided", {
  expect_equal(as.character(class(get_learning_tracks("http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7"))), "data.frame")
  expect_gte(nrow(get_learning_tracks("http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7")), 1)
})

test_that("No learning track found (with or without valid skill argument) -- returns NULL", {
  expect_null(get_learning_tracks("999999999999999999999999999999999"))
  #expect_null(get_learning_tracks(999999999))
})
