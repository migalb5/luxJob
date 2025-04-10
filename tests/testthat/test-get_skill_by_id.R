
test_that("No argument provided -- Data frame with (default) Communication skill details is returned", {
  df <- get_skill_by_id()
  expect_equal(as.character(class(df)), "data.frame")
  expect_equal(nrow(df), 1)
  expect_equal(df$skill_id[1], "http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7")
})

test_that("Valid argument provided -- Data frame with corresponding skill details is returned", {
  df <- get_skill_by_id("http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7")
  expect_equal(as.character(class(df)), "data.frame")
  expect_equal(nrow(df), 1)
  expect_equal(df$skill_id[1], "http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7")
})

test_that("Invalid argument provided -- Data frame with (default) Communication skill details is returned", {
  df <- get_skill_by_id(4.56)
  expect_equal(as.character(class(df)), "data.frame")
  expect_equal(nrow(df), 1)
  expect_equal(df$skill_id[1], "http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7")
})

test_that("Provided skill ID is not found -- NULL is returned", {
  expect_null(get_skill_by_id("abcabc"))
})
