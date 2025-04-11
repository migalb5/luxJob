
test_that("No or invalid argument provided -- returns NULL", {
  expect_null(get_vacancy_by_id())
  expect_null(get_vacancy_by_id(FALSE))
  expect_null(get_vacancy_by_id("9999999999999999999999999999999"))
})

test_that("Vacancy not found -- returns NULL", {
  expect_null(get_vacancy_by_id(99999999999999999999))
})

test_that("Valid/existing vacancy ID provided -- returns a list with 2 data frames: first, with the vacancy details; second, with the skills required by / associated to that vacancy", {
  list_res <- get_vacancy_by_id(967488291)
  expect_equal(as.character(class(list_res)), "list")
  expect_equal(as.character(class(list_res[[1]])), "data.frame")
  expect_equal(as.character(class(list_res[[2]])), "data.frame")
  expect_equal(nrow(list_res[[1]]), 1)
  expect_gte(nrow(list_res[[2]]), 0)
})

