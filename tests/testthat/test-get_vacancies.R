
test_that("No or invalid argument skill provided (including no arguments provided at all) -- returns data frame with vacancies filtered by conjunction of eventual other criteria provided", {
  df1 <- get_vacancies(skill = 99999999999999)
  expect_equal(as.character(class(df1)), "data.frame")
  expect_lte(nrow(df1), 100)

  df2 <- get_vacancies(company = "Company_1", canton = "Luxembourg", limit = 20)
  expect_equal(as.character(class(df2)), "data.frame")
  expect_lte(nrow(df2), 20)

  df3 <- get_vacancies()
  expect_equal(as.character(class(df3)), "data.frame")
  expect_lte(nrow(df3), 100)

})

test_that("No or invalid argument company provided -- returns data frame with vacancies filtered by conjunction of eventual other criteria provided", {
  df1 <- get_vacancies(company = 9999999999999999)
  expect_equal(as.character(class(df1)), "data.frame")
  expect_lte(nrow(df1), 100)

  df2 <- get_vacancies(skill = "communication", canton = "Luxembourg", limit = 20)
  expect_equal(as.character(class(df2)), "data.frame")
  expect_lte(nrow(df2), 20)
})

test_that("No or invalid argument canton provided -- returns data frame with vacancies filtered by conjunction of eventual other criteria provided", {
  df1 <- get_vacancies(canton = 999999999999)
  expect_equal(as.character(class(df1)), "data.frame")
  expect_lte(nrow(df1), 100)

  df2 <- get_vacancies(skill = "communication", company = "Company_1", limit = 20)
  expect_equal(as.character(class(df2)), "data.frame")
  expect_lte(nrow(df2), 20)
})

test_that("No or invalid argument limit provided -- returns data frame with vacancies up to a maximum size of 100 (default)", {
  df1 <- get_vacancies(limit = 0)
  expect_equal(as.character(class(df1)), "data.frame")
  expect_lte(nrow(df1), 100)

  df2 <- get_vacancies(skill = "communication", company = "Company_1", canton = "Luxembourg")
  expect_equal(as.character(class(df2)), "data.frame")
  expect_lte(nrow(df2), 100)
})

test_that("All arguments provided are valid -- returns data frame with vacancies filtered by conjunction of criteria provided", {
# improvement: from atop and also for other functions: consider retrieving available valid argument values for testing from DB, inside the tests themselves

  df1 <- get_vacancies(skill = "communication", company = "Company_1", canton = "Luxembourg", limit = 3)
  expect_equal(as.character(class(df1)), "data.frame")
  expect_lte(nrow(df1), 3)

  df2 <- get_vacancies(skill = "English", company = "Company_2", canton = "Clervaux", limit = 8)
  expect_equal(as.character(class(df2)), "data.frame")
  expect_lte(nrow(df2), 8)

  df3 <- get_vacancies(skill = "German", company = "Company_3", canton = "Remich", limit = 10000)
  expect_equal(as.character(class(df3)), "data.frame")
  expect_lte(nrow(df3), 10000)
})

test_that("No vacancies found matching the valid criteria provided -- returns NULL", {
  expect_null(get_vacancies(skill = "abcabc"))
  expect_null(get_vacancies(company = "99999999999999999999999999999999999999"))
  expect_null(get_vacancies(canton = "9999999999999999999999999999999"))
  expect_null(get_vacancies(skill = "abcabc", company = "99999999999999999999999999999999999999"))
  expect_null(get_vacancies(skill = "abcabc", canton = "9999999999999999999999999999999"))
  expect_null(get_vacancies(company = "99999999999999999999999999999999999999", canton = "9999999999999999999999999999999"))
  expect_null(get_vacancies(skill = "abcabc", company = "99999999999999999999999999999999999999", limit = 12))
})
