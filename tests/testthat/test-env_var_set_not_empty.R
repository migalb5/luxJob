
test_that("Environment variable checking is successful", {
  withr::local_envvar(VAR_1 = "xyz")

  expect_true(env_var_set_not_empty("VAR_1"))
})

test_that("Environment variable checking is NOT successful", {
  withr::local_envvar(VAR_2 = "")

  expect_false(env_var_set_not_empty("VAR_2"))
  expect_false(env_var_set_not_empty("VAR_3"))
})
