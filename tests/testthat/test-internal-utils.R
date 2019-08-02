context("internal-utils")

test_that("set_class",{
  x <- list()
  expect_identical(class(x), "list")
  x <- set_class(x, "new_class")
  expect_identical(class(x), "new_class")
})

test_that("is_try_error",{
  expect_false(is_try_error(1))
  expect_false(is_try_error(identity(1)))
  expect_true(is_try_error(try(stop(), silent = TRUE)))
})

test_that("sys_time",{
  expect_is(sys_time(), "POSIXct")
  expect_identical(attr(sys_time(), "tzone"), "UTC")
})

test_that("remove_nulls",{
  # vector
  expect_identical(remove_nulls(integer(0)), integer(0))
  expect_identical(remove_nulls(1), 1)
  expect_identical(remove_nulls(NA), NA)
  expect_identical(remove_nulls(c(2, NA)), c(2, NA))
  
  # list
  expect_identical(remove_nulls(list()), list())
  expect_identical(remove_nulls(list(1)), list(1))
  expect_identical(remove_nulls(list(NA)), list(NA))
  expect_identical(remove_nulls(list(2, NA)), list(2, NA))
  expect_identical(remove_nulls(list(2, NA, NULL)), list(2, NA))
  expect_identical(remove_nulls(list(NULL)), list())
  expect_identical(remove_nulls(list(x = 1, y = NULL)), list(x = 1))
})
