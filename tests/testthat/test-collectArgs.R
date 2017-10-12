context("args_collected")

test_that("Args collected correctly", {
  test_func <- function(x, second_arg=1:5, third_arg=NULL, fourth_arg=c("hello world")) {
    cat("test_func_executed\n")

    dont_use_me_when_iterating <- "I should not have been used"
    # i_will_cause_failure <- "jkhjkhkjh"

    if (is.list(x)) {
      return(iterateWithArgs(x, FUNC=test_func, except="dont_use_me_when_iterating"))
    }

  return(x^2)
  }

  ## vectors
  expect_equal(test_func(1:4), c(1, 4, 9, 16))
  expect_output(test_func(1:4), "test_func_executed")

  ## list
  expect_equal(test_func(as.list(1:4)), list(1, 4, 9, 16))
  expect_output(test_func(as.list(1:4)), c("test_func_executed\ntest_func_executed\ntest_func_executed\ntest_func_executed\ntest_func_executed"))
})

test_that("Error Thrown Correctly", {
  test_func_will_error_only_on_list <- function(x, second_arg=1:5, third_arg=NULL, fourth_arg=c("hello world")) {
    cat("test_func_executed\n")

    dont_use_me_when_iterating <- "I should not have been used"
    i_will_cause_failure <- "jkhjkhkjh"

    if (is.list(x)) {
      return(iterateWithArgs(x, FUNC=test_func, except="dont_use_me_when_iterating"))
    }

  return(x^2)
  }

  ## vectors
  expect_equal(test_func_will_error_only_on_list(1:4), c(1, 4, 9, 16))
  expect_output(test_func_will_error_only_on_list(1:4), "test_func_executed")

  ## list
  expect_error(test_func_will_error_only_on_list(as.list(1:4)))
})

