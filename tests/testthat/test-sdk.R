source("../../R/sdk.R", chdir = TRUE)
library(testthat)

test_that("check", {
  expect_equal(hello(), "Hello, world!")
})
