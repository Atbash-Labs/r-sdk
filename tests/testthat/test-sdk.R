# source("../../R/sdk.R", chdir = TRUE) # this is here for local tests script(run_tests.sh) # nolint
context("sdk")


new_valid_buyer <- function() {
  api_key <- "buyer_key"
  ip_addr <- "127.0.0.1"
  new_buyer <- buyer$new(api_key, ip_addr,port = 8080, use_https = FALSE)
  return(new_buyer)
}

new_invalid_buyer <- function() {
  api_key <- "invalid_buyer_key"
  ip_addr <- "127.0.0.1"
  new_buyer <- buyer$new(api_key, ip_addr,port = 8080, use_https = FALSE)
  return(new_buyer)
}

test_that("test_get_key_list_success", {
  buyer <- new_valid_buyer()
  key_list <- buyer$get_key_list()

  expect_equal(length(key_list$subkeys), 1)
})

test_that("test_key_list_added_key", {
  buyer <- new_valid_buyer()

  key_list <- buyer$get_key_list()
  expect_equal(length(key_list$subkeys), 1)

  stub <- buyer$get_key()

  key_list <- buyer$get_key_list()
  expect_equal(length(key_list$subkeys), 2)
})

test_that("test_invalid_buyer_for_key_list", {
  buyer <- new_invalid_buyer()
  key_list <- buyer$get_key_list()

  expect_equal(key_list$subkeys, NULL)
})

test_that("test_get_key_success", {
  buyer <- new_valid_buyer()
  new_key <- buyer$get_key()

  expect_false(length(new_key$subkey) == 0)
})

test_that("test_invalid_buyer_for_new_key", {
  buyer <- new_invalid_buyer()
  new_key <- buyer$get_key()

  expect_true(length(new_key$subkey) == 0)
})
