# source("../../R/sdk.R", chdir = TRUE) # this is here for local tests
library(testthat)


new_valid_buyer <- function() {
  api_key <- "buyer_key"
  ip_addr <- "127.0.0.1"
  buyer <- new_buyer(api_key, ip_addr)
  return(buyer)
}

new_invalid_buyer <- function() {
  api_key <- "invalid_buyer_key"
  ip_addr <- "127.0.0.1"
  buyer <- new_buyer(api_key, ip_addr)
  return(buyer)
}

test_that("test_get_key_list_success", {
  buyer <- new_valid_buyer()
  key_list <- get_key_list(buyer)

  expect_equal(length(key_list$subkeys), 1)
})

test_that("test_key_list_added_key", {
  buyer <- new_valid_buyer()

  key_list <- get_key_list(buyer)
  expect_equal(length(key_list$subkeys), 1)

  stub <- get_key(buyer)

  key_list <- get_key_list(buyer)
  expect_equal(length(key_list$subkeys), 2)
})

test_that("test_invalid_buyer_for_key_list", {
  buyer <- new_invalid_buyer()
  key_list <- get_key_list(buyer)

  expect_equal(key_list$subkeys, NULL)
})

test_that("test_get_key_success", {
  buyer <- new_valid_buyer()
  new_key <- get_key(buyer)

  expect_false(length(new_key$subkey) == 0)
})

test_that("test_invalid_buyer_for_new_key", {
  buyer <- new_invalid_buyer()
  new_key <- get_key(buyer)

  expect_true(length(new_key$subkey) == 0)
})

test_that("test_query_success", {
  # buyer <- new_valid_buyer()
  # sql_query <- "select count(*) as numpeople from public.condition_era_death"
  # result <- buyer.query(query = sql_query)

  # expect_false(length(result) == 0)
  # expect_false(length(accuracy) == 0)
})


test_that("test_query_history", {

})
