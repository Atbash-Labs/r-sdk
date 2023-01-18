# source("../../R/sdk.R", chdir = TRUE) # this is here for local tests script(run_tests.sh) # nolint
context("sdk")


new_valid_buyer <- function() {
  api_key <- "buyer_key"
  ip_addr <- "127.0.0.1"
  new_buyer <- buyer$new(api_key, ip_addr, port = 8080, use_https = FALSE)
  return(new_buyer)
}

new_invalid_buyer <- function() {
  api_key <- "invalid_buyer_key"
  ip_addr <- "127.0.0.1"
  new_buyer <- buyer$new(api_key, ip_addr, port = 8080, use_https = FALSE)
  return(new_buyer)
}

test_that("test_get_key_list_success", {
  buyer <- new_valid_buyer()
  key_list <- buyer$get_key_list()

  expect_equal(length(key_list), 1)
})

test_that("test_key_list_added_key", {
  buyer <- new_valid_buyer()

  key_list <- buyer$get_key_list()
  expect_equal(length(key_list), 1)
  expect_equal(length(buyer$key_list), 1)

  stub <- buyer$get_key()

  key_list <- buyer$get_key_list()
  expect_equal(length(key_list), 2)
  expect_equal(length(buyer$key_list), 2)
})

test_that("test_invalid_buyer_for_key_list", {
  buyer <- new_invalid_buyer()
  key_list <- buyer$get_key_list()

  expect_equal(key_list$subkeys, NULL)
})

test_that("test_get_key_success", {
  buyer <- new_valid_buyer()
  new_key <- buyer$get_key()

  expect_false(length(new_key) == 0)
})

test_that("test_invalid_buyer_for_new_key", {
  buyer <- new_invalid_buyer()
  new_key <- buyer$get_key()

  expect_true(length(new_key$subkey) == 0)
})

test_that("test_query_success", {
  buyer <- new_valid_buyer()

  sql_query <- "select count(*) as numpeople from public.condition_era_death"
  output <- buyer$query(query = sql_query)

  expect_false(length(output$result) == 0)
  expect_false(length(output$accuracy) == 0)
})

test_that("test_query_history", {
  buyer <- new_valid_buyer()

  sql_query <- "select count(*) as numpeople from public.condition_era_death"
  stub <- buyer$query(query = sql_query)
  sql_query <- "select count(*) as numpeople from public.condition_era_death"
  stub <- buyer$query(query = sql_query)

  length <- nrow(buyer$all_queries)
  expect_equal(length, 2)

  sql_query <- "select count(*) as numpeople from public.condition_era_death"
  stub <- buyer$query(query = sql_query)

  length <- nrow(buyer$all_queries)
  expect_equal(length, 3)
})
