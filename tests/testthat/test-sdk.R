source("../../R/sdk.R", chdir = TRUE)
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
