library(httr)

#' Instantiate a new ``Buyer`` object
#'
#' @param api_key The ``API KEY`` from the website console
#' @param ip_addr The ``IP Address`` key from the website console
#' @param port [optional] The ``PORT`` from the website console
#' @return ``Buyer`` object to interact with the network
#' @examples
#' api_key <- "buyer_key"
#' ip_addr <- "127.0.0.1"
#' buyer <- new_buyer(api_key, ip_addr)
new_buyer <- function(api_key, ip_addr, port = "8080") {
  base_url <- paste("http://", ip_addr, ":", port, sep = "")

  inst <- list(
    "api_key" = api_key,
    "ip_addr" = ip_addr,
    "port" = port,
    "base_url" = base_url,
    "all_queries" = list(),
    "key_list" = list()
  )

  key_list <- get_key_list(inst)

  if (length(key_list$subkeys) == 0) {
    key <- get_key(inst)
    curr_list <- inst$key_list
    inst$key_list <- append(curr_list, list(key$subkey))
  } else {
    inst$key_list <- key_list
  }

  return(inst)
}

#' Returns a sub key for the passed in buyer
#'
#' @param buyer The ``Buyer`` object from the ``new_buyer`` function
#' @return the subkey to interact with the network
get_key <- function(buyer) {
  base_url <- as.character(buyer["base_url"])
  subkey_url <- paste(base_url, "/get_subkey", sep = "")
  payload <- list("buyer_api_key" = as.character(buyer["api_key"]))
  r <- GET(subkey_url, query = payload)
  rsp <- content(r, "parsed")

  if (status_code(r) != 200) {
    error <- as.character(rsp["message"])
    print(error)
  }

  subkey <- (rsp["subkey"])
  return(subkey)
}

#' Return the list of sub keys for the passed in buyer
#'
#' @param buyer The ``Buyer`` object from the ``new_buyer`` function
#' @return the list of sub keys
get_key_list <- function(buyer) {
  base_url <- as.character(buyer["base_url"])
  subkey_list_url <- paste(base_url, "/list_subkeys", sep = "")
  payload <- list("buyer_api_key" = as.character(buyer["api_key"]))
  r <- GET(subkey_list_url, query = payload)
  rsp <- content(r, "parsed")

  if (status_code(r) != 200) {
    error <- as.character(rsp["message"])
    print(error)
  }

  subkey_list <- (rsp["subkeys"])
  return(subkey_list)
}

#' Return the list of sub keys for the passed in buyer
#'
#' @param buyer The ``Buyer`` object from the ``new_buyer`` function
#' @param query_key [optional] subkey query key
#' @param query query string
#' @return returns ``result`` and ``accuracy`` of the query
#' #' @examples
#' sql_query <- "select count(*) as numpeople from public.condition_era_death"
#' result <- query(buyer, query = sql_query)
#' print_query_history(buyer)
query <- function(buyer, query_key = NULL, query) {
  if (missing(query_key)) {
    query_key <- buyer$key_list[[1]]
  }

  base_url <- as.character(buyer["base_url"])
  query_url <- paste(base_url, "/execute_query", sep = "")
  payload <- list(
    "buyer_api_key" = as.character(buyer["api_key"]),
    "subkey" = query_key, "query" = query
  )
  r <- GET(query_url, query = payload)
  rsp <- content(r, "parsed")

  if (status_code(r) != 200) {
    error <- as.character(rsp["message"])
    print(error)
  }

  output_result <- as.character(rsp["result"])
  output_accuracy <- as.character(rsp["accuracy"])
  output <- c(output_result, output_accuracy)
  curr_query <- c(query, output_result, output_accuracy)
  curr_history <- buyer$all_queries
  buyer$all_queries <<- append(curr_history, curr_query)
  return(output)
}

#' Return the table ddl columns
#'
#' @param buyer The ``Buyer`` object from the ``new_buyer`` function
#' @param query_key [optional] subkey query key
#' @return returns the list of columns
get_columns <- function(buyer, query_key) {
  base_url <- as.character(buyer["base_url"])
  query_url <- paste(base_url, "/list_columns", sep = "")
  payload <- list(
    "buyer_api_key" = as.character(buyer["api_key"]),
    "subkey" = query_key
  )
  r <- GET(query_url, query = payload)
  rsp <- content(r, "parsed")

  if (status_code(r) != 200) {
    error <- as.character(rsp["message"])
    print(error)
  }

  columns <- as.character(rsp["columns"])

  return(columns)
}

#' Pretty print the history queries with accuracy and results
#'
#' @param buyer The ``Buyer`` object from the ``new_buyer`` function
print_query_history <- function(buyer) {
  print("yoyo")
  print(buyer$all_queries)
}
