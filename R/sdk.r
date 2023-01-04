library(httr)

new_buyer <- function(api_key, ip_addr, port = "8080") {
  base_url <- paste("http://", ip_addr, ":", port, sep = "")

  all_queries <- c()

  inst <- c(
    "api_key" = api_key,
    "ip_addr" = ip_addr, "port" = port, "base_url" = base_url,
    "all_queries" = all_queries
  )

  return(inst)
}

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

query <- function(buyer, query_key = NULL, query) {
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

  output_result <- rsp["result"]
  output_accuracy <- as.character(rsp["accuracy"])
  output <- c(output_result, output_accuracy)
  return(output)
}

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
