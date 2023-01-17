library(httr)
library(jsonlite)

# #' Instantiate a new ``Buyer`` object
# #'
# #' @param api_key The ``API KEY`` from the website console
# #' @param ip_addr The ``IP Address`` key from the website console
# #' @param port [optional] The ``PORT`` from the website console
# #' @return ``Buyer`` object to interact with the network
# #' @examples
# #' api_key <- "buyer_key"
# #' ip_addr <- "127.0.0.1"
# #' buyer <- new_buyer(api_key, ip_addr)
# #'
# #' @export
# new_buyer <- function(api_key, ip_addr, port = "8080") {
#   base_url <- paste("http://", ip_addr, ":", port, sep = "")

#   columns <- c("query", "result", "accuracy")
#   df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
#   colnames(df) <- columns

#   inst <- list(
#     "api_key" = api_key,
#     "ip_addr" = ip_addr,
#     "port" = port,
#     "base_url" = base_url,
#     "all_queries" = df,
#     "key_list" = list()
#   )

#   key_list <- get_key_list(inst)

#   if (length(key_list$subkeys) == 0) {
#     key <- get_key(inst)
#     curr_list <- inst$key_list
#     inst$key_list <- append(curr_list, list(key$subkey))
#   } else {
#     inst$key_list <- key_list
#   }

#   return(inst)
# }

# #' Returns a sub key for the passed in buyer
# #'
# #' @param buyer The ``Buyer`` object from the ``new_buyer`` function
# #' @return the subkey to interact with the network
# #'
# #' @export
# get_key <- function(buyer) {
#   base_url <- as.character(buyer["base_url"])
#   subkey_url <- paste(base_url, "/get_subkey", sep = "")
#   payload <- list("buyer_api_key" = as.character(buyer["api_key"]))
#   r <- GET(subkey_url, query = payload)
#   rsp <- content(r, "parsed")

#   if (status_code(r) != 200) {
#     error <- as.character(rsp["message"])
#     print(error)
#   }

#   subkey <- (rsp["subkey"])
#   return(subkey)
# }

# #' Return the list of sub keys for the passed in buyer
# #'
# #' @param buyer The ``Buyer`` object from the ``new_buyer`` function
# #' @return the list of sub keys
# #'
# #' @export
# get_key_list <- function(buyer) {
#   base_url <- as.character(buyer["base_url"])
#   subkey_list_url <- paste(base_url, "/list_subkeys", sep = "")
#   payload <- list("buyer_api_key" = as.character(buyer["api_key"]))
#   r <- GET(subkey_list_url, query = payload)
#   rsp <- content(r, "parsed")

#   if (status_code(r) != 200) {
#     error <- as.character(rsp["message"])
#     print(error)
#   }

#   subkey_list <- (rsp["subkeys"])
#   return(subkey_list)
# }

# #' Return the list of sub keys for the passed in buyer
# #'
# #' @param buyer The ``Buyer`` object from the ``new_buyer`` function
# #' @param query_key [optional] subkey query key
# #' @param query query string
# #' @return returns ``result`` and ``accuracy`` of the query
# #' #' @examples
# #' sql_query <- "select count(*) as numpeople from public.condition_era_death"
# #' result <- query(buyer, query = sql_query)
# #' print_query_history(buyer)
# #'
# #' @export
# query <- function(buyer, query_key = NULL, query) {
#   if (missing(query_key)) {
#     query_key <- buyer$key_list[[1]]
#   }

#   base_url <- as.character(buyer["base_url"])
#   query_url <- paste(base_url, "/execute_query", sep = "")
#   payload <- list(
#     "buyer_api_key" = as.character(buyer["api_key"]),
#     "subkey" = query_key, "query" = query
#   )
#   r <- GET(query_url, query = payload)
#   rsp <- content(r, "parsed")

#   if (status_code(r) != 200) {
#     error <- as.character(rsp["message"])
#     print(error)
#   }

#   output_result <- as.character(rsp["result"])
#   output_accuracy <- as.character(rsp["accuracy"])
#   output <- c(output_result, output_accuracy)
#   curr_history <- buyer$all_queries
#   new_row <- data.frame(
#     query = query, result = output_result,
#     accuracy = output_accuracy
#   )
#   new_history <- rbind(curr_history, new_row)
#   buyer$all_queries <<- new_history
#   return(output)
# }

# #' Return the table ddl columns
# #'
# #' @param buyer The ``Buyer`` object from the ``new_buyer`` function
# #' @param query_key [optional] subkey query key
# #' @return returns the list of columns
# #'
# #' @export
# get_columns <- function(buyer, query_key = NULL) {
#   if (missing(query_key)) {
#     query_key <- buyer$key_list[[1]]
#   }

#   base_url <- as.character(buyer["base_url"])
#   query_url <- paste(base_url, "/list_columns", sep = "")
#   payload <- list(
#     "buyer_api_key" = as.character(buyer["api_key"]),
#     "subkey" = query_key
#   )
#   r <- GET(query_url, query = payload)
#   rsp <- content(r, "parsed")

#   if (status_code(r) != 200) {
#     error <- as.character(rsp["message"])
#     print(error)
#   }

#   columns <- rsp["columns"]

#   return(columns)
# }

# #' Pretty print the history queries with accuracy and results
# #'
# #' @param buyer The ``Buyer`` object from the ``new_buyer`` function
# #'
# #' @export
# print_query_history <- function(buyer) {
#   table_frame <- paste(replicate(90, "-"), collapse = "")
#   table_frame <- paste("+", table_frame, "+", sep = "")

#   len <- nrow(buyer$all_queries)

#   for (i in 1:len) {
#     print(table_frame)
#     print(buyer$all_queries[["query"]][[i]])
#     print(buyer$all_queries[["result"]][[i]])
#     print(buyer$all_queries[["accuracy"]][[i]])
#   }
# }




#' @export
buyer <- setRefClass("buyer",
  fields = list(
    api_key = "character",
    query_count = "numeric",
    all_queries = "list",
    ip_addr = "character",
    port = "numeric",
    terminator = "character",
    url = "character",
    key_list = "list"
  ),
  methods = list(
    initialize = function(api_key, ip_addr, port = 8080, use_https = FALSE) {
      .self$api_key <- api_key
      .self$query_count <- 0
      .self$all_queries <- list()
      .self$ip_addr <- ip_addr
      .self$port <- port
      .self$terminator <- if (use_https) "s" else ""
      .self$url <- paste0(
        "http", .self$terminator,
        "://", .self$ip_addr, ":", .self$port
      )
      .self$key_list <- list()
      initial_list <- get_key_list()

      if (length(initial_list$subkeys) == 0) {
        key <- get_key()
        curr_list <- .self$key_list
        .self$key_list <- append(curr_list, list(key$subkey))
      } else {
        .self$key_list <- key_list
      }
    },
    get_key = function() {
      subkey_url <- paste0(.self$url, "/get_subkey")
      payload <- list("buyer_api_key" = .self$api_key)
      r <- GET(subkey_url, query = payload)
      rsp <- content(r, "text", encoding = "UTF-8")
      rsp <- fromJSON(rsp)
      if (status_code(r) != 200) {
        error <- rsp["message"]
        print(error)
      }

      subkey <- rsp["subkey"]
      .self$key_list <- c(subkey, .self$key_list)
      return(subkey)
    },
    get_key_list = function() {
      subkey_list_url <- paste0(.self$url, "/list_subkeys")
      payload <- list("buyer_api_key" = .self$api_key)
      r <- GET(subkey_list_url, query = payload)
      rsp <- content(r, "text", encoding = "UTF-8")
      rsp <- fromJSON(rsp)
      if (status_code(r) != 200) {
        error <- rsp["message"]
        print(error)
      }

      subkey_list <- rsp["subkeys"]
      return(subkey_list)

    }
  )
)
