library(httr)
library(jsonlite)


#' Buyer class for interacting with the network
#'
#' @param api_key The ``API KEY`` from the website console
#' @param ip_addr The ``IP Address`` key from the website console
#' @param port [optional] The ``PORT`` from the website console
#' @param use_https the https boolean toogle
#'
#' @export buyer
buyer <- setRefClass("buyer",
  fields = list(
    api_key = "character",
    query_count = "numeric",
    all_queries = "data.frame",
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
      .self$ip_addr <- ip_addr
      .self$port <- port
      .self$terminator <- if (use_https) "s" else ""
      .self$url <- paste0(
        "http", .self$terminator,
        "://", .self$ip_addr, ":", .self$port
      )
      columns <- c("query", "result", "accuracy")
      df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
      colnames(df) <- columns

      .self$all_queries <- df

      .self$key_list <- list()
      initial_list <- get_key_list()

      if (length(initial_list$subkeys) == 0) {
        key <- get_key()
        curr_list <- .self$key_list
        .self$key_list <- append(curr_list, list(key$subkey))
      } else {
        .self$key_list <- initial_list
      }
    },
    get_key = function() {
      "Returns a subkey to interact with the network"

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
      "Return the list of sub keys for the current buyer"

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

    },
    query = function(query_key = NULL, query = "") {
      "returns result and accuracy of the query"

      if (is.null(query_key)) {
        query_key <- .self$key_list[[1]]
      }
      query_url <- paste0(.self$url, "/execute_query")
      payload <- list(
        "buyer_api_key" = .self$api_key,
        "subkey" = query_key, "query" = query
      )
      r <- GET(query_url, query = payload)
      rsp <- content(r, "text", encoding = "UTF-8")
      rsp <- fromJSON(rsp)

      if (status_code(r) != 200) {
        error <- rsp["message"]
        print(error)
      }


      result <- as.character(rsp["result"])
      accuracy <- as.character(rsp["accuracy"])

      output <- data.frame(
        query = query, result = result,
        accuracy = accuracy
      )
      .self$all_queries <- rbind(.self$all_queries, output)

      return(output)
    },
    print_query_history = function() {
      "Pretty print the history queries with accuracy and results"

      table_frame <- paste(replicate(90, "-"), collapse = "")
      table_frame <- paste("+", table_frame, "+", sep = "")

      len <- nrow(.self$all_queries)

      for (i in 1:len) {
        print(table_frame)
        print(buyer$all_queries[["query"]][[i]])
        print(buyer$all_queries[["result"]][[i]])
        print(buyer$all_queries[["accuracy"]][[i]])
      }
    },
    get_columns = function(query_key = NULL) {
      "Return the table ddl columns"

      if (is.null(query_key)) {
        query_key <- .self$key_list[[1]]
      }
      query_url <- paste0(.self$url, "/list_columns")

      payload <- list("buyer_api_key" = .self$api_key, "subkey" = query_key)
      r <- GET(query_url, query = payload)
      rsp <- content(r, "text", encoding = "UTF-8")
      rsp <- fromJSON(rsp)


      if (status_code(r) != 200) {
        error <- rsp["message"]
        print(error)

      }

      columns <- rsp$columns
      return(columns)
    }
  )
)
