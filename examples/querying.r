###########################################
####      EXAMPLE Querying USAGE       ####
###########################################

source("../R/sdk.R", chdir = TRUE)

###########################################
####      Buyer Interaction Flow       ####
###########################################

api_key <- "buyer_key"
ip_addr <- "127.0.0.1"

buyer <- new_buyer(api_key, ip_addr)


# Initiate a query
sql_query <- "select count(*) as numpeople from public.condition_era_death"
result <- query(buyer, query = sql_query)

sql_query <- "select count(*) as people_per_condition from person"
result <- query(buyer, query = sql_query)


print_query_history(buyer)
