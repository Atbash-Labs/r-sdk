###########################################
####      EXAMPLE Querying USAGE       ####
###########################################

source("sdk.r")

###########################################
####      Buyer Interaction Flow       ####
###########################################

api_key <- "buyer_key"
ip_addr <- "127.0.0.1"


buyer <- new_buyer(api_key, ip_addr)

# retrieve the query sub key
query_key <- get_key(buyer)
print(query_key)


# Initiate a query
sql_query <- "select count(*) as numpeople from public.condition_era_death"
result <- query(buyer, query_key, sql_query)

# get table columns
columns <- get_columns(buyer, query_key)
print(columns)