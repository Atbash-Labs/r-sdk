###########################################
####      EXAMPLE Querying USAGE       ####
###########################################

source("../R/sdk.R", chdir = TRUE)
print(getwd())
###########################################
####      Buyer Interaction Flow       ####
###########################################

api_key <- "buyer_key"
ip_addr <- "127.0.0.1"


buyer <- new_buyer(api_key, ip_addr)


print(buyer)

# retrieve the query sub key
query_key <- get_key(buyer)
# print(query_key)


# Initiate a query
sql_query <- "select count(*) as numpeople from public.condition_era_death"
result <- query(buyer, query = sql_query)
print(result)

print_query_history(buyer)
