#' Read data from the Economic Complexity API
#'
#' @param query GraphiQL query. See `https://github.com/harvard-growth-lab/api-docs/blob/main/atlas.md` for examples.
#' @param query_name The name of the query. Whatever you want to call it.
#'
#' @returns dataframe
#' @export
#'
read_complexity_api <- function(query, query_name) {

con <- ghql::GraphqlClient$new(
  url = "https://atlas.hks.harvard.edu/api/graphql",
)

#Query object is the first line of the query
query_object <- stringr::str_split(query, pattern = "\n")[[1]][2]
query_object <- trimws(stringr::str_remove_all(query_object, "\\("))

qry <- ghql::Query$new()

qry$query(query_name,  
          query)

qj <- con$exec(qry$queries[[query_name]])

res <- jsonlite::fromJSON(qj, flatten = T, simplifyDataFrame = T) # Requires extra work to convert to a tibble

res[["data"]][[query_object]] |> 
  as_tibble()


}
