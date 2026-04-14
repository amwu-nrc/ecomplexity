#' Add product names to a dataset
#'
#' @param data a data frame with a column of product codes
#' @param digits 
#' @param classification 
#'
#' @return tibble
#' @export
#'
#' @examples \dontrun{
#' library(complexitydata)
#' state_economic_complexity |> 
#' add_product_names()
#' }
#' @importFrom rlang .data
add_product_names <- function(data, digits, classification) {
  
  
  prod_data <- product_data |> 
    dplyr::filter(classification == {{classification}}) |> 
    dplyr::select(contains(as.character(digits))) |> 
    dplyr::distinct()
  
  join_by_col <- paste0("code_", digits)
  rename_col <- paste0("name_", digits)

  data |>  
    dplyr::left_join(prod_data, by = c("product_code" = join_by_col)) |> 
    dplyr::rename(hs_name_short_en = rename_col)
}

#' Add Atlas of Economic Complexity section colours to products
#'
#' @param data a data frame with a column of hs92 product codes
#'
#' @return tibble
#' @export
#'
#' @examples \dontrun{
#' read_complexitydata("state_economic_complexity") |>
#' add_product_colours()
#' }
add_product_colours <- function(data) {


  prod_data <- complexity_classification |>
    dplyr::select("hs_product_code", "colour")

  data |>
    dplyr::left_join(prod_data, by = 'hs_product_code')
}