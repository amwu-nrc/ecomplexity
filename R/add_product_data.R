#' Add product names to a dataset
#'
#' @param data a data frame with a column of hs92 product codes
#'
#' @return tibble
#' @export
#'
#' @examples \dontrun{
#' library(complexitydata)
#' state_economic_complexity |> 
#' add_product_digits()
#' }
#' @importFrom rlang .data
add_product_names <- function(data) {
  
  prod_data <- product_data |> 
    dplyr::select("hs_product_name_short_en", "hs_product_code")
  
  data |>  
    dplyr::left_join(prod_data, by = "hs_product_code")
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