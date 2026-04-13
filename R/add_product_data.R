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
#' add_product_names()
#' }
#' @importFrom rlang .data
add_product_names <- function(data, digits, classification) {
  
  if (classification == "hs92") {
    product_data <- product_data92
  } else {
    product_data <- product_data12
  }
  
  prod_data <- product_data |> 
    dplyr::select(contains(digits)) |> 
    dplyr::distinct()

  data |>  
    dplyr::left_join(prod_data, by = c("product_code" = "code_4")) |> 
    dplyr::rename(hs_name_short_en = name_4)
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