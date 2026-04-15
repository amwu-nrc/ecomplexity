#' Harmonised System (1992/2012/2022) product data.
#' 
#' A dataset containing the names, product code, group name and section name of products
#' the Harmonised System. 
#' 
#' @format Tibble with
#' \describe{
#' \item{code_6}{Six digit product code}
#' \item{code_4}{Four digit product code}
#' \item{code_2}{Two digit product code}
#' \item{code_1}{One digit product code}
#' \item{name_6}{Six digit product name}
#' \item{name_4}{Four digit product name}
#' \item{name_2}{Two digit product name}
#' \item{name_1}{One digit product name}
#' \item{classification}{HS Classification}
#' }
"product_data"

#' ANZSIC to Harmonised System concordance
#' 
#' The relationship between product codes and Australian industry classifications. 
#' Derived from Appendix 6.1 of the ABS International Merchandise Trade Australia: Concepts, Sources and Methods release. 
#' 
#' @format Tibble with
#' \describe{
#' \item{hs_product_code}{character representation of the product code - HS92 4 digits}
#' \item{anzsic}{character representation of the ANZSIC class}
#' }
#' @source \url{https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/international-merchandise-trade-australia-concepts-sources-and-methods/latest-release}
"anzsic_hs"


