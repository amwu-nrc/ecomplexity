#' Economic complexity rank time series
#'
#' @param data complexity dataset from `complexitydata` package
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' library(ecomplexity)
#' data <- read_complexitydata("state_economic_complexity")
#' graph_complexity_rank(data)
graph_complexity_rank <- function(data) {
  if (
    !all(
      c(
        "year",
        "location_code",
        "eci_rank",
        "eci_rank_first",
        "eci_rank_final"
      ) %in%
        colnames(data)
    )
  ) {
    data <- data |>
      dplyr::distinct(.data$year, .data$location_code, .data$eci_rank) |>
      dplyr::mutate(
        year = as.Date(paste0(year, "0101"), format = "%Y%d%m"),
        location_code = strayr::clean_state(
          .data$location_code,
          to = "state_name"
        )
      )

    data_final <- data |>
      dplyr::slice_max(order_by = .data$year) |>
      dplyr::distinct(.data$location_code, eci_rank_final = .data$eci_rank) |>
      dplyr::mutate(
        location_code = strayr::clean_state(
          .data$location_code,
          to = "state_name"
        )
      )

    data_first <- data |>
      dplyr::slice_min(order_by = .data$year) |>
      dplyr::distinct(.data$location_code, eci_rank_first = .data$eci_rank) |>
      dplyr::mutate(
        location_code = strayr::clean_state(
          .data$location_code,
          to = "state_name"
        )
      )

    data <- data |>
      dplyr::inner_join(data_first, by = c("location_code")) |>
      dplyr::inner_join(data_final, by = c("location_code"))
  }

  data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$year,
      y = .data$eci_rank,
      col = .data$location_code,
      fill = .data$location_code
    )) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(shape = 21, colour = "white", size = 2) +
    ggplot2::scale_y_reverse(
      breaks = NULL,
      expand = c(0, 0),
      name = NULL,
      sec.axis = ggplot2::dup_axis(
        breaks = unique(data$eci_rank_final),
        labels = paste0(
          unique(data$location_code),
          " (",
          unique(data$eci_rank_final),
          ")"
        ),
        name = NULL
      )
    ) +
    ggplot2::scale_x_date(
      limits = as.Date(c("19950101", "20220101"), format = "%Y%d%m"),
      name = NULL,
      expand = ggplot2::expansion(mult = c(0.02, 0))
    ) +
    ggplot2::scale_colour_manual(
      values = strayr::palette_state_name_2016,
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = strayr::palette_state_name_2016,
      name = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    cowplot::theme_cowplot() +
    ggplot2::theme(
      legend.position = "none",
      axis.line.y.right = ggplot2::element_blank(),
      axis.ticks.y.right = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0))
    )
}

#' Export Tree Map.
#'
#' Colours adjusted to match Atlas of Economic Complexity.
#'
#' @param year year
#' @param region which region to draw the map.
#' @param data export data
#' @param classification hs92/hs12/hs22
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' data <- read_complexitydata("state_economic_complexity")
#' graph_complexity_tree(data, 2022, "SA", classification = "hs92")

graph_complexity_tree <- function(data, year, region, digits, classification) {
  
  product_data <- product_data |> 
    dplyr::filter(classification == {{classification}}) |> 
    dplyr::distinct(dplyr::pick(contains(as.character(digits))), classification, name_1)

  data <- data |>
    dplyr::filter(
      .data$year == {{ year }},
      .data$country_iso3_code == {{ region }},
      .data$product_code != "unspecified"
    ) |>
    dplyr::mutate(
      export_value_share = scales::label_percent(accuracy = 0.1, scale = 100)(
        .data$export_value / sum(.data$export_value)
      ),
      pci_label = scales::label_number(accuracy = 0.01)(
        .data$pci
      )
    ) |>
    dplyr::left_join(product_data, by = c("product_code" = paste0("code_", digits)))
  
  name_label <- paste0("name_", digits)

  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      area = .data$export_value,
      fill = round(.data$pci, 3),
      subgroup = .data$name_1,
      label = paste(.data[[name_label]], .data$pci_label, sep = "\n")
    )
  ) +
    treemapify::geom_treemap(colour = "white") +
    treemapify::geom_treemap_text(
      ggplot2::aes(
        colour = ggplot2::after_scale(prismatic::best_contrast(
          fill,
          c("white", "black")
        ))
      ),
      place = "centre",
      size = 15
    ) +
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 1.5) +
    ggplot2::scale_fill_gradientn(
      colours = atlas_complexity_colours()$colour,
      values = atlas_complexity_colours()$percent,
      breaks = range(data$pci),
      labels = c("Low Complexity", "High Complexity")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = 10,
        title = NULL,
        direction = "horizontal",
        draw.ulim = F,
        draw.llim = F,
        label.position = "top"
      )
    ) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom", legend.justification = "center")
}

#' Draw Product Spaces
#' @param data export data
#' @param country country
#' @param year year
#' @param services should services be included? Default is FALSE which excludes service exports
#' @param classification hs92 or hs12
#'
#' @returns ggplot
#' @export
#'
#' @examples \dontrun{
#' library(dplyr)
#' data <- read_complexitydata("atlas_economic_complexity12")
#' graph_complexity_product_space(data, country = "AUS", year = 2024, classification = "hs12")
#' }
graph_complexity_product_space <- function(
  data,
  country,
  year,
  classification,
  highlight = NULL,
  colour_by = "sector"
) {
  rlang::check_installed(
    pkg = c("ggraph", "igraph"),
    reason = "to use `graph_complexity_product_space()`"
  )

  world_trade <- data |>
    dplyr::filter(year == {{ year }},
                  product_code != "XXXX") |>
    dplyr::summarise(global_exports = sum(export_value), .by = product_code) |> 
    dplyr::mutate(product_code = as.numeric(product_code))
  
  m <- data |> 
    dplyr::filter(year == {{year}},
                  country_iso3_code == {{country}},
                  product_code != "XXXX") |> 
    dplyr::mutate(m = ifelse(export_rca >= 1, 1, 0),
                  product_code = as.numeric(product_code)) |> 
    dplyr::select(product_code, m)

  pci <- data |> 
    dplyr::filter(year == {{year}},
    product_code != "XXXX") |> 
    dplyr::mutate(product_code = as.numeric(product_code)) |> 
    dplyr::distinct(product_code, pci)
  
  if (!is.null(highlight)) {
    m <- m |> 
      dplyr::mutate(m = ifelse(product_code %in% highlight, 1, 0))
  }


  product_space_colours <- tibble::tribble(
    ~product_space_cluster_name, ~colour,
    "Agriculture", "#EAC218",
    "Construction, Building, and Home Supplies", "#D1852A",
    "Electronic and Electrical Goods", "#52E2DE",
    "Industrial Chemicals and Metals", "#A42DE2",
    "Metalworking and Electrical Machinery and Parts", "#C64646",
    "Minerals", "#7C6760",
    "Textile and Home Goods", "#757777",
    "Textile Apparel and Accessories", "#36B250"
  )
  
  ps_data_edges <- product_space_edge_list |> 
    dplyr::rename(from = product_hs92_code_source,
           to = product_hs92_code_target)
  
  ps_data_vertices <- product_space_xy |> 
    dplyr::left_join(m, by = c("product_hs92_code" = "product_code")) |> 
    tidyr::replace_na(list(m = 0)) |> 
    dplyr::left_join(pci, by = c("product_hs92_code" = "product_code")) |> 
    dplyr::left_join(product_space_colours, by = c("product_space_cluster_name")) |> 
    dplyr::left_join(world_trade, by = c("product_hs92_code" = "product_code")) |> 
    dplyr::mutate(highlight = ifelse(m == 0, NA, product_space_cluster_name)) |> 
    dplyr::rename(name = product_hs92_code) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(colour = atlas_complexity_colours_manual(pci)) |> 
    dplyr::ungroup()
  

  ps_data <- igraph::graph_from_data_frame(d = ps_data_edges,
                                           vertices = ps_data_vertices) |> 
    ggraph::create_layout(layout = product_space_xy |> dplyr::select(-product_space_cluster_name))

  if (colour_by == "sector") {
  
  cols <- rlang::set_names(nm = product_space_colours$product_space_cluster_name, 
                           x = product_space_colours$colour)
    
    fill_fn <-    ggplot2::scale_fill_manual(name = NULL,
                               values = cols,
                      na.translate = F,
                      na.value = "#E9E9E9") 
    
    fill_var <- "highlight"
  } else {

    fill_fn <- ggplot2::scale_fill_identity()


    fill_var <- "colour"
  }
  

  
  ggraph::ggraph(ps_data) + 
    ggraph::geom_node_point(shape = 21, 
                            col = "grey",
                            ggplot2::aes(size = global_exports, 
                                fill = !!rlang::ensym(fill_var))) +
    ggraph::geom_edge_link(alpha = 0.01) + 
    fill_fn +
    ggplot2::scale_size_area(guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")

  
  }

#' Data Coverage
#'
#' @param data economic complexity input data
#' @param region quoted name of the region
#' @param activity quoted name of the 'activity' or product
#' @param flip logical to flip the graph
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' data <- read_complexitydata("sa3_indp3")
#' graph_complexity_coverage(data, "sa3", "indp")
#'
graph_complexity_coverage <- function(data, region, activity, flip = FALSE) {
  region_size = paste0(region, "_size")
  activity_size = paste0(activity, "_size")

  data |>
    dplyr::group_by(.data[[region]]) |>
    dplyr::mutate({{ region_size }} := sum(count)) |>
    dplyr::group_by(.data[[activity]]) |>
    dplyr::mutate({{ activity_size }} := sum(count)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      presence = count > 0,
      {{ region }} := forcats::fct_infreq(
        .data[[region]],
        w = .data[[region_size]]
      ),
      {{ activity }} := forcats::fct_infreq(
        .data[[activity]],
        w = .data[[activity_size]]
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = if (!flip) .data[[region]] else .data[[activity]],
      y = if (!flip) .data[[activity]] else .data[[region]],
      fill = presence
    )) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(
      breaks = c(TRUE, FALSE),
      values = c("#3182db", "white")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA),
      legend.position = "none"
    ) +
    ggplot2::coord_equal()
}

#' Complexity map
#'
#' @param data data of class sf
#' @param fill.var quoted name of the variable used for the fill aesthetic
#'
#' @return tmap object
#' @export
#'
#' @examples \dontrun{graph_complexity_map(data, "city_complexity_index")}
graph_complexity_map <- function(data, fill.var) {
  rlang::check_installed("tmap", reason = "to use `graph_complexity_map()`")

  tmap::tm_shape(data) +
    tmap::tm_polygons(
      fill = fill.var,
      col = "grey80",
      lwd = 0.1,
      fill.scale = tmap::tm_scale_continuous(values = "greek"),
      fill.legend = tmap::tm_legend(
        title = "Regional Complexity",
        orientation = "landscape"
      )
    ) +
    tmap::tm_place_legends_bottom()
}


#' Atlas of Economic Complexity PCI colour map
#' @importFrom grDevices rgb
#' @export
atlas_complexity_colours <- function() {
  rgbm <- function(r, g, b) {
    rgb(r, g, b, maxColorValue = 255)
  }

  tibble::tribble(
    ~"colour"           , ~"percent" ,
    rgbm(227, 159, 96), 0          ,
    rgbm(231, 173, 120), 0.278697  ,
    rgbm(235, 188, 143), 0.338965,
    rgbm(240, 202, 168) , 0.398272  ,
    rgbm(244, 217, 191) , 0.448314 ,
    rgbm(248, 231, 215) , 0.493999  ,
    rgbm(192, 228, 225) , 0.494099  ,
    rgbm(154, 211, 207), 0.533691,
    rgbm(116, 195, 189), 0.571435,
    rgbm(77, 178, 171), 0.606597,
    rgbm(40, 162, 153), 0.661681,
    rgbm(2, 146, 135)   , 1
  )
}

#' Atlas of Economic Complexity PCI Manual
#' 
atlas_complexity_colours_manual <- function(pci) {
   tibble::tribble(
    ~"colour", ~"pci_cutoff" ,
    "#e39f60", -3.2189,
    "#e7ad78", -1.34486,
    "#ebbc8f", -0.9396,
    "#f0caa8", -0.5408,
    "#f4d9bf", -0.2043,
    "#f8e7d7", 0.1029,
    "#c0e4e1", 0.1029,
    "#9ad3cf", 0.3698,
    "#74c3bd", 0.6236,
    "#4db2ab", 0.86004,
    "#28a299", 1.23044,
    "#029287", 3.5054
   )
  # ) |> 
  #   dplyr::filter(pci_cutoff > pci) |> 
  #   dplyr::slice_min(order_by = pci_cutoff, n = 1, with_ties = F) |> 
  #   dplyr::pull(colour)
}
