## code to prepare `complexity_colours` dataset goes here
library(stringr)
library(tibble)
library(readr)
library(dplyr)

sector_colours <- tribble(
  ~sector,  ~colour_
  "Agriculture",  "#F5CF23", 
  "Minerals", "#bb968a",
  "Chemicals", "#c57bd9",
  "Textiles", "#7ddaa1",
  "Stone", "#dab47d",
  "Metals", "#d97b7b",
  "Machinery", "#7ba2d9",
  "Electronics", "#7ddada",
  "Vehicles", "#8d7bd8",
  "Other", "#2a607c",
  "Services", "#b23d6d"
) 

cluster_colours <- tribble(
  ~cluster, ~colour,
  "Agricultural", "#EAC218",
  "Construction", "#D1852A",
  "Electronics", "#52E2DE",
  "Chemicals and Basic Metals", "#A42DE2",
  "Metalworking and Machinery", "#C64646",
  "Minerals", "#7C6760",
  "Textile and Home", "#757777",
  "Apparel", "#36B250"
)

# HS 22 Data

product_data22 <- read_csv("data-raw/product_hs22.csv")
product_data6 <- product_data22 |> 
  filter(product_level == 6) |> 
  select(product_hs22_code6 = product_hs22_code,
         product_name_short6 = product_name_short,
         product_id,
         product_parent_id6 = product_parent_id)

product_data4 <- product_data22 |> 
  filter(product_level == 4) |> 
  select(product_hs22_code4 = product_hs22_code, 
         product_name_short4 = product_name_short,
         product_id,
         product_parent_id4 = product_parent_id) 

product_data2 <- product_data22 |> 
  filter(product_level == 2) |> 
  select(product_hs22_code2 = product_hs22_code,
         product_name_short2 = product_name_short,
         product_id,
         product_parent_id2 = product_parent_id) 

product_data1 <- product_data22 |> 
  filter(product_level == 1) |> 
  select(product_hs22_code1 = product_hs22_code,
         product_name_short1 = product_name_short,
         product_id,
         product_parent_id1 = product_parent_id)

product_data22 <- inner_join(product_data6, product_data4, by = c("product_parent_id6" = "product_id")) |> 
  inner_join(product_data2, by = c("product_parent_id4" = "product_id")) |> 
  inner_join(product_data1, by = c("product_parent_id2" = "product_id")) |> 
  select(code_6 = product_hs22_code6, code_4 = product_hs22_code4, code_2 = product_hs22_code2, code_1 = product_hs22_code1,
         name_6 = product_name_short6, name_4 = product_name_short4, name_2 = product_name_short2, name_1 = product_name_short1) |> 
  mutate(classification = "hs22")

# HS 12 Data
product_data12 <- read_csv("data-raw/product_hs12.csv")
product_data6 <- product_data12 |> 
  filter(product_level == 6) |> 
  select(product_hs12_code6 = product_hs12_code,
         product_name_short6 = product_name_short,
         product_id,
         product_parent_id6 = product_parent_id)

product_data4 <- product_data12 |> 
  filter(product_level == 4) |> 
  select(product_hs12_code4 = product_hs12_code, 
         product_name_short4 = product_name_short,
         product_id,
         product_parent_id4 = product_parent_id) 

product_data2 <- product_data12 |> 
  filter(product_level == 2) |> 
  select(product_hs12_code2 = product_hs12_code,
         product_name_short2 = product_name_short,
         product_id,
         product_parent_id2 = product_parent_id) 

product_data1 <- product_data12 |> 
  filter(product_level == 1) |> 
  select(product_hs12_code1 = product_hs12_code,
         product_name_short1 = product_name_short,
         product_id,
         product_parent_id1 = product_parent_id)

product_data12 <- inner_join(product_data6, product_data4, by = c("product_parent_id6" = "product_id")) |> 
  inner_join(product_data2, by = c("product_parent_id4" = "product_id")) |> 
  inner_join(product_data1, by = c("product_parent_id2" = "product_id")) |> 
  select(code_6 = product_hs12_code6, code_4 = product_hs12_code4, code_2 = product_hs12_code2, code_1 = product_hs12_code1,
         name_6 = product_name_short6, name_4 = product_name_short4, name_2 = product_name_short2, name_1 = product_name_short1) |> 
  mutate(classification = "hs12")

#HS 92 Data

product_data92 <- read_csv("data-raw/product_hs92.csv")
product_data6 <- product_data92 |> 
  filter(product_level == 6) |> 
  select(product_hs92_code6 = product_hs92_code,
         product_name_short6 = product_name_short,
         product_id,
         product_parent_id6 = product_parent_id)

product_data4 <- product_data92 |> 
  filter(product_level == 4) |> 
  select(product_hs92_code4 = product_hs92_code, 
         product_name_short4 = product_name_short,
         product_id,
         product_parent_id4 = product_parent_id) 

product_data2 <- product_data92 |> 
  filter(product_level == 2) |> 
  select(product_hs92_code2 = product_hs92_code,
         product_name_short2 = product_name_short,
         product_id,
         product_parent_id2 = product_parent_id) 

product_data1 <- product_data92 |> 
  filter(product_level == 1) |> 
  select(product_hs92_code1 = product_hs92_code,
         product_name_short1 = product_name_short,
         product_id,
         product_parent_id1 = product_parent_id)

product_data92 <- inner_join(product_data6, product_data4, by = c("product_parent_id6" = "product_id")) |> 
  inner_join(product_data2, by = c("product_parent_id4" = "product_id")) |> 
  inner_join(product_data1, by = c("product_parent_id2" = "product_id")) |> 
  select(code_6 = product_hs92_code6, code_4 = product_hs92_code4, code_2 = product_hs92_code2, code_1 = product_hs92_code1,
         name_6 = product_name_short6, name_4 = product_name_short4, name_2 = product_name_short2, name_1 = product_name_short1) |> 
  mutate(classification = "hs92")


complexity_classification22 <- product_data22 |> 
  select(product_code = code_4,
         sector = name_1) |> 
  inner_join(complexity_colours) |> 
  mutate(classification = "hs22") |> 
  distinct()


complexity_classification12 <- product_data12 |> 
  select(product_code = code_4,
         sector = name_1) |> 
  inner_join(complexity_colours) |> 
  mutate(classification = "hs12") |> 
  distinct()

complexity_classification92 <- product_data92 |> 
  select(product_code = code_4,
         sector = name_1) |> 
  inner_join(complexity_colours) |> 
  mutate(classification = "hs92") |> 
  distinct()

product_data <- bind_rows(
  product_data22,
  product_data12,
  product_data92
)

complexity_classification <- bind_rows(
  complexity_classification22,
  complexity_classification12,
  complexity_classification92
)

product_space_xy <- read_csv("data-raw/umap_layout_hs92.csv",
                              col_select = c(product_code = product_hs92_code,
                                         x = product_space_x,
                                         y = product_space_y,
                                         product_space_cluster_name)) |> 
  arrange(product_code)

product_space_edge_list <- read_csv("data-raw/top_edges_hs92.csv") |> 
  arrange(product_hs92_code_source)


usethis::use_data(complexity_classification, product_space_xy, product_space_edge_list, internal = TRUE, overwrite = TRUE)
usethis::use_data(product_data, compress = "xz", overwrite = TRUE)

