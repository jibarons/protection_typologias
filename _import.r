# Import ----

# Load packages ---
# Load packages ---
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(FactoMineR)
library(factoextra)
library(VIM)
library(RM.weights)
library(sf)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

# Import Survey data ----
hh_data <- tibble::tibble(utils::read.csv("_data\\quan\\opt_hh_raw.csv")) |>
  dplyr::rename("uuid" = X_uuid) |>
  dplyr::filter(region %in% c("west_bank", "ej", "H2"))

# Import Qualitative data ---- 
q_data <- readr::read_csv("_data\\qual\\tool_pa.csv")
q_data <- q_data |>
  dplyr::filter(deleted_at == "NULL", lubridate::year(created_at) == 2022) |>
  dplyr::select(id, cluster_id, text_identifier, text, highlighted_phrase) |>
  dplyr::rename(text_id = text_identifier)

# Import GIS data ----
## Import Admin2  doundaries
adm2 <- sf::read_sf("_data\\gis\\opt_adm2\\opt_adm2_210806.shp")
## Import Oslo area
oslo <- sf::read_sf("_data\\gis\\oslo_area\\oslo_area.shp") |>
  dplyr::mutate(
    CLASS = dplyr::if_else(CLASS == "A" & Shape_Leng > 1e6, "B", CLASS)
  )
## Import WB all communities
comm_wb <- sf::read_sf("_data\\gis\\wb_comm\\wb_communities.shp")
## Get CPA clusters and admins
cluster <- readr::read_csv("_data\\gis\\cluster.csv")
admin <- readr::read_csv("_data\\gis\\admin_units.csv") |>
  dplyr::mutate(id = as.character(id))
# Fix aberrant value son CPA clusters
admins <- dplyr::left_join(cluster, admin, by = c("admin_unit_id" = "id")) |>
  dplyr::select(cluster_id = id, name, code, cluster_coord = coordinates.x) |>
  tidyr::separate(cluster_coord, c("long_x", "lat_y"), sep = ",") |>
  dplyr::mutate(
    dplyr::across(c(long_x, lat_y), as.numeric),
    lat_y = dplyr::if_else(code == "Madama", 35.232268, lat_y),
    lat_y = dplyr::if_else(code == "Asira al Qibliya", 35.213489, lat_y),
    lat_y = dplyr::if_else(code == "Kobar Bedouins", 35.14754, lat_y),
    lat_y = dplyr::if_else(code == "Dahyiat al Aqbat", 35.259231, lat_y),
    lat_y = dplyr::if_else(code == "Marj Na'ja Herders", 35.531463, lat_y),
    lat_y = dplyr::if_else(code == "Abu Al-Urgan", 35.020753, lat_y),
    lat_y = dplyr::if_else(code == "Khirbet Bism", 35.032343, lat_y),
    lat_y = dplyr::if_else(code == "Al Jwaya", 35.1504542, lat_y),
    lang_x = dplyr::if_else(code == "Al Jwaya", 31.42663, long_x),
  ) |>
  dplyr::filter(long_x != 0, !is.na(lat_y), !is.na(lat_y))

# Convert CPA cluster to geo class
pcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
loc_qual <- sf::st_as_sf(x = admins, coords = c("lat_y", "long_x"), crs = pcrs)

## Funciones de formato

format_tbl <- function(.data, caption = NULL, digits = 2, align = TRUE) {
  if (align) {
    align.c <- rep("c", ncol(.data) - 1)
      
    kableExtra::kbl(
      .data, format = "latex", booktabs = TRUE,
      align = c("l", align.c), digits = digits,
      caption = caption
    ) %>%
      kableExtra::kable_styling(latex_options = c("hold_position", "striped")) %>%
      kableExtra::row_spec(0, bold = TRUE) %>%
      kableExtra::column_spec(0, bold = TRUE)
  } else {
    kableExtra::kbl(
      .data, format = "latex", booktabs = TRUE, 
      digits = digits, caption = caption
    ) %>%
      kableExtra::kable_styling(latex_options = c("hold_position", "striped")) %>%
      kableExtra::row_spec(0, bold = TRUE) %>%
      kableExtra::column_spec(0, bold = TRUE)
  }
}
fmat_num = function(x, d = 0) format(
  as.numeric(sprintf(paste0("%.", d, "f"), x)),
   big.mark = ".", decimal.mark = ","
)
fmat_per = function(x, d = 1, sym = "%%") sprintf(paste0("%.",d,"f", sym), x)

# Import util funs
tidyr_res <- function(result = m1_res$var, coef = c("coord", "contrib",
  "cos2", "v.test")) {

  result <- result[coef]

  df_result <- purrr::imap(
    result, ~ {
      nam <- as.character(.y)
      df_results <- data.frame(.x)
      df_results$var <- row.names(df_results)
      df_results <- dplyr::rename_with(
        df_results, ~paste0(., "_", nam), dplyr::matches("Dim\\.")
      )
      df_results <- tibble::tibble(df_results) |>
        dplyr::relocate(var, .before = 1)
    }
  )
  df_result <- Reduce(df_result, f = dplyr::full_join)
  return(df_result)
}


benz_adj <- function(x, n_var = 28, perc = FALSE) {
  u <- 1/n_var
  x_adj <- ifelse(u < x, (n_var / (n_var - 1))^2 * (x - (1 / n_var))^2, 0)
  p_adj <- ifelse(u < x, x_adj/sum(x_adj), 0)
  if (perc) {return(p_adj)} else {return(x_adj)}
}



p_cont_cos <- function(.data, coef = "cos2", axis = 1:2, filter = TRUE) {

  target <- paste0(axis, "_", coef, collapse = "|")

  .data <- dplyr::select(.data, var, dplyr::matches(target))

  if (filter) {
      .data <- dplyr::filter(.data, grepl(".+_1$", var)) |>
        dplyr::mutate(var = gsub("^(.+)_bin_1$", "\\1", var))
  }

  .data <- .data %>% dplyr::mutate(
    avg = round(rowSums(.[-1]), 5),
    var = forcats::fct_reorder(as.factor(var), avg, .desc = TRUE),
    .before = 2,
  ) |>
    dplyr::arrange(dplyr::desc(avg))

  coef_avg <- mean(.data$avg)

  p <- ggplot2::ggplot(.data, ggplot2::aes(x = var, y = avg, fill = avg)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_gradient2(low = "red", mid = "grey", high = "blue",
      midpoint = mean(.data$avg)) +
    ggplot2::geom_hline(yintercept = coef_avg, linetype = "dashed", 
      color = "red", linewidth = 0.3) +
    ggplot2::labs(x = "Indicadores", y = "Contribución relativa") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_text(size = 8)
    )
  return(p)
}

p_vtest <- function(.data, coef = "v.test", axis = 1:2, filter = TRUE) {

  target <- paste0(axis, "_", coef, collapse = "|")

  .data <- dplyr::select(.data, var, dplyr::matches(target))

  if (filter) {
      .data <- dplyr::filter(.data, grepl(".+_1$", var)) |>
        dplyr::mutate(var = gsub("^(.+)_bin_1$", "\\1", var))
  }

  .data <- .data %>% dplyr::mutate(
    avg = round(rowMeans(.[-1]), 5),
    var = forcats::fct_reorder(as.factor(var), avg, .desc = TRUE),
    .before = 2,
  ) |>
    dplyr::arrange(dplyr::desc(avg))

  p <- ggplot2::ggplot(.data, ggplot2::aes(x = var, y = avg, fill = avg)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = -2, linetype = "dashed", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(low = "red", mid = "grey", high = "blue",
      midpoint = mean(.data$avg)) +
    ggplot2::labs(x = "Indicadores", y = "Coeficientes V-Test") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_text(size = 8)
    )

  return(p)
}

