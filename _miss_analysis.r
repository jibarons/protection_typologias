
# Description of indicators
desc_i <- c(
  "health_bar_quality2_bin" = "Percibe barreras de calidad en la salud",
  "health_bar_access2_bin" = "Percibe barreras de acceso a la salud",
  "educa_bar_quality_bin" = "Percibe barreras de calidad en la educación",
  "educa_bar_access_bin" = "Percibe barreras de acceso a la educación",
  "aid_received_bin" = "Recibe ayuda recibida", # Need to change name!!!
  "complaint_mechanisms_bin" = "Conoce mecanismos de reclamación",
  "aid_satisfaction_bin" = "Satisfecho con la ayuda recibida",
  "risk_eviction_bin" = "Percibe riesgo de desahucio",
  "demolition_order_bin" = "Tiene una orden de demolición",
  "unsafe_shelter_bin" = "Vivienda es insegura",
  "shelter_issues_bin" = "La vivienda tiene problemas",
  "child_marriage_bin" = "Menores en situación de matrimonio infantil",
  "child_labour_bin" = "Menores en situación de trabajo infantil",
  "crisis_cope_bin" = "Aplica estrategias de crisis ",
  "fies_mod_bin" = "Experimenta inseguridad alimentaria",
  "poverty_60_bin" = "Bajo el umbral de pobreza monetaria (60% de la mediana)",
  "poverty_30_bin" = "Bajo el umbral de pobreza monetaria (30% de la mediana)",
  "hoh_female_bin"= "Hogar encabezado por una mujer",
  "refugee_status_bin" = "Posee estatus de refugiado",
  "distress_bin" = "Miembros del hogar con estrés postraumático",
  "disability_bin" = "Miembros del hogar con necesidades especiales",
  "viol_physical2_bin" = "Percibe riesgo de violencia física",
  "viol_exploit_bin" = "Percibe riesgo de explotación (violencia no física)",
  "viol_harrasment_bin" = "Percibe riesgo de acoso (violencia no física)",
  "settler_bin" = "Afectado por violencia de colonos",
  "flood_bin" = "Afectado por inundaciones",
  "unsafe_waste_bin" = "Gestión de desechos insegura",
  "school_unsafe_bin" = "Percibe riesgo en asistir a la escuela"
)
desc_i <- data.frame(Codigo = names(desc_i), Descripcion = unname(desc_i)) |>
  dplyr::mutate(Codigo = gsub("^(.+)_bin$", "\\1", Codigo)) |>
  dplyr::arrange(Codigo)


# Missing analysis for relevant indicators for the model only
indicators <- c("health_bar_quality2_bin", "health_bar_access2_bin",
  "educa_bar_quality_bin", "educa_bar_access_bin",
  "aid_received_bin", "complaint_mechanisms_bin", "aid_satisfaction_bin",
  "risk_eviction_bin", "demolition_order_bin", "unsafe_shelter_bin", "shelter_issues_bin",
  "child_marriage_bin", "child_labour_bin", "crisis_cope_bin", "fies_mod_bin",
  "poverty_60_bin", "poverty_30_bin", "hoh_female_bin", "refugee_status_bin",
  "distress_bin", "disability_bin", "viol_physical2_bin", "viol_exploit_bin",
  "viol_harrasment_bin", "settler_bin", "flood_bin", "unsafe_waste_bin",
  "school_unsafe_bin")

ind_miss_per <- dplyr::select(indicators_data, uuid, dplyr::all_of(indicators)) |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(-uuid) |>
  dplyr::mutate(miss = is.na(value)) |>
  dplyr::group_by(name) |>
  dplyr::summarise(n = dplyr::n(), miss = mean(miss)) |>
  dplyr::filter(grepl("_bin$", name))

ind_miss_per <- dplyr::arrange(ind_miss_per, name) |>
  dplyr::mutate(
    miss = fmat_per(miss * 100, d = 2, sym = ""), 
    name = gsub("^(.+)_bin$", "\\1", name)
  ) |>
  dplyr::mutate_all(utils::type.convert, as.is = TRUE) |>
  dplyr::rename("Indicador" = name, "%" = miss)

ind_miss_per_wide <- data.frame(x = "error")
ind_miss_per_wide <- bind_cols(
  dplyr::slice(ind_miss_per, 1:14),
  dplyr::slice(ind_miss_per, 15:28), 
  .name_repair = "unique"
)
# Remove indicators with missing values
keep <- c("ind_miss_per_wide", "desc_i")
rm(list = ls()[!(ls() %in% keep)])