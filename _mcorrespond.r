# Interpreting MCA
## Variance = eigenvalues
## DimX = Coordinate of the dimension (eigenvalues??)
##   Coordinates for cats = square correlation ratio (eta2) between var and dim
##   Coordinates for num = square correlation coeff (R2) between var and dim
## ctr = Contribution to the construction of the dimension (%)
## cos2 = Quality of the representation. Ranges 0 to 1. Closer to 1, better
##  projected  on the dimension/map. The more dimensionthe smalled cos2 expected
## vtest = Test if category significantly different from 0. Ranges -inf to inf.
##  Takes >|2| if sig. diff from 0, else <|2|
## eta = Influence of each categorical var in the dimension. Is the Squared
##  correlation ratio, same use in one-way ANOVA. Ranges 0 to 1. Closer to 1,
##  stronger between dimension and variable
## Suplementary variables dont contribute to the construction of the dimension
## methods = Indicator matrix or Burt matrix. Burt method change explained
##  variance and contribution of categories. Explained variabce is > with Burt

# Model Step 1
m_vars <- c("health_bar_quality2_bin", "health_bar_access2_bin",
  "educa_bar_quality_bin", "educa_bar_access_bin",
  "aid_received_bin", "complaint_mechanisms_bin", "aid_satisfaction_bin",
  "risk_eviction_bin", "demolition_order_bin", "unsafe_shelter_bin", "shelter_issues_bin",
  "child_marriage_bin", "child_labour_bin", "crisis_cope_bin", "fies_mod_bin",
  "poverty_60_bin", "poverty_30_bin", "hoh_female_bin", "refugee_status_bin",
  "distress_bin", "disability_bin", "viol_physical2_bin", "viol_exploit_bin",
  "viol_harrasment_bin", "settler_bin", "flood_bin", "unsafe_waste_bin",
  "school_unsafe_bin")

# Suplementary vars
supcat_var <- c("area_cat")
# Check vars exist
c(m_vars, supcat_var)[!(c(m_vars, supcat_var)  %in% colnames(indicators_data))]
# Format model data
m_data <- indicators_data[, c(m_vars, supcat_var)]
supcat_var_i <- which(colnames(m_data) %in% supcat_var)

# Compute model
m_res <- FactoMineR::MCA(X = m_data, ncp = 2, quali.sup = supcat_var_i, graph = FALSE, row.w = indicators_data$weights)

# Corrected eigen
n_var <- nrow(m_res$var$coord)/2
eig <- as.numeric(m_res$eig[,1])
df_eig_adj_m1 <- data.frame(dims = row.names(m_res$eig),
  eig_adj = benz_adj(eig, n_var), 
  peig_adj = benz_adj(eig, n_var, perc = TRUE),
  eig = as.numeric(m_res$eig[,1]),
  peig = as.numeric(m_res$eig[,2])) |>
  dplyr::mutate(dims = forcats::fct_reorder(dims, eig, .desc = TRUE))


# Dimensional plot
df_mca_m1 <- factoextra::fviz_mca_var(m_res, col.var = "contrib", axes = 1:2)$data
# Tidy results
df_m1_res <- tidyr_res(m_res$var)
# Quality results for dim 1
p_cos1_m1 <- p_cont_cos(df_m1_res, coef = "cos2", axis = 1, filter = TRUE)
p_test1_m1 <- p_vtest(df_m1_res, coef = "v.test", axis = 1, filter = TRUE)
# Quality results for dim 2
p_cos2_m1 <- p_cont_cos(df_m1_res, coef = "cos2", axis = 2, filter = TRUE)
p_test2_m1 <- p_vtest(df_m1_res, coef = "v.test", axis = 2, filter = TRUE)


# Strep 2
m_vars <- c("health_bar_quality2_bin", "health_bar_access2_bin",
  "school_unsafe_bin", "aid_received_bin", "complaint_mechanisms_bin",
  "aid_satisfaction_bin", "risk_eviction_bin", "demolition_order_bin",
  "unsafe_shelter_bin", "shelter_issues_bin", "crisis_cope_bin",
  "fies_mod_bin", "poverty_60_bin", "poverty_30_bin", "refugee_status_bin",
  "distress_bin", "disability_bin", "hoh_female_bin",
  "viol_bin", "child_marriage_bin", "child_labour_bin",
  "settler_bin", "flood_bin", "unsafe_waste_bin")

# Suplementary vars
supcat_var <- c("area_cat")
# Check vars exist
c(m_vars, supcat_var)[!(c(m_vars, supcat_var)  %in% colnames(indicators_data))]
# Format model data
m_data <- indicators_data[, c(m_vars, supcat_var)]
supcat_var_i <- which(colnames(m_data) %in% supcat_var)

# Compute model
m_res <- FactoMineR::MCA(X = m_data, ncp =2, quali.sup = supcat_var_i, graph = FALSE, row.w = indicators_data$weights)

# Corrected eigen
n_var <- nrow(m_res$var$coord)/2
eig <- as.numeric(m_res$eig[,1])
df_eig_adj_m2 <- data.frame(dims = row.names(m_res$eig),
  eig_adj = benz_adj(eig, n_var), 
  peig_adj = benz_adj(eig, n_var, perc = TRUE),
  eig = as.numeric(m_res$eig[,1]),
  peig = as.numeric(m_res$eig[,2])) |>
  dplyr::mutate(dims = forcats::fct_reorder(dims, eig, .desc = TRUE))


# Dimensional plot
df_mca_m2 <- factoextra::fviz_mca_var(m_res, col.var = "contrib", axes = 1:2)$data
# Tidy results
df_m2_res <- tidyr_res(m_res$var)
# Quality results for dim 1
p_cos1_m2 <- p_cont_cos(df_m2_res, coef = "cos2", axis = 1, filter = TRUE)
p_test1_m2 <- p_vtest(df_m2_res, coef = "v.test", axis = 1, filter = TRUE)
# Quality results for dim 2
p_cos2_m2 <- p_cont_cos(df_m2_res, coef = "cos2", axis = 2, filter = TRUE)
p_test2_m2 <- p_vtest(df_m2_res, coef = "v.test", axis = 2, filter = TRUE)


# Calculate Index ----

# Calculate from data
# Get weights
w <- dplyr::select(df_m2_res, var, w = Dim.1_coord) |>
  dplyr::filter(w > 0) |>
  dplyr::mutate(var = gsub("^(.+)_1", "\\1", var), w = w / max(w)) #adjusted weigths

# compute risk index
cross_vars <- c(
  "uuid", "region_cat", "area_cat", "westbank_cat", "weights", "safe_water_bin",
  "children_bin", "hh_debt_bin"
)
data_risk <- dplyr::select(indicators_data, uuid, dplyr::all_of(m_vars)) |>
  tidyr::pivot_longer(-uuid) |>
  dplyr::left_join(w, by = c("name" = "var")) |>
  dplyr::mutate(value = as.numeric(value) - 1) |> # fct to num make it 1,2. -1 for dummy
  dplyr::group_by(uuid) |>
  dplyr::summarise(risk_sc = weighted.mean(value, w)) |>
  dplyr::left_join(indicators_data[, c(cross_vars)], by = "uuid")


