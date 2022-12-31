
## Cleaning Script

perc_missing <- apply(hh_data, 2, function(x) mean(is.na(x)))
perc_missing <- data.frame("question" = names(perc_missing), "perc" = perc_missing)
perc_miss_single <- perc_missing[!grepl("^.+\\..+$", perc_missing$question),]
perc_miss_single <- perc_miss_single[perc_miss_single$perc > 0, ]


# Clean one sporious case
 hh_data <- dplyr::filter(hh_data, uuid != "")

# Imputation of missing values for livleihood variables ----
hh_data <- VIM::hotdeck(
  hh_data, c("tot_income", "how_much_debt"),
  domain_var = c("strata"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
   "primary_livelihood"),
)

# Imputation of missing values for total expenditure
hh_data <- hh_data %>% 
  dplyr::mutate(
    tot_expenses = rowSums(dplyr::select(., dplyr::matches("_exp$")), na.rm = TRUE)
  )

hh_data <- VIM::hotdeck(
  hh_data, c("tot_expenses"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood", "tot_income", "how_much_debt"),
)

# Missing values imputation for solid waste
hh_data <- VIM::hotdeck(
  hh_data, c("solid_waste_disposal"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood"),
)

# Missing values imputation for Eviction
hh_data <- dplyr::mutate(hh_data, hh_risk_eviction = dplyr::if_else(
  hh_risk_eviction == "do_not_know", NA_character_, hh_risk_eviction
))

hh_data <- VIM::hotdeck(
  hh_data, c("hh_risk_eviction"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)

# Missing values imputation for food consumption score
hh_data <- VIM::hotdeck(
  hh_data, c("cereals", "nuts_seed", "meat", "vegetables", "fruits", "milk_dairy",
   "oil_fats", "sweets"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood"),
)

# Missing values imputation for Aid Received
hh_data <- dplyr::mutate(hh_data, aid_received = dplyr::if_else(
  aid_received %in% c("do_not_know", "decline_to_answer"), NA_character_, aid_received
))

hh_data <- VIM::hotdeck(
  hh_data, c("aid_received"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood"),
)

# Missing values imputation for Demolition
hh_data <- dplyr::mutate(hh_data, demolition_order_wb = dplyr::if_else(
  demolition_order_wb %in% c("do_not_know", "decline_to_answer", ""), 
  NA_character_, demolition_order_wb
))
hh_data <- VIM::hotdeck(
  hh_data, c("demolition_order_wb"),
  domain_var = c("strata"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)
# table(hh_data$demolition_order_wb, useNA = "always")

# Missing values imputation for school safety
hh_data <- dplyr::mutate(hh_data, school_safety = dplyr::if_else(
  school_safety %in% c("not_sure", "prefer_answer"), NA_character_, school_safety
))
hh_data <- VIM::hotdeck(
  hh_data, c("school_safety"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)
# table(hh_data$school_safety, useNA = "always")

# Missing values imputation for Complaint Mechanism
hh_data <- dplyr::mutate(hh_data, complaint_mechanisms = dplyr::case_when(
    complaint_mechanisms == "" ~ "no", # No missing if "" then no aid recieved 
    complaint_mechanisms %in% c("decline_to_answer", "do_not_know") ~ NA_character_,
    TRUE ~ complaint_mechanisms
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("complaint_mechanisms"),
  domain_var = c("strata"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood", "aid_received")
)
# table(hh_data$complaint_mechanisms, useNA = "always")

# Missing values imputation for Aid satsfaction
hh_data <- dplyr::mutate(hh_data, aid_satisfaction = dplyr::case_when(
    aid_satisfaction == "" ~ "no", # No missing if "" then no aid recieved 
    aid_satisfaction %in% c("decline_to_answer", "do_not_know") ~ NA_character_,
    TRUE ~ aid_satisfaction
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("aid_satisfaction"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood", "aid_received")
)
# table(x$aid_satisfaction, useNA = "always")



# Missing values imputation for latrine waste
hh_data <- dplyr::mutate(hh_data, latrine_waste_drainage = dplyr::case_when(
    latrine_waste_drainage %in% c("decline_to_answer", "do_not_know", "") ~ NA_character_,
    TRUE ~ latrine_waste_drainage
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("latrine_waste_drainage"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)
# table(hh_data$latrine_waste_drainage, useNA = "always")

# Missing values imputation for Settlers threats
hh_data <- dplyr::mutate(hh_data, settler_threats_wb = dplyr::case_when(
    settler_threats_wb %in% c("decline_to_answer", "do_not_know", "") ~ NA_character_,
    TRUE ~ settler_threats_wb
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("settler_threats_wb"),
  domain_var = c("strata"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)
# table(x$settler_threats_wb, useNA = "always")

# Missing values imputation for Violence vars
hh_data <- dplyr::mutate(hh_data, dplyr::across(
  dplyr::matches("unsafe_locations_why.+"),
  ~ dplyr::case_when(
    women_feel_unsafe %in% c("decline_to_answer", "do_not_know") ~ "0",
     women_feel_unsafe == "no" ~ "0",
    TRUE ~ as.character(.)
  )
))

# Missing values imputation for Health vars
hh_data <- dplyr::mutate(hh_data, dplyr::across(
  dplyr::matches("unsafe_locations_why.+"),
  ~ dplyr::case_when(
    health_accessed %in% c("no", "do_not_know") ~ "0",
     health_barriers %in% c("no", "do_not_know", "") ~ "0",
    TRUE ~ as.character(.)
  )
))

# Missing values imputation for solid waste
hh_data <- dplyr::mutate(hh_data, solid_waste_disposal = dplyr::case_when(
    solid_waste_disposal == "do_not_know" ~ NA_character_,
    TRUE ~ solid_waste_disposal
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("solid_waste_disposal"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)

# Missing values imputation for Health
hh_data <- dplyr::mutate(hh_data, health_barriers = dplyr::case_when(
    health_barriers %in% c("decline_to_answer", "do_not_know") ~ NA_character_,
    health_barriers == "" ~ "no",
    TRUE ~ health_barriers
  ))
hh_data <- VIM::hotdeck(
  hh_data, c("health_barriers"),
  domain_var = c("strata", "locality_code"),
  ord_var = c("gender_hhh", "refugee_status", "working_adults", "tot_children",
  "primary_livelihood")
)
