# Demography indicators ####

## Location
i_location <- dplyr::select(
  hh_data, uuid, strata_cat = strata, oslo_cat = oslo_area,
  westbank_cat = governorate_wb, gaza_cat = locality_gaza, region_cat = region,
  weights
) |>
  dplyr::mutate(
    oslo_cat = dplyr::case_when(
      strata_cat == "east_jerusalem" ~ "east_jerusalem",
      strata_cat == "h2" ~ "hebron_h2",
      TRUE ~ oslo_cat
    ),
    westbank_cat = dplyr::case_when(
      strata_cat == "east_jerusalem" ~ "jerusalem_2",
      strata_cat == "h2" ~ "hebron",
      TRUE ~ westbank_cat
    ),
    region_cat = dplyr::case_when(
      region_cat == "ej" ~ "east_jerusalem",
      region_cat == "H2" ~ "hebron_h2",
      TRUE ~ region_cat
    ),
    area_cat = dplyr::case_when(
      strata_cat == "east_jerusalem" ~ "east_jerusalem",
      strata_cat == "h2" ~ "hebron_h2",
      strata_cat == "camps_wb" ~ "camps_wb",
      grepl(".+_c$", strata_cat) ~ "area_c",
      TRUE ~ strata_cat
    ),
    strata_cat = dplyr::if_else(strata_cat == "h2", "hebron_h2", strata_cat),
    weights = dplyr::if_else(is.na(weights), 1, weights)
  )

## Children in the household
i_children <- dplyr::select(hh_data, uuid, tot_children) |>
  dplyr::mutate(children_bin = dplyr::if_else(tot_children > 0, 1, 0)) |>
  dplyr::rename(children_num = tot_children)
# hh_data[which(is.na(i_children$children_bin)), ] # Check NA # nolint

## Gender of the head of household ####
i_hoh_gender <- hh_data |>
  dplyr::mutate(
    hoh_gender_cat = dplyr::if_else(hhh == "yes", gender_respondent, gender_hhh),
    hoh_female_bin = dplyr::if_else(hoh_gender_cat == "female", 1, 0)
  ) |>
  dplyr::select(uuid, hoh_gender_cat, hoh_female_bin)
# hh_data[which(is.na(i_hoh_gender$hoh_gender_cat)), ] # Check NA

## Refugee status ----
i_refugee <- hh_data[, c("uuid", "refugee_status")]
i_refugee$refugee_status <- ifelse(i_refugee$refugee_status == "yes", 1, 0)
colnames(i_refugee) <- c("uuid", "refugee_status_bin")
# hh_data[which(is.na(i_refugee$refugee_status_bin)), ] # Check NA


# Persons with disability
i_disability <- dplyr::select(hh_data, uuid, presence_disability) |>
  dplyr::mutate(disability_bin = dplyr::if_else(presence_disability == "none", 0, 1)) |>
  dplyr::select(uuid, disability_bin)
#i_disability[which(is.na(i_disability$disability_bin)), ] # Check NA

# Persons with distress ----
i_distress <- dplyr::select(hh_data, uuid, child_distress_number, adult_distress_number) |>
  tidyr::pivot_longer(-uuid) |>
  dplyr::group_by(uuid) |>
  dplyr::summarise(distress_num = sum(value, na.rm = TRUE))

i_distress <- dplyr::mutate(
  i_distress, distress_cat = dplyr::case_when(
    distress_num  == 0 ~ "distress_0",
    distress_num %in% 1 ~ "distress_1",
    distress_num %in% 2:3 ~ "distress_2_3",
    distress_num >= 3 ~ "distress_4"
  ),
  distress_bin = dplyr::if_else(distress_num > 0, 1, 0)
)
# i_distress[which(is.na(i_distress$distress_bin)), ] # Check NA


# Livelihood Indicators ----
## Household poverty ----
poor_60 <- median(hh_data$tot_income, na.rm = TRUE) * 0.6 
poor_30 <- median(hh_data$tot_income, na.rm = TRUE) * 0.3

i_poverty <- dplyr::select(hh_data, uuid, tot_income) |>
  dplyr::mutate(
    poverty_60_bin = dplyr::if_else(tot_income <= poor_60, 1, 0),
    poverty_30_bin = dplyr::if_else(tot_income <= poor_30, 1, 0),
    poverty_cat = dplyr::case_when(
      tot_income <= poor_30 ~ "very_poor",
      tot_income > poor_30 & tot_income <= poor_60 ~ "poor",
      tot_income > poor_60 ~ "not_poor",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(uuid, poverty_30_bin, poverty_60_bin, poverty_cat)
 #table(i_poverty$poverty_cat, useNA = "always")


# Child Protection ----
## Child marriage ----
i_child_marriage <- dplyr::select(
  hh_data, uuid, tot_children, coping_forced_marriage,
  drop_out_reason.child_marriage, non_enrollment_reasons.child_marriage
) |>
  dplyr::mutate(
    child_marriage_bin = dplyr::case_when(
      tot_children == 0 ~ 0, # Remove NA if no children, no child marriage...
      coping_forced_marriage %in% c("no_already_did", "yes") ~ 1,
      drop_out_reason.child_marriage == 1 ~ 1,
      non_enrollment_reasons.child_marriage == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  dplyr::select(uuid, child_marriage_bin)
# i_child_marriage[which(is.na(i_child_marriage$child_marriage_bin)), ] # Check NA
## Child labour ----
# We refer to children < 18, not necesarily child labouras per legal def
# Direct referal of children employed 
child_employ <- dplyr::select(
  hh_data, uuid, tot_children, dplyr::matches("under_18_working")
) |>
  dplyr::mutate(
    dplyr::across(c(dplyr::matches("boys|girls")), ~tidyr::replace_na(. , 0)),
    child_employ_rat = (under_18_working_boys + under_18_working_girls) / tot_children,
    child_employ_rat = dplyr::if_else(
      under_18_working == "decline_to_answer", NA_real_, child_employ_rat
    )
  ) |>
  dplyr::select(uuid, child_employ_rat)

# Indirect referal of children working
child_work <- dplyr::select(
  hh_data, uuid, tot_children, coping_child_labour, income_earner,
  drop_out_reason.child_labour, non_enrollment_reasons.child_labour
) |>
  dplyr::mutate(
    child_work_bin = dplyr::case_when(
      tot_children == 0 ~ 0, # Remove NA if no children, no child labour...
      coping_child_labour %in% c("no_already_did", "yes") ~ 1,
      grepl("_child_", income_earner) ~ 1,
      drop_out_reason.child_labour == 1 ~ 1,
      non_enrollment_reasons.child_labour == 1 ~ 1,
      TRUE ~ 0
    )
  ) |>
  dplyr::select(uuid, child_work_bin)
# Joining direct and indiretc referal of child labour
i_child_labour <- dplyr::full_join(child_employ, child_work, by = "uuid") |>
  dplyr::mutate(
    child_labour_bin = dplyr::case_when(
      child_employ_rat > 0 ~ 1, child_work_bin > 0 ~ 1, TRUE ~ 0 # Remove NA if no children...
    )
  )
# i_child_labour[which(is.na(i_child_labour$child_labour_bin)), ] # Check NA

# Food Security Indicators ####
## Food Insecurity Experience Scale (FIES) ----
i_fies <- dplyr::select(
  hh_data, uuid,
  lack_enough_food, lack_healthy_food, lack_food_variety, skipped_meals,
  ate_less, out_of_food, hungry_ddnt_eat, day_without_eating
) |>
  dplyr::mutate(
    dplyr::across(-uuid, ~ dplyr::if_else(. == "yes", 1, 0)),
  ) %>% # R base pipe does not recognise `.`
  dplyr::mutate(fies_raw_score = rowSums(.[2:9]), .before = 2)

# Calcualte standard score
m_fies <- RM.weights::RM.w(i_fies[3:ncol(i_fies)])
fies_sc <- data.frame(fies_raw_score = 0:8, fies_std_score = m_fies$a)
# Calculate probabilities of FIES sever/moderate
p_fies <- RM.weights::equating.fun(m_fies, iterative = FALSE)
fies_probs <- data.frame(
  fies_raw_score = 0:8,
  fies_mod_rat = p_fies$probs.rs[, 1], fies_sev_rat = p_fies$probs.rs[, 2]
)
# Join all together
i_fies <- dplyr::select(i_fies, uuid, fies_raw_score) |>
dplyr::left_join(fies_sc, by = "fies_raw_score") |>
dplyr::left_join(fies_probs, by = "fies_raw_score")

i_fies <- dplyr::mutate(
  i_fies, fies_mod_bin = dplyr::if_else(fies_mod_rat > 0.5, 1, 0)
)
# i_fcs[which(is.na(i_fcs$fies_mod_bin)), ] # Check NA

## Livelihood Coping Strategies Index ----
# Subset and pivot data
i_lcs <- dplyr::select(
  hh_data, uuid,
  coping_selling_properties, coping_food_credit, coping_selling_tranport,
  coping_children_dropout, coping_reducing_expenditure, coping_changing_residency,
  coping_risky_behaviour, coping_child_labour, coping_migration,
  coping_forced_marriage, coping_selling_animals, coping_less_expensive_school
) |>
  tidyr::pivot_longer(-uuid, names_to = "strategy")

# Tranform to dummy and caetgories
i_lcs <- dplyr::mutate(
  i_lcs,
   value = dplyr::case_when(
    value %in% c("yes", "no_already_did") ~ 1,
    grepl("not_applicable.*|(no|on)_one", value) ~ 0
  ),
  cat = dplyr::case_when(
    strategy %in% c(
      "coping_selling_properties", "coping_food_credit",
      "coping_less_expensive_school", "coping_changing_residency"
    ) ~ 1,
    strategy %in% c(
      "coping_selling_animals", "coping_children_dropout",
      "coping_selling_tranport", "coping_reducing_expenditure"
    ) ~ 2,
    strategy %in% c(
      "coping_risky_behaviour", "coping_migration",
      "coping_child_labour", "coping_forced_marriage"
    ) ~ 3
  )
)
# Compute score and caetgories
i_lcs <- dplyr::mutate(i_lcs, lcs_score = value * cat) |>
  dplyr::group_by(uuid) |>
  dplyr::summarise(lcs_score = max(lcs_score)) |>
  dplyr::mutate(
    lcs_cat = dplyr::case_when(
      lcs_score == 0 ~ "None", lcs_score == 1 ~ "stress",
      lcs_score == 2 ~ "crisis", lcs_score == 3 ~ "emergency"
    ),
    crisis_cope_bin = dplyr::case_when(
      lcs_score %in% 0 ~ "0", # none+stress
      lcs_score %in% 1:3 ~ "1"# crisis+emergency
    )
  )
# i_lcs[which(is.na(i_lcs$crisis_cope_bin)), ] # Check NA


# Health Indicators ----
## Barriers ot services
health_bar_quality <- c(
  "civil_docs_problems", # Problems with civil documents
  "refused_treatment", # Medical staff refused treatment without any explanation
  "not_inclusive", # Health services not inclusive of people with disabilities
  "no_fem_staff", # Lack of female health staff
  "no_referral_phc", # Public health clinic or hospital did not provide referral
  "phc_closed", # Public health clinic not open
  "unqualified_staff", # Did not get access to qualified health staff at the health facility
  "no_medicine", # No medicine available at health facility / pharmacy
  "no_offered_treatment", # No treatment available for my disease at the health facility
  "quality_of_services" # Quality of services could not meet the needs
)
health_bar_access <- c(
  "cost", # Cost of services and/or medicine was too high
  "distance_to_treatmentcenter", # The treatment center was too far / Transportation constraints
  # "lack_of_awareness" #"Awareness of how/where to access services" # REMOVED
  "refugee_status", # Do not have appropriate refugee status (e.g. refugee or non-refugee)
  "authorities_denied_request", # Authorities refused to facilitate transfer
  #"family_imposed_barrier", # Socially/family-imposed barrier # REMOVED
  "movement_to_facility_restricted" # Movement to or from the healthcare facility was restricted
)
# List of barriers
heath_bar_vars <- list(
  health_bar_quality2_ = health_bar_quality,
  health_bar_access2_ = health_bar_access
)
# Subset relevant vars and calculate num and bin indicatorsfor each group
i_health_bar <- purrr::imap(
  heath_bar_vars, ~{
    df <- dplyr::select(
      hh_data, uuid, dplyr::matches(paste0("type_health.+", .x, collapse = "$|"))
    )
    df <-  tidyr::pivot_longer(df, -1, ) |>
      dplyr::group_by(uuid) |>
      dplyr::summarise(!!paste0(.y, "num") := sum(value, na.rm = TRUE)) |>
      dplyr::mutate(!!paste0(.y, "bin") := dplyr::if_else(
        .data[[paste0(.y, "num")]] > 0, 1, 0
      ))
  }
)
# Join df with indicator for eahc barrier category
i_health_bar <- Reduce(dplyr::left_join, i_health_bar)
# i_health_bar[which(is.na(i_health_bar$health_bar_access_num)), ] # Check NA


# WASH Indicators ----
## Solid waste management ----
i_solid_waste <- dplyr::select(hh_data, uuid, solid_waste_disposal) |>
  dplyr::mutate(unsafe_solid_waste_bin = dplyr::case_when(
    solid_waste_disposal == "municipality_waste_system" ~ 0,
    solid_waste_disposal == "do_not_know" ~ NA_real_,
    TRUE ~ 1
  )) |>
  dplyr::select(uuid, unsafe_solid_waste_bin)
# table(i_solid_waste$unsafe_solid_waste_bin, useNA = "always")

## Latrine waste management ----
i_latrine_waste <- dplyr::select(hh_data, uuid, latrine_waste_drainage) |>
  dplyr::mutate(unsafe_latrine_waste_bin = dplyr::case_when(
    latrine_waste_drainage %in% c("sewage_system", "covered_septic") ~ 0,
    latrine_waste_drainage == "do_not_know" ~ NA_real_,
    TRUE ~ 1
  )) |>
  dplyr::select(uuid, unsafe_latrine_waste_bin)
# table(i_latrine_waste$unsafe_latrine_waste_bin, useNA = "always")

## Join indicator solid and latrine waste
i_waste <- dplyr::full_join(i_solid_waste, i_latrine_waste, by = "uuid") |>
  dplyr::mutate(unsafe_waste_bin = dplyr::if_else(
    unsafe_solid_waste_bin == 1 | unsafe_latrine_waste_bin == 1, 1, 0
  )) |>
  dplyr::select(uuid, unsafe_waste_bin)
# table(i_waste$unsafe_waste_bin, useNA = "always")

# Shelter Indicators ----
## Risk of eviction ----
i_risk_evict <- hh_data |>
  dplyr::rename("risk_eviction_bin" = hh_risk_eviction) |>
  dplyr::mutate(risk_eviction_bin = dplyr::case_when(
  risk_eviction_bin == "yes" ~ 1, risk_eviction_bin == "no" ~ 0, TRUE ~ NA_real_
)) |>
  dplyr::select(uuid, risk_eviction_bin)
# i_risk_evict[which(is.na(i_risk_evict$risk_eviction_bin)), ] # Check NA

## Demolition Order recieved -----
i_demolition <- dplyr::select(hh_data, uuid, demolition_order_wb) |>
  #dplyr::filter(governorate_wb != "") |>
  dplyr::mutate(demolition_order_bin = dplyr::case_when(
    demolition_order_wb == "yes" ~ 1, demolition_order_wb == "no" ~ 0,
    demolition_order_wb %in% c("decline_to_answer", "do_not_know") ~ 0, # Imputed to 0, but better ifwe impute with VIM
  )) |>
  dplyr::select(uuid, demolition_order_bin)
# table(i_demolition$demolition_order_bin, useNA = "always")

# Education Indicators ---
## School attendance safety ----
i_school_unsafe <- dplyr::select(hh_data, uuid, tot_children, school_safety) |>
  dplyr::mutate(school_unsafe_bin = dplyr::case_when(
    #tot_children == 0 ~ 0, # Remove NA if no children, no child labour...
    school_safety %in% c("safe", "very_safe") ~ 0,
    school_safety %in% c("unsafe", "very_unsafe", "neither_unsafe_safe") ~ 1,
    TRUE ~  NA_real_
  )) |>
  dplyr::select(uuid, school_unsafe_bin)

## Education attendance barriers ---
# Select relevant barriers
edu_att_bar_safety <- c(
  "commuting_not_safe_girls", # Going to or attending school is not safe for girls
  "commuting_not_safe_boys" # Going to or attending school is not safe for boys
)
edu_att_bar_quality <- c(
  "overcrowed_schools", # School and classes are overcrowded
  "lack_of_staff", # Lack of staff to run the school
  "poor_infrastructure" # The school infrastructure is poor
)
edu_att_bar_access <- c(
  "lack_fees", # We can’t afford to pay for the school related expenses
  "lack_of_schools", # Lack of available schools in the area
  "cannot_physically_go", # Our children can’t physically go to the school 
    "curriculum_issues", # The curriculum and teaching are not adapted for our children
  "unable_to_enroll"# We are not able to register or enrol our children in the school
)
# List of barriers
edu_att_bar_vars <- list(
  edu_att_bar_safety_ = edu_att_bar_safety,
  edu_att_bar_quality_ = edu_att_bar_quality,
  edu_att_bar_access_ = edu_att_bar_access
)
# Subset relevant vars and calculate num and bin indicatorsfor each group
i_edu_att_bar <- purrr::imap(
  edu_att_bar_vars, ~{
    df <- dplyr::select(
      hh_data, uuid, dplyr::matches(paste0("edu_barriers.+", .x, collapse = "$|"))
    )

    df <- tidyr::pivot_longer(df, -1) |>
      dplyr::group_by(uuid) |>
      dplyr::summarise(!!paste0(.y, "num") := sum(value, na.rm = TRUE)) |>
      dplyr::mutate(!!paste0(.y, "bin") := dplyr::if_else(
        .data[[paste0(.y, "num")]] > 0, 1, 0
      ))
  }
)
# Join df with indicator for eahc barrier category
i_edu_att_bar <- Reduce(dplyr::left_join, i_edu_att_bar)
# Add NA cases when no children
i_edu_att_bar <- dplyr::full_join(
  i_edu_att_bar, hh_data[, c("uuid", "tot_children")], by = "uuid"
) |>
  dplyr::mutate(dplyr::across(dplyr::matches("^edu_"), ~ dplyr::if_else(
    tot_children == 0, 0, as.numeric(.) # Remove NA if no children, no child labour...,
  ))) |>
  dplyr::select(-tot_children)

## Education enrolment barriers ----
# Select relevant barriers
edu_enrol_bar_safety <- c(
  "bullying", # The curriculum and teaching are not adapted for our children
  "school_unsafe", # Going to or attending school is not safe for girls
  "commuting_not_safe_boys" # Going to or attending school is not safe for boys
)
edu_enrol_bar_quality <- c(
  "school_overcrowded", # School and classes are overcrowded
  "corporal_punishment", # Lack of staff to run the school
  "school_unhygienic" # The school infrastructure is poor
)
edu_enrol_bar_access <- c(
  "cannot_afford", # We can’t afford to pay for the school related expenses
  "school_closed", # Lack of available schools in the area
  "cannot_physically_go", # Our children can’t physically go to the school
    "not_adapted_child_disability"# We are not able to register or enrol our children in the school 
)
# List of barriers
edu_enrol_bar_vars <- list(
  edu_enrol_bar_safety_ = edu_enrol_bar_safety,
  edu_enrol_bar_quality_ = edu_enrol_bar_quality,
  edu_enrol_bar_access_ = edu_enrol_bar_access
)
# Subset relevant vars and calculate num and bin indicatorsfor each group
i_edu_enrol_bar <- purrr::imap(
  edu_enrol_bar_vars, ~{
    df <- dplyr::select(
      hh_data, uuid,
      dplyr::matches(paste0("non_enrollment_reasons.+", .x, collapse = "$|"))
    )

    df <- tidyr::pivot_longer(df, -1) |>
      dplyr::group_by(uuid) |>
      dplyr::summarise(!!paste0(.y, "num") := sum(value, na.rm = TRUE)) |>
      dplyr::mutate(!!paste0(.y, "bin") := dplyr::if_else(
        .data[[paste0(.y, "num")]] > 0, 1, 0
      ))
  }
)
# Join df with indicator for eahc barrier category
i_edu_enrol_bar <- Reduce(dplyr::left_join, i_edu_enrol_bar)
# Add NA cases when no children
i_edu_enrol_bar <- dplyr::full_join(
  i_edu_enrol_bar, hh_data[, c("uuid", "tot_children")], by = "uuid"
) |>
  dplyr::mutate(dplyr::across(dplyr::matches("^edu_"), ~ dplyr::if_else(
    tot_children == 0, 0, as.numeric(.) # Remove NA if no children, no child labour...,
  ))) |>
  dplyr::select(-tot_children)

## Educational barriers ----
# Join and combine values for attendance and enrollment barriers
i_edu_bar <- dplyr::full_join(i_edu_att_bar, i_edu_enrol_bar, by = "uuid") |>
  tidyr::pivot_longer(-uuid) |>
  tidyr::separate(name, c("a", "b", "c", "barrier", "type"), sep = "_") |>
  dplyr::group_by(uuid, barrier, type) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  tidyr::unite("barrier", barrier, type) |>
  dplyr::mutate(value = dplyr::if_else(
    grepl("bin", barrier) & value > 0, 1, value)
  ) |>
  tidyr::pivot_wider(names_from = barrier, values_from = value)
# Add NA cases when no children
i_edu_bar <- dplyr::full_join(
  i_edu_bar, hh_data[, c("uuid", "tot_children")], by = "uuid"
) |>
  dplyr::mutate(dplyr::across(dplyr::matches("num|bin"), ~ dplyr::if_else(
    tot_children == 0, 0, as.numeric(.) # Remove NA if no children, no child labour...,
  ))) |>
  dplyr::select(-tot_children)
# Add thematic prefix to colnames
i_edu_bar <- dplyr::rename_with(i_edu_bar, ~paste0("educa_bar_", .), -uuid)


# General safety perception ----
# Security concerns 
viol_exploit <- c(
  "exploitation",
  "sexual_exploitation",
  "discrimination",
  "social_stigma"
)
viol_harrasment <- c(
  "physcial_harassment",
  "sexual_harrassment",
  "verbal_harrassment"
)
viol_physical <- c(
  "kidnapped",
  "tear_gas",
  "detention",
  "threatened_with_violence",
  "robbed" #Fear of being robbed
)
# List of barriers
viol_vars <- list(
  viol_exploit_ = viol_exploit,
  viol_harrasment_ = viol_harrasment,
  viol_physical2_ = viol_physical
)
# Subset relevant vars and calculate num and bin indicatorsfor each group
i_violence <- purrr::imap(
  viol_vars, ~{
    df <- dplyr::select(
      hh_data, uuid,
      dplyr::matches(paste0("unsafe_locations_why.+", .x, collapse = "$|"))
    )

    df <- tidyr::pivot_longer(df, -1) |>
      dplyr::mutate(value = utils::type.convert(value, as.is = TRUE)) |>
      dplyr::group_by(uuid) |>
      dplyr::summarise(!!paste0(.y, "num") := sum(value, na.rm = TRUE)) |>
      dplyr::mutate(!!paste0(.y, "bin") := dplyr::if_else(
        .data[[paste0(.y, "num")]] > 0, 1, 0
      ))
  }
)
# Join df with indicator for eahc barrier category
i_violence <- Reduce(dplyr::left_join, i_violence)
# sapply(i_violence[, 2:7], table, useNA= "always") # Check NA

## Unique violence binary variable
i_violence_one <- dplyr::mutate(i_violence, 
  viol_num = rowSums(i_violence[grepl("_bin$", colnames(i_violence))], na.rm = TRUE),
  viol_one = dplyr::if_else(viol_num == 1, 1, 0),
  viol_two = dplyr::if_else(viol_num > 1, 1, 0),
  viol_bin = dplyr::if_else(viol_num > 0, 1, 0)
)

# Settler violence ----
i_settler <- dplyr::select(hh_data, uuid, settler_threats_wb) |>
  dplyr::mutate(settler_bin = dplyr::case_when(
    settler_threats_wb == "yes" ~ 1, settler_threats_wb == "no" ~ 0
  )) |>
  dplyr::select(uuid, settler_bin)
#table(i_settler$settler_bin, useNA = "always")

# Affected by Flooding ----
i_flood <- dplyr::select(hh_data, uuid, flood_num =num_of_floods) |>
    dplyr::mutate(
      flood_cat = dplyr::case_when(
        flood_num == 0 ~ "none",
        flood_num %in% 1:3 ~ "times_1_3",
        flood_num >= 4 ~ "times_4_more",
      ),
      flood_bin = dplyr::if_else(flood_num > 0, 1, 0)
    )
 # i_flood[which(is.na(i_flood$flood_bin)), ] # Check NA

# Assistance ----
## Aid recieved ----
i_aid_recieved <- hh_data |>
  dplyr::rename("aid_received_bin" = aid_received) |>
  dplyr::mutate(aid_received_bin = dplyr::case_when(
  aid_received_bin == "yes" ~ 1, aid_received_bin == "no" ~ 0, TRUE ~ NA_real_
)) |>
  dplyr::select(uuid, aid_received_bin)

## Know complaint mechanism ----
i_complaint <- dplyr::select(hh_data, uuid, complaint_mechanisms) |>
  dplyr::mutate(complaint_mechanisms_bin = dplyr::if_else(
    complaint_mechanisms == "yes", 1, 0
  )) |>
  dplyr::select(uuid, complaint_mechanisms_bin)
# table(i_complaint$complaint_mechanisms_bin, useNA="always")

## Aid satisfaction
i_satisfaction <- dplyr::select(hh_data, uuid, aid_satisfaction) |>
  dplyr::mutate(aid_satisfaction_bin = dplyr::if_else(
    aid_satisfaction == "yes", 1, 0
  )) |>
  dplyr::select(uuid, aid_satisfaction_bin)
 table(i_satisfaction$aid_satisfaction_bin, useNA="always")

# Shelter -----
## Sehlter type
i_shelter_type <- dplyr::select(hh_data, uuid, shelter_type) |>
  dplyr::mutate(unsafe_shelter_bin = dplyr::case_when(
    shelter_type %in% c("apartment", "house") ~ "0",
    TRUE ~ "1"
  ))|>
  dplyr::select(uuid, unsafe_shelter_bin)
# table(i_shelter_type$durable_shelter_bin, useNA = "always")

## Sehlter issues
i_shelter_issues <- dplyr::select(hh_data, uuid, shelter_issues.none) |>
  dplyr::mutate(shelter_issues_bin = abs(shelter_issues.none - 1)) |> # reversed
  dplyr::select(uuid, shelter_issues_bin)
# table(i_shelter_issues$shelter_issues_bin, useNA = "always")


## Suplementary indicators

## Children in the household
i_children <- dplyr::select(hh_data, uuid, tot_children) |>
  dplyr::mutate(children_bin = dplyr::if_else(tot_children > 0, 1, 0)) |>
  dplyr::rename(children_num = tot_children)

## Household indebted ----
i_debt <- dplyr::select(hh_data, uuid, how_much_debt) |>
  dplyr::mutate(hh_debt_bin = dplyr::if_else(how_much_debt > 0, 1, 0)) |>
  dplyr::select(uuid, hh_debt_bin)
# i_debt[which(is.na(i_debt$hh_debt_bin)), ] # Check NA

impute_median <- median(hh_data$distance_hospital, na.rm = TRUE)

i_dist_hosp <- dplyr::select(hh_data, uuid, distance_hospital) |>
  dplyr::mutate(
    distance_hospital_rat= (distance_hospital - min(distance_hospital, na.rm = TRUE)) /
      (max(distance_hospital, na.rm = TRUE) - min(distance_hospital, na.rm = TRUE)),
    distance_hospital_cat = dplyr::case_when(
      distance_hospital < 5 ~ "less_than_5km",
      distance_hospital >= 5 & distance_hospital < 10  ~ "between_5_10km",
      distance_hospital >= 10 & distance_hospital < 15  ~ "between_10_15km",
      distance_hospital >= 15 ~ "more_than_15km",
    ),
    distance_hospital_bin = dplyr::case_when(
      distance_hospital <= 10 ~ as.numeric(1),
      is.na(distance_hospital) ~ as.numeric(impute_median),
      TRUE ~ as.numeric(0)
    )
  ) |>
  dplyr::rename("distance_hospital_num" = distance_hospital)


## % in education ----
i_sch_enrol <- dplyr::select(
  hh_data, uuid, tot_children, total_enrolled_children
) |>
  dplyr::mutate(
    sch_enrol_rat = total_enrolled_children / tot_children,
    sch_enrol_rat = dplyr::if_else(sch_enrol_rat > 1, 1, sch_enrol_rat),
    all_enrol_bin = dplyr::case_when(
      sch_enrol_rat == 1 ~ 1, is.na(sch_enrol_rat) ~ 0, TRUE ~ 0)
  ) |>
  dplyr::select(uuid, sch_enrol_rat, all_enrol_bin)

## Improved water sources ----
i_water_src <- dplyr::select(hh_data, uuid, drinking_water_source) |>
  dplyr::mutate(safe_water_bin = dplyr::case_when(
    drinking_water_source %in% c(
      "network_private", "network_comm", "borehole", "prot_well",
      "prot_spring", "prot_tank", "bottled_water", "water_trucking"
    ) ~ 1,
    drinking_water_source %in% c(
      "illegal_connection", "surface_water", "unprot_well", "unprot_spring",
      "other", "do_not_know", "decline_to_answer"
    ) ~ 0,
    TRUE ~ NA_real_
  )) |>
  dplyr::select(uuid, safe_water_bin)
# i_water_src[which(is.na(i_water_src$water_source_bin)), ] # Check NA


# Join indicators to data
indicators_data <- lapply(ls(pattern="^i_.+$"), get)
names(indicators_data) <- ls(pattern="^i_.+$")
indicators_data <- Reduce(indicators_data, f = dplyr::full_join)

indicators_data <- dplyr::mutate(indicators_data,
    dplyr::across(dplyr::matches("_(num|cat|bin)$"), ~ factor(.)),
    dplyr::across(dplyr::matches("_(rat)$"), ~ as.numeric(.)),
  )

# Remove indicators with missing values
keep <- c("indicators_data", "hh_data", "q_data", # import data
 "adm2", "oslo", "comm_wb", "cluster", "admins", #import gis
 "format_tbl", "fmat_num", "fmat_per", "tidyr_res", # import fun fmt
 "benz_adj", "p_cont_cos", "p_vtest", # import fun plot
 "ind_miss_per_wide", "desc_i") # miss_analysis
rm(list = ls()[!(ls() %in% keep)])
gc()
