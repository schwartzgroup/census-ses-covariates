# Formula interface for Census data with modular data retrieval

library(dplyr)
library(rlang)
library(totalcensus)

cache_dir = "cache"

drivers <- c(
  acs5 = totalcensus::read_acs5year,
  acs1 = totalcensus::read_acs1year,
  decennial = totalcensus::read_decennial
)

query_census <- function(formulas,
                         driver = "acs5",
                         ...
                         ) {
  # extract all variables from the right-hand sides of all formulas
  needed_variables <- unique(unlist(sapply(
    formulas,
    function(f) all.vars(f_rhs(f))
  )))
  
  # query for all the needed variables
  data <- drivers[[driver]](
    table_contents = needed_variables,
    show_progress = FALSE,
    ...
  )
  
  # extract the geoids column
  geoids <- list(geoid = gsub(".*US", "", data$GEOID))
  
  # create new columns using the right-hand sides of the formulas and assign to the variable name from the left-hand side
  columns <- lapply(
    formulas,
    function(.formula) {
      transmute(data, !!f_lhs(.formula) := !!f_rhs(.formula))
    }
  )
  
  # bind all the columns into a data frame
  return(do.call(cbind, c(
    year = list(...)[["year"]],
    geoids,
    columns
  )))
}

# demo / testing ---------------------------------------------------------

query_census(
  c(
    race_total ~ B03002_001,
    pct_age_21 ~ (B01001_009 + B01001_033) / B01001_001,
    ethnic_fractionalization ~ 1 - (
      (B03002_003 / B01001_001)^2 + (B03002_004 / B01001_001)^2 +
      (B03002_006 / B01001_001)^2 + (B03002_012 / B01001_001)
    )
  ),
  year = 2012,
  states = c("MA"),
  summary_level = "tract"
)

query_census(
  c(
    race_total ~ B03002_001,
    pct_age_21 ~ (B01001_009 + B01001_033) / B01001_001,
    ethnic_fractionalization ~ 1 - (
      (B03002_003 / B01001_001)^2 + (B03002_004 / B01001_001)^2 +
      (B03002_006 / B01001_001)^2 + (B03002_012 / B01001_001)
    )
  ),
  year = 2019,
  states = "US",
  geo_headers = "ZCTA5",
  summary_level = "860"
)

query_census(
  c(
    pct_age_under_5 ~ (B01001_003 + B01001_027) / B01001_001,
    pct_age_5_to_9 ~ (B01001_004 + B01001_028) / B01001_001,
    pct_age_10_to_14 ~ (B01001_005 + B01001_029) / B01001_001,
    pct_age_15_to_17 ~ (B01001_006 + B01001_030) / B01001_001,
    pct_age_18_to_19 ~ (B01001_007 + B01001_031) / B01001_001,
    pct_age_20 ~ (B01001_008 + B01001_032) / B01001_001,
    pct_age_21 ~ (B01001_009 + B01001_033) / B01001_001,
    pct_age_22_to_24 ~ (B01001_010 + B01001_034) / B01001_001,
    pct_age_25_to_29 ~ (B01001_011 + B01001_035) / B01001_001,
    pct_age_30_to_34 ~ (B01001_012 + B01001_036) / B01001_001,
    pct_age_35_to_39 ~ (B01001_013 + B01001_037) / B01001_001,
    pct_age_40_to_44 ~ (B01001_014 + B01001_038) / B01001_001,
    pct_age_45_to_49 ~ (B01001_015 + B01001_039) / B01001_001,
    pct_age_50_to_54 ~ (B01001_016 + B01001_040) / B01001_001,
    pct_age_55_to_59 ~ (B01001_017 + B01001_041) / B01001_001,
    pct_age_60_to_61 ~ (B01001_018 + B01001_042) / B01001_001,
    pct_age_62_to_64 ~ (B01001_019 + B01001_043) / B01001_001,
    pct_age_65_to_66 ~ (B01001_020 + B01001_044) / B01001_001,
    pct_age_67_to_69 ~ (B01001_021 + B01001_045) / B01001_001,
    pct_age_70_to_74 ~ (B01001_022 + B01001_046) / B01001_001,
    pct_age_75_to_79 ~ (B01001_023 + B01001_047) / B01001_001,
    pct_age_80_to_84 ~ (B01001_024 + B01001_048) / B01001_001,
    pct_age_over_85 ~ (B01001_025 + B01001_049) / B01001_001
  ),
  year = 2019,
  states = "MA",
  summary_level = "tract"
)

query_census(
  c(
    pct_age_under_5 ~ (B01001_003 + B01001_027) / B01001_001,
    pct_age_5_to_9 ~ (B01001_004 + B01001_028) / B01001_001,
    pct_age_10_to_14 ~ (B01001_005 + B01001_029) / B01001_001,
    pct_age_15_to_17 ~ (B01001_006 + B01001_030) / B01001_001,
    pct_age_18_to_19 ~ (B01001_007 + B01001_031) / B01001_001,
    pct_age_20 ~ (B01001_008 + B01001_032) / B01001_001,
    pct_age_21 ~ (B01001_009 + B01001_033) / B01001_001,
    pct_age_22_to_24 ~ (B01001_010 + B01001_034) / B01001_001,
    pct_age_25_to_29 ~ (B01001_011 + B01001_035) / B01001_001,
    pct_age_30_to_34 ~ (B01001_012 + B01001_036) / B01001_001,
    pct_age_35_to_39 ~ (B01001_013 + B01001_037) / B01001_001,
    pct_age_40_to_44 ~ (B01001_014 + B01001_038) / B01001_001,
    pct_age_45_to_49 ~ (B01001_015 + B01001_039) / B01001_001,
    pct_age_50_to_54 ~ (B01001_016 + B01001_040) / B01001_001,
    pct_age_55_to_59 ~ (B01001_017 + B01001_041) / B01001_001,
    pct_age_60_to_61 ~ (B01001_018 + B01001_042) / B01001_001,
    pct_age_62_to_64 ~ (B01001_019 + B01001_043) / B01001_001,
    pct_age_65_to_66 ~ (B01001_020 + B01001_044) / B01001_001,
    pct_age_67_to_69 ~ (B01001_021 + B01001_045) / B01001_001,
    pct_age_70_to_74 ~ (B01001_022 + B01001_046) / B01001_001,
    pct_age_75_to_79 ~ (B01001_023 + B01001_047) / B01001_001,
    pct_age_80_to_84 ~ (B01001_024 + B01001_048) / B01001_001,
    pct_age_over_85 ~ (B01001_025 + B01001_049) / B01001_001
  ),
  year = 2011,
  states = "US",
  geo_headers = "ZCTA5",
  summary_level = "860"
)

query_census(
  formulas = c(
    gini_index ~ B19083_001,
    med_household_income ~ B19013_001,
    ethnic_fractionalization ~ 1 - (
      (B03002_003 / B01001_001)^2 + (B03002_004 / B01001_001)^2 +
      (B03002_006 / B01001_001)^2 + (B03002_012 / B01001_001)
    ),
    ice_race_ethnicity ~ (B03002_003 - B03002_004) / B01001_001
  ),
  year = 2018,
  states = c("MA"),
  summary_level = "tract"
)
