# Census SES covariate generator

library(data.table)
library(dplyr)
library(readxl)
library(stringr)
library(totalcensus)
library(tidyr)

states <- states_DC

output_directory <- file.path("outputs", "tables")

years <- c(2009:2019)

# TODO: add ability to put `zip code` here
#
# need to do some extra work because it's only available for `US` and not
# individual states, and `zip code` is not a proper summary_level. see `Start
# loops` section for workaround - need to adjust read_acs5 args
#
# temporary workaround: set states <- c("US"), geometries <- c("zip code")
geometries <- c("county", "tract", "block group")

#states <- c("US")
#geometries <- c("zip code")

# Originally located in my home directory due to the very long download not
# playing well with the stability of qnap3; later moved to qnap3. See the setup
# section below.
totalcensus_path <- "/media/qnap4/Covariates/totalcensus/"
#totalcensus_path <- "/home/edgar/totalcensus/"
#totalcensus_path <- file.path("external", "totalcensus")

# Setup --------------------------------------------------------------------

dir.create(totalcensus_path, showWarnings = FALSE, recursive = TRUE)
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

# Official docs of `totalcensus` say to use `set_path_to_census` but that
# function requires user interaction
Sys.setenv(PATH_TO_CENSUS = totalcensus_path)

# Inflation adjustments, obtained from the BLS CPI calculator
# Sig figs obtained by entering very large numbers
inflation_adjustments <- fread("inputs/inflation_adjustment.csv")
inflation_target_year <- max(inflation_adjustments$year)

# State FIPS codes, used for sorting processes in the final merge
state_fips_codes <- fread("inputs/state_fips_codes.csv")

# Cutoffs for inflation-adjusted 20th and 80th percentile income categories used
# in the B19001 table
b19001_cutoffs <- read_excel("inputs/B19001_cutoffs.xlsx")

# One-time setup ----------------------------------------------------------
# Pre-download all the data so we don't get prompted during the main loop
#
# The network drives sometimes have some IO issues due to the size of this
# (~50GB/year) so it's better to download everything to a home directory and
# then copy the processed files over to the QNAPs later
#
# Note that this function requires user interaction; comment this code block
# back out once the initial download is complete

#for (year in years) {
#  download_census("acs5", year, states = states)
#}

# Start loops ---------------------------------------------------------------
# Don't run this section if you are working with a specific set of constants
# from above

total <- length(states) * length(years) * length(geometries)
i <- 0

for (state in states) {
  output_subdirectory <- file.path(output_directory, tolower(state))
  dir.create(output_subdirectory, showWarnings = FALSE)
  
  for (year in years) {
    # for scoping issues where a column is also named `year`
    year_ <- year
    
    for (geometry in geometries) {
      geometry_plural <- paste0(gsub("y", "ie", geometry), "s")
      geometry_underscores <- gsub(" ", "_", geometry_plural)
      
      i <- i + 1
      
      cat(sprintf(
        "(%d/%d) Processing: %s %s %s\nCalculating:\n",
        i, total, year, state, geometry_plural
      ))
      
      # `zip code` is not a valid summary_level
      if (geometry == "zip code") {
        global_args <- list(
          year = year,
          states = "US",
          geo_headers = "ZCTA5",
          summary_level = "860",
          show_progress = FALSE
        )
      } else {
        global_args <- list(
          year = year,
          states = c(state),
          summary_level = geometry,
          show_progress = FALSE
        )
      }

# Quick override for debugging --------------------------------------------

if (FALSE) {
    global_args <- list(
      year = 2009,
      states = c("MA"),
      summary_level = "tract",
      show_progress = FALSE
    )
    global_args <- list(
      year = 2019,
      states = c("MA"),
      summary_level = "tract",
      show_progress = FALSE
    )
    global_args <- list(
      year = 2019,
      states = "US",
      geo_headers = "ZCTA5",
      summary_level = "860",
      show_progress = FALSE
    )
    global_args <- list(
      year = 2015,
      states = "US",
      geo_headers = "ZCTA5",
      summary_level = "860",
      show_progress = FALSE
    )
    global_args <- list(
      year = 2010,
      states = "US",
      geo_headers = "ZCTA5",
      summary_level = "860",
      show_progress = FALSE
    )
    
    do.call(
      read_acs5year,
      c(
        list(
          table_contents = c(
            "population = B01001_001", # already included by totalcensus by default
            "pct_female = B01001_026",
            "pct_poverty = B17001_002",
            "pct_white = B02001_002",
            "pct_black = B02001_003",
            "pct_asian = B02001_005"
          )
        ),
        global_args
      )
    )[]

}

# Universe: Total population ----------------------------------------------

cat("* Total population...")

total_population <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "pct_female = B01001_026",
        "pct_poverty = B17001_002",
        "pct_white = B02001_002",
        "pct_black = B02001_003",
        "pct_native = B02001_004",
        "pct_asian = B02001_005",
        "pct_two_or_more_races = B02001_008",
        "pct_hispanic_white = B03002_013",
        "pct_hispanic_black = B03002_014",
        "pct_hispanic_native = B03002_015",
        "pct_hispanic_asian = B03002_016",
        "pct_hispanic_two_or_more_races = B03002_019",
        "pct_non_hispanic_white = B03002_003",
        "pct_non_hispanic_black = B03002_004",
        "pct_non_hispanic_native = B03002_005",
        "pct_non_hispanic_asian = B03002_006",
        "pct_non_hispanic_two_or_more_races = B03002_009",
        "pct_hispanic = B03002_012",
        "pct_foreign_born = B05006_001",
        
        # Compatibility with 1990 Decennial Census
        "temp_pacific_islander = B02001_006",
        "temp_hispanic_pacific_islander = B03002_017",
        "temp_non_hispanic_pacific_islander = B03002_007"
      )
    ),
    global_args
  )
) %>%
  mutate(
    pct_asian_pacific_islander = (pct_asian + temp_pacific_islander) / population,
    pct_hispanic_asian_pacific_islander = (pct_hispanic_asian + temp_hispanic_pacific_islander) / population,
    pct_non_hispanic_asian_pacific_islander = (pct_non_hispanic_asian + temp_non_hispanic_pacific_islander) / population,
    
    across(starts_with("pct_"), ~./population),
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat,
    
    -temp_pacific_islander, -temp_hispanic_pacific_islander, -temp_non_hispanic_pacific_islander
  )

cat(" ok\n")

# Universe: Households ----------------------------------------------------

cat("* Household variables")

households <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "n_households = B11001_001",
        "mean_household_size = B25010_001",
        "pct_households_single_father = B11001_005",
        "pct_households_single_mother = B11001_006",
        "pct_public_assistance = B19057_002"
      )
    ),
    global_args
  )
) %>%
  mutate(
    across(starts_with("pct_"), ~./n_households)
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat, -population
  )

cat(" ok\n")

# Universe: Occupied housing units ----------------------------------------
        
cat("* Occupied housing units...")

occupied_housing_units <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "n_occupied_housing_units = B25003_001",
        "pct_renting = B25003_003",
        "pct_heating_utility_gas = B25040_002",
        "pct_heating_gas_tank = B25040_003",
        "pct_heating_electricity = B25040_004",
        "pct_heating_oil = B25040_005",
        "pct_heating_coal = B25040_006",
        "pct_heating_wood = B25040_007",
        "pct_heating_solar = B25040_008",
        "pct_heating_other = B25040_009",
        "pct_heating_none = B25040_010"
      )
    ),
    global_args
  )
) %>%
  mutate(
    across(starts_with("pct_"), ~./n_occupied_housing_units)
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat, -population
  )

cat(" ok\n")

# Universe: Housing units -------------------------------------------------

cat("* Housing units...")

housing_units <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "n_housing_units = B25024_001",
        "pct_housing_standalone = B25024_002",
        "pct_housing_1_unit = B25024_003",
        "pct_housing_2_units = B25024_004",
        "pct_housing_3_to_4_units = B25024_005",
        "pct_housing_5_to_9_units = B25024_006",
        "pct_housing_10_to_19_units = B25024_007",
        "pct_housing_20_to_49_units = B25024_008",
        "pct_housing_gt_50_units = B25024_009",
        "pct_housing_mobile_home = B25024_010",
        "pct_housing_vehicle = B25024_011",
        "pct_built_2014_onwards = B25034_002",
        "pct_built_2010_to_1949 = B25034_003",
        "pct_built_2000_to_1949 = B25034_004",
        "pct_built_1990_to_1949 = B25034_005",
        "pct_built_1980_to_1949 = B25034_006",
        "pct_built_1970_to_1949 = B25034_007",
        "pct_built_1960_to_1949 = B25034_008",
        "pct_built_1950_to_1949 = B25034_009",
        "pct_built_1940_to_1949 = B25034_010",
        "med_year_built = B25035_001"
      )
    ),
    global_args
  )
) %>%
  mutate(
    across(starts_with("pct_"), ~./n_housing_units)
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat, -population
  )

cat(" ok\n")

# Universe: Population 16 years and over (employment) ---------------------

cat("* Population 16 years and over...")

if (global_args$year %in% c(2009, 2010)) {
  # using long formulation because the abbreviated versions aren't available
  # in older versions of the ACS (2009 and 2010)
  cat(" using long formulation...")
  employment <- do.call(
    read_acs5year,
    c(
      list(
        table_contents = sapply(
          c(1:173),
          function(x) sprintf("B23001_%03d", x)
        )
      ),
      global_args
    )
  ) %>%
    mutate(
      labor_force = (
        B23001_004 + B23001_011 + B23001_018 + B23001_025 +
        B23001_032 + B23001_039 + B23001_046 + B23001_053 +
        B23001_060 + B23001_067 + B23001_074 + B23001_079 +
        B23001_084 + B23001_090 + B23001_097 + B23001_104 +
        B23001_111 + B23001_118 + B23001_125 + B23001_132 +
        B23001_139 + B23001_146 + B23001_153 + B23001_160 +
        B23001_165 + B23001_170
      ),
      unemployed = (
        B23001_008 + B23001_015 + B23001_022 + B23001_029 +
        B23001_036 + B23001_043 + B23001_050 + B23001_057 +
        B23001_064 + B23001_071 + B23001_076 + B23001_081 +
        B23001_086 + B23001_094 + B23001_101 + B23001_108 +
        B23001_115 + B23001_122 + B23001_129 + B23001_136 +
        B23001_143 + B23001_150 + B23001_157 + B23001_162 +
        B23001_167 + B23001_172
      )
    ) %>%
    transmute(
      GEOID,
      labor_force,
      pct_unemployed = unemployed / labor_force
    )
} else {
  # in 2011, aggregate measures were added in as B23025
  employment <- do.call(
    read_acs5year,
    c(
      list(
        table_contents = c(
          "labor_force = B23025_002",
          "unemployed = B23025_005"
        )
      ),
      global_args
    )
  ) %>%
    transmute(
      GEOID,
      labor_force,
      pct_unemployed = unemployed / labor_force
    )
}

cat(" ok\n")

# Universe: Population 18 years and over (education) ---------------------

cat("* Population 18 years and over...")

population_18_years_and_over<- do.call(
  read_acs5year,
  c(
    list(
      table_contents = sapply(
        c(1:83),
        function(x) sprintf("B15001_%03d", x)
      )
    ),
    global_args
  )
) %>%
  transmute(
    GEOID,
    pct_edu_lt_9th_grade = (
      B15001_004 + B15001_012 + B15001_020 + B15001_028 + B15001_036 + B15001_045+ B15001_053 + B15001_061 + B15001_069 + B15001_077
    ) / B15001_001,
    pct_edu_9th_to_12th_grade = (
      B15001_005 + B15001_013 + B15001_021 + B15001_029 + B15001_037 + B15001_046 + B15001_054 + B15001_062 + B15001_070 + B15001_078
    ) / B15001_001,
    pct_edu_high_school = (
      B15001_006 + B15001_014 + B15001_022 + B15001_030 + B15001_038 + B15001_047 + B15001_055 + B15001_063 + B15001_071 + B15001_079
    ) / B15001_001,
    pct_edu_some_college = (
      B15001_007 + B15001_015 + B15001_023 + B15001_031 + B15001_039 + B15001_048 + B15001_056 + B15001_064 + B15001_072 + B15001_080
    ) / B15001_001,
    pct_edu_associate = (
      B15001_008 + B15001_016 + B15001_024 + B15001_032 + B15001_040 + B15001_049 + B15001_057 + B15001_065 + B15001_073 + B15001_081
    ) / B15001_001,
    pct_edu_bachelors = (
      B15001_009 + B15001_017 + B15001_025 + B15001_033 + B15001_041 + B15001_050 + B15001_058 + B15001_066 + B15001_074 + B15001_082
    ) / B15001_001,
    pct_edu_graduate_or_professional = (
      B15001_010 + B15001_018 + B15001_026 + B15001_034 + B15001_042 + B15001_051 + B15001_059 + B15001_067 + B15001_075 + B15001_083
    ) / B15001_001,
  )

cat(" ok\n")

# Universe: Workers 16 years and over -------------------------------------

cat("* Workers 16 years and over...")

workers_16_years_and_over <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "n_workers_16_years_and_over = B08301_001",
        "pct_transport_auto = B08301_002",
        "pct_transport_public_transit = B08301_010",
        "pct_transport_bus = B08301_011",
        "pct_transport_streetcar = B08301_012",
        "pct_transport_subway = B08301_013",
        "pct_transport_rail = B08301_014",
        "pct_transport_taxi = B08301_016",
        "pct_transport_motorcycle = B08301_017",
        "pct_transport_bicycle = B08301_018",
        "pct_transport_walk = B08301_019",
        "pct_transport_other = B08301_020",
        "pct_transport_wfh = B08301_021"
      )
    ),
    global_args
  )
) %>%
  mutate(
    # convert pct_* columns into actual percentages
    across(starts_with("pct_"), ~./n_workers_16_years_and_over),
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat, -population
  )

cat(" ok\n")

# Universe: Workers 16 years and over who did not work at home ------------

cat("* Workers 16 years and over who did not work at home...")

workers_16_years_and_over_not_wfh <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "n_workers_16_years_and_over_not_wfh = B08303_001",
        "pct_travel_lt_5_min = B08303_002",
        "pct_travel_5_to_9_min = B08303_003",
        "pct_travel_10_to_14_min = B08303_004",
        "pct_travel_15_to_19_min = B08303_005",
        "pct_travel_20_to_24_min = B08303_006",
        "pct_travel_25_to_29_min = B08303_007",
        "pct_travel_30_to_34_min = B08303_008",
        "pct_travel_35_to_39_min = B08303_009",
        "pct_travel_40_to_44_min = B08303_010",
        "pct_travel_45_to_59_min = B08303_011",
        "pct_travel_60_to_89_min = B08303_012",
        "pct_travel_gt_90_min = B08303_013"
      )
    ),
    global_args
  )
) %>%
  mutate(
    across(starts_with("pct_"), ~./n_workers_16_years_and_over_not_wfh),
  ) %>%
  select(
    # extra variables added by totalcensus
    -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat, -population
  )

cat(" ok\n")

# Nullable plain Census variables (deprecated) -----------------------
# These variables may not exist in every version of the census

# cat("* Nullable variables...")
# 
# nullable_column_definitions <- c(
#   "labor_force = B23025_002",
#   "unemployed = B23025_005",
#   "houses_built_before_1939 = B25034_011" # TODO: move this to the household variables
# )
# 
# # we are assuming that census_vars ran correctly and can use its GEOID column
# # here to build the nullable variables table
# geoids <- census_vars[,GEOID]
# 
# # if a census table exists, return the column as defined by the `totalcensus`
# # column definition; otherwise, return it filled with NA
# get_nullable_column <- function(column_definition) {
#   column_name <- str_extract(column_definition, "[^ =]+")
#   return(
#     tryCatch(
#       {
#         do.call(
#           read_acs5year,
#           c(
#             list(table_contents = c(column_definition)),
#             global_args
#           )
#         ) %>%
#           select(GEOID, !!as.name(column_name))
#       },
#       error = function(x) {
#         return(
#           census_vars %>%
#             select(GEOID) %>%
#             mutate(!!as.name(column_name) := NA)
#         )
#       }
#     )
#   )
# }
# 
# nullable_census_vars <- Reduce(
#   function(x, y) x[y, on = "GEOID"],
#   lapply(
#     nullable_column_definitions,
#     get_nullable_column
#   )
# ) %>%
#   mutate(
#     # convert pct_* columns into actual percentages
#     across(starts_with("pct_"), ~./population),
#     
#     # manual calculations
#     pct_elligible_unemployed = unemployed / labor_force
#   ) %>%
#   select(
#     # drop temporary columns
#     -unemployed, -labor_force
#   )
# 
# cat(" ok\n")

# Currency-based variables ------------------------------------------------
# These need to be inflation-adjusted

cat("* Currency-vased variables...")

# can't figure out how to dynamically set the index for a list in R so a
# temporary list is being created here instead
mutate_across_funs <- list()
mutate_across_funs[[as.character(inflation_target_year)]] <-
  ~.*inflation_adjustments[year == year_, cpi_adjustment]

currency_vars <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "med_household_income = B19013_001",
        "med_family_income = B19113_001",
        "med_property_value = B25077_001"
      )
    ),
    global_args
  )
) %>%
  mutate(across(starts_with("med_"), mutate_across_funs)) %>%
  select(
    # extra variables added by totalcensus
    -population, -NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat
  )

cat(" ok\n")

# Age distribution --------------------------------------------------------

cat("* Age Distribution...")

age_dist <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = sapply(
        c(3:25, 27:49), # 003-025: Male; 027-049: Female
        function(x) sprintf("B01001_%03d", x)
      )
    ),
    global_args
  )
) %>%
  transmute(
    GEOID,
    pct_age_under_5 = (B01001_003 + B01001_027) / population,
    pct_age_5_to_9 = (B01001_004 + B01001_028) / population,
    pct_age_10_to_14 = (B01001_005 + B01001_029) / population,
    pct_age_15_to_17 = (B01001_006 + B01001_030) / population,
    pct_age_18_to_19 = (B01001_007 + B01001_031) / population,
    pct_age_20 = (B01001_008 + B01001_032) / population,
    pct_age_21 = (B01001_009 + B01001_033) / population,
    pct_age_22_to_24 = (B01001_010 + B01001_034) / population,
    pct_age_25_to_29 = (B01001_011 + B01001_035) / population,
    pct_age_30_to_34 = (B01001_012 + B01001_036) / population,
    pct_age_35_to_39 = (B01001_013 + B01001_037) / population,
    pct_age_40_to_44 = (B01001_014 + B01001_038) / population,
    pct_age_45_to_49 = (B01001_015 + B01001_039) / population,
    pct_age_50_to_54 = (B01001_016 + B01001_040) / population,
    pct_age_55_to_59 = (B01001_017 + B01001_041) / population,
    pct_age_60_to_61 = (B01001_018 + B01001_042) / population,
    pct_age_62_to_64 = (B01001_019 + B01001_043) / population,
    pct_age_65_to_66 = (B01001_020 + B01001_044) / population,
    pct_age_67_to_69 = (B01001_021 + B01001_045) / population,
    pct_age_65_to_69 = (B01001_020 + B01001_044 + B01001_021 + B01001_045) / population,
    pct_age_70_to_74 = (B01001_022 + B01001_046) / population,
    pct_age_75_to_79 = (B01001_023 + B01001_047) / population,
    pct_age_80_to_84 = (B01001_024 + B01001_048) / population,
    pct_age_over_85 = (B01001_025 + B01001_049) / population,
  )

cat(" ok\n")

# Gini index (directly from the Census) -----------------------------------
# Only available at the tract level

cat("* Gini index...")

gini_index <- do.call(
  read_acs5year,
  c(
    table_contents = "gini_index = B19083_001",
    global_args
  )
) %>%
  select(
    GEOID,
    gini_index
  )

cat(" ok\n")

# Ethnic fractionalization ------------------------------------------------
# Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R. (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194. https://doi.org/10.1023/A:1024471506938

cat("* Ethnic fractionalization...")

ethnic_fractionalization <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "race_total = B03002_001",
        "non_hispanic_white = B03002_003",
        "non_hispanic_black = B03002_004",
        "non_hispanic_asian = B03002_006",
        "hispanic = B03002_012"
      )
    ),
    global_args
  )
) %>%
  transmute(
    GEOID,
    ethnic_fractionalization = 1 - (
      (non_hispanic_white / race_total)^2
      + (non_hispanic_black / race_total)^2
      + (non_hispanic_asian / race_total)^2
      + (hispanic / race_total)^2
    )
  )

cat(" ok\n")

# Index of Concentration at the Extremes (ICE) ----------------------------
# Krieger, N., Kim, R., Feldman, J., & Waterman, P. D. (2018). Using the Index of Concentration at the Extremes at multiple geographical levels to monitor health inequities in an era of growing spatial social polarization: Massachusetts, USA (2010–14). International Journal of Epidemiology, 47(3), 788–819. https://doi.org/10.1093/ije/dyy004

# adjustments found with the BLS CPI inflation calculator:
# https://www.bls.gov/data/inflation_calculator.htm
# comparing $1,000 in december 2019 to december 20xx then dividing by $1,000
# using $1,000 for extra precision
row <- b19001_cutoffs %>%
  filter(str_starts(year, as.character(.GlobalEnv$year))) %>%
  head(1) %>% # there are some years with multiple rows, e.g. 2013 -> 2013 (39) and 2013 (38); TODO: fix
  as.list()

cat("* Index of concentration at the extremes (income and race/ethnicity)...")

ice_income <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = sapply(
        c("", "A", "B", "H"),
        function(race_eth) {
          sapply(
            c(1:17),
            function(x) sprintf("B19001%s_%03d", race_eth, x)
          )
        }
      )
    ),
    global_args
  )
) %>%
  mutate(
    # total number of people measured
    income_total = get(sprintf("%s_%03d", row$acs_table, row$acs_total)),
    
    # lower 20th pctile
    income_lower_20th_pctile = rowSums(select(
        .,
        sapply(
          c(row$acs_total + 1:row$acs_20th_upper),
          function(x) sprintf("%s_%03d", row$acs_table, x)
        ),
    )),
    
    # upper 20th pctile
    income_upper_20th_pctile = rowSums(select(
        .,
        sapply(
          c(row$acs_80th_lower:17),
          function(x) sprintf("%s_%03d", row$acs_table, x)
        ),
    )),
    
    # upper 20th pctile white
    income_upper_20th_pctile_white = rowSums(select(
        .,
        sapply(
          c(row$acs_80th_lower:17),
          function(x) sprintf("%sA_%03d", row$acs_table, x)
        ),
    )),
    
    # lower 20th pctile black
    income_lower_20th_pctile_black = rowSums(select(
        .,
        sapply(
          c(row$acs_total + 1:row$acs_20th_upper),
          function(x) sprintf("%sB_%03d", row$acs_table, x)
        ),
    )),
    
    # upper 20th pctile white nonhispanic
    income_upper_20th_pctile_white_nonhisp = rowSums(select(
        .,
        sapply(
          c(row$acs_80th_lower:17),
          function(x) sprintf("%sH_%03d", row$acs_table, x)
        ),
    )),
    
    # lower 20th pctile all except white nonhispanic (subtract white nonhisp from total)
    income_lower_20th_pctile_white_nonhisp = rowSums(select(
        .,
        sapply(
          c(row$acs_total + 1:row$acs_20th_upper),
          function(x) sprintf("%sH_%03d", row$acs_table, x)
        ),
    )),
    income_lower_20th_pctile_not_white_nonhisp = (
      income_lower_20th_pctile - income_lower_20th_pctile_white_nonhisp
    )
    
  ) %>%
  transmute(
    GEOID,
    ice_income = (
      income_upper_20th_pctile - income_lower_20th_pctile
    ) / income_total,
    ice_income_white_v_black = (
      income_upper_20th_pctile_white - income_lower_20th_pctile_black
    ) / income_total,
    ice_income_white_nonhisp_v_other = (
      income_upper_20th_pctile_white_nonhisp - income_lower_20th_pctile_not_white_nonhisp
    ) / income_total
  )

cat(" ok\n")

cat("* Index of concentration at the extremes (race/ethnicity)...")

ice_race_ethnicity <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "race_total = B03002_001",
        "non_hispanic_white = B03002_003",
        "non_hispanic_black = B03002_004"
      )
    ),
    global_args
  )
) %>%
  transmute(
    GEOID,
    ice_race_ethnicity = (non_hispanic_white - non_hispanic_black) / race_total
  )

cat(" ok\n")

# Townsend Index ----------------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4053913/
#
# "Furthermore, a high Townsend Index is indicative of high material
# deprivation"; negative values are in rich areas

cat("* Townsend index...")

# employment info is joined from the employment table from earlier
townsend_index <- do.call(
  read_acs5year,
  c(
    list(
      table_contents = c(
        "households = B11001_001",
        "occupied_units = B25009_001",
        "units_owner_occupied = B25009_002",
        #"persons_unemployed = B23025_005",
        "households_no_vehicle = B08201_002",
        
        # occupancy: > 1 occupants per room
        "B25014_005", "B25014_006", "B25014_007",
        "B25014_011", "B25014_012", "B25014_013"
      )
    ),
    global_args
  )
) %>%
  left_join(employment, by = "GEOID") %>%
  mutate(
    # denominators chosen according to the universe of the respective measure
    # e.g. for vehicles: `Universe: Universe: Households`
    # e.g. for occupancy: `Universe: Universe: Occupied housing units`
    
    pct_households_no_vehicle = households_no_vehicle / households,
    
    pct_overcrowded = (
      B25014_005 + B25014_006 + B25014_007 + B25014_011 + B25014_012 + B25014_013
    ),
    log_pct_overcrowded = log(pct_overcrowded + 1),
    
    pct_units_not_owner_occupied = ifelse(
      occupied_units == 0,
      0,
      (households - units_owner_occupied) / occupied_units
    ),
    
    log_pct_unemployed = log(pct_unemployed + 1),
  ) %>%
  transmute(
    GEOID,
    townsend_index = (
      scale(pct_households_no_vehicle) +
      scale(log_pct_overcrowded) +
      scale(pct_units_not_owner_occupied) +
      scale(log_pct_unemployed)
    )
  )

cat(" ok\n")

# Join all and write out -----------------------------------------------

output_file <- file.path(
  output_subdirectory,
  sprintf("%d_%s.csv.gz", year, geometry_underscores)
)
cat(sprintf("Joining and writing to %s...", output_file))

Reduce(
  function(x, y) left_join(x, y, by = "GEOID"),
  list(
    total_population,
    households,
    occupied_housing_units,
    housing_units,
    employment,
    population_18_years_and_over,
    workers_16_years_and_over,
    workers_16_years_and_over_not_wfh,
    
    currency_vars,
    age_dist,
    
    gini_index,
    ethnic_fractionalization,
    ice_income,
    ice_race_ethnicity,
    townsend_index
  )
) %>%
  mutate(GEOID = as.character(GEOID)) %>% # disable scientific notation
  fwrite(output_file)

cat(" ok\n")

# End small loops ---------------------------------------------------------
# Don't run this section if you didn't run the start loops section

    cat("Done\n\n")
  }
}

# Merge yearly files into one ---------------------------------------------

# Wide
cat("Merging into wide format:\n")
for (geometry in geometries) {
  cat(sprintf("* %s:", geometry_plural))
  geometry_plural <- paste0(gsub("y", "ie", geometry), "s")
  geometry_underscores <- gsub(" ", "_", geometry_plural)
  
  merged <- Reduce(
    #function(x, y) left_join(x, y, by = "GEOID"), # dplyr
    function(x, y) x[y, on = "GEOID"], # data.table
    lapply(
      years,
      function(year) {
        cat(sprintf(" %s...", year))
        fread(
          file.path(
            output_subdirectory,
            sprintf("%d_%s.csv.gz", year, geometry_underscores)
          )
        ) %>%
          rename_all(paste0, "_", year) %>%
          mutate(GEOID = str_extract(get(paste0("GEOID_", year)), "US[0-9]+"))
      }
    )
  )
  
  cat(" writing...")
  fwrite(
    merged,
    file.path(
      output_subdirectory,
      sprintf(
        "time_series_%s_to_%s_%s_wide.csv.gz",
        min(years),
        max(years),
        geometry_underscores
      )
    )
  )
  
  cat(" ok\n")
  rm(merged)
}

# Long
cat("Writing into long format:\n")
for (geometry in geometries) {
  cat(sprintf("* %s:", geometry_plural))
  geometry_plural <- paste0(gsub("y", "ie", geometry), "s")
  geometry_underscores <- gsub(" ", "_", geometry_plural)
  
  # can't figure out how to append to .csv.gz files in R so we just concatenate
  # everything before writing it out
  concatenated <- rbindlist(
    lapply(
      years,
      function(year) {
        cat(sprintf(" %s...", year))
        fread(
          file.path(
            output_subdirectory,
            sprintf("%d_%s.csv.gz", year, geometry_underscores)
          )
        ) %>%
          mutate(year = year)
      }
    )
  )
    
  cat(" writing...")
  fwrite(
    concatenated,
    file.path(
      output_subdirectory,
      sprintf(
        "time_series_%s_to_%s_%s_long.csv.gz",
        min(years),
        max(years),
        geometry_underscores
      )
    )
  )

  cat(" ok\n")
  rm(concatenated)
}

# End main loop -----------------------------------------------------------

}

# Merge for nationwide block groups ---------------------------------------

files <- data.table(
  path = Sys.glob(file.path(output_directory, "*", "time_series*"))
) %>%
  separate(
    path,
    c(NA, NA, "abbreviation", "filename"),
    sep = "/",
    remove = FALSE
  ) %>%
  mutate(
    abbreviation = toupper(abbreviation)
  ) %>%
  left_join(state_fips_codes) %>%
  arrange(fips, filename) # so that the merged data will be sorted

filenames <-sort(unique(files$filename))
n_filenames <- length(filenames)

for (i in c(1:n_filenames)) {
  filename_ <- filenames[i] # underscore to prevent column name conflict
  output_file <- file.path(output_directory, filename_)
  first <- TRUE
  
  cat(sprintf("(%d/%d) merging %s", i, n_filenames, filename_))
  
  fwrite(
    rbindlist(
      lapply(
        files[filename == filename_, path],
        function(path) {
          cat(".") # for progress indicator
          return(fread(path))
        }
      )
    ),
    output_file
  ) 
  cat(" ok\n")
  
  # Low-memory version (much slower)
  #for (path in files[filename == filename_, path]) {
  #  if (first) { # copy the first file
  #    file.copy(path, output_directory)
  #  } else { # copy lines from other files
  #    fp_input <- gzfile(path)
  #    fp_output <- gzfile(output_file, "at")
  #    header <- readLines(fp_input, n = 1) # discard the header
  #    while (TRUE) {
  #      line <- readLines(fp_input, n = 1)
  #      if (length(line) == 0) {
  #        break
  #      } else {
  #        writeLines(line, fp_output)
  #      }
  #    }
  #  }
  #  first  <- FALSE
  #  cat(".")
  #}
  #cat(" ok\n")
}
