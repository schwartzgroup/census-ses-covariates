library(data.table)
library(dplyr)
library(readxl)
library(stringr)
library(totalcensus)

#states <- states_DC
states <- c("MA")

output_directory <- "generated"

years <- c(2009:2019)

geometries <- c("tract", "block group")

# Originally located in my home directory due to the very long download not
# playing well with the stability of qnap3; later moved to qnap3. See the setup
# section below.
#totalcensus_path <- "/media/qnap3/Covariates/totalcensus/"
#totalcensus_path <- "/home/edgar/totalcensus/"
totalcensus_path <- "./totalcensus/"


# Setup --------------------------------------------------------------------

dir.create(output_directory, showWarnings = FALSE)

# Official docs of `totalcensus` say to use `set_path_to_census` but that
# function requires user interaction
Sys.setenv(PATH_TO_CENSUS = totalcensus_path)

# Pre-download all the data so we don't get prompted during the main loop
#
# The network drives sometimes have some IO issues due to the size of this
# (~50GB/year) so it's better to download everything to a home directory and
# then copy the processed files over to the QNAPs later
#
# Note that this function requires user interaction
#for (year in years) {
#  download_census("acs5", year, states = states)
#}

# Start loops ---------------------------------------------------------------
# Don't run this section if you are working with a specific set of constants
# from above

for (year in years) {
  for (geometry in geometries) {
    cat(sprintf("Processing: %s %ss\nCalculating:\n", year, geometry))

# Plain Census values -----------------------------------------------------

cat("* Plain variables...")

#census_vars <- read_acs5year(
#  year = year,
#  states = states,
#  table_contents = c(
#    
#  ),
#  summary_level = geometry,
#  show_progress = FALSE
#) %>%
#  select(
#    GEOID,
#    gini_index
#  )

cat(" ok\n")

# Gini index (directly from the Census) -----------------------------------
# Only available at the tract level

cat("* Gini index...")

gini_index <- read_acs5year(
  year = year,
  states = states,
  table_contents = "gini_index = B19083_001",
  summary_level = geometry,
  show_progress = FALSE
) %>%
  select(
    GEOID,
    gini_index
  )

cat(" ok\n")

# Ethnic fractionalization ------------------------------------------------
# Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R. (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194. https://doi.org/10.1023/A:1024471506938

cat("* Ethnic fractionalization...")

ethnic_fractionalization <- read_acs5year(
  year = year,
  states = states,
  table_contents = c(
    "race_total = B03002_001",
    "non_hispanic_white = B03002_003",
    "non_hispanic_black = B03002_004",
    "non_hispanic_asian = B03002_006",
    "hispanic = B03002_012"
  ),
  summary_level = geometry,
  show_progress = FALSE
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

cat("* Index of concentration at the extremes (income)...")

# adjustments found with the BLS CPI inflation calculator:
# https://www.bls.gov/data/inflation_calculator.htm
# comparing $1,000 in december 2019 to december 20xx then dividing by $1,000
# using $1,000 for extra precision
row <- read_excel("B19001_cutoffs.xlsx") %>%
  filter(str_starts(year, as.character(.GlobalEnv$year))) %>%
  head(1) %>% # there are some years with multiple rows, e.g. 2013 -> 2013 (39) and 2013 (38); TODO: fix
  as.list()

ice_income <- read_acs5year(
  year = year,
  states = states,
  table_contents = sapply(
    c(1:17),
    function(x) sprintf("B19001_%03d", x)
  ),
  summary_level = geometry,
  show_progress = FALSE
) %>%
  mutate(
    # total number of people measured
    income_total = get(sprintf("%s_%03d", row$acs_table, row$acs_total)),
    
    # sum of lower 20th percentile columns
    income_lower_20th_pctile = rowSums(select(
        .,
        sapply(
          c(row$acs_total + 1:row$acs_20th_upper),
          function(x) sprintf("%s_%03d", row$acs_table, x)
        ),
    )),
    
    # sum of upper 20th percentile columns
    income_upper_20th_pctile = rowSums(select(
        .,
        sapply(
          c(row$acs_80th_lower:17),
          function(x) sprintf("%s_%03d", row$acs_table, x)
        ),
    )),
  ) %>%
  transmute(
    GEOID,
    ice_income = (income_upper_20th_pctile - income_lower_20th_pctile) / income_total
  )

cat(" ok\n")

cat("* Index of concentration at the extremes (race/ethnicity)...")

ice_race_ethnicity <- read_acs5year(
  year = year,
  states = states,
  table_contents = c(
    "race_total = B03002_001",
    "non_hispanic_white = B03002_003",
    "non_hispanic_black = B03002_004"
  ),
  summary_level = geometry,
  show_progress = FALSE
) %>%
  transmute(
    GEOID,
    ice_race_ethnicity = (non_hispanic_white - non_hispanic_black) / race_total
  )

cat(" ok\n")

# Join all and write out -----------------------------------------------

output_file <- file.path(
  output_directory,
  sprintf("%d_%ss.csv.gz", year, str_replace(geometry, " ", "_"))
)
cat(sprintf("Joining and writing to %s...", output_file))

Reduce(
  function(x, y) left_join(x, y, by = "GEOID"),
  list(
    gini_index,
    ethnic_fractionalization,
    ice_income,
    ice_race_ethnicity
  )
) %>%
    fwrite(output_file)

cat(" ok\n")

# End loops ---------------------------------------------------------------
# Don't run this section if you didn't run the start loops section

    cat("Done\n\n")
  }
}

# Merge yearly files into one ---------------------------------------------

# Wide
cat("Merging into wide format:")
for (geometry in geometries) {
  cat(sprintf("* %ss:", geometry))
  
  merged <- Reduce(
    #function(x, y) left_join(x, y, by = "GEOID"), # dplyr
    function(x, y) x[y, on = "GEOID"], # data.table
    lapply(
      years,
      function(year) {
        cat(sprintf(" %s...", year))
        fread(
          file.path(
            output_directory,
            sprintf("%d_%ss.csv.gz", year, str_replace(geometry, " ", "_"))
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
      output_directory,
      sprintf(
        "time_series_%s_to_%s_%ss_wide.csv.gz",
        min(years),
        max(years),
        str_replace(geometry, " ", "_")
      )
    )
  )
  
  cat(" ok\n")
  rm(merged)
}

# Long
cat("Writing into long format:")
for (geometry in geometries) {
  cat(sprintf("* %ss:", geometry))
  
  # can't figure out how to append to .csv.gz files in R so we just concatenate
  # everything before writing it out
  concatenated <- rbindlist(
    lapply(
      years,
      function(year) {
        cat(sprintf(" %s...", year))
        fread(
          file.path(
            output_directory,
            sprintf("%d_%ss.csv.gz", year, str_replace(geometry, " ", "_"))
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
      output_directory,
      sprintf(
        "time_series_%s_to_%s_%ss_long.csv.gz",
        min(years),
        max(years),
        str_replace(geometry, " ", "_")
      )
    )
  )

  cat(" ok\n")
  rm(concatenated)
}
