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

geometries <- c("county", "tract", "block group")

# Originally located in my home directory due to the very long download not
# playing well with the stability of qnap3; later moved to qnap3. See the setup
# section below.
#totalcensus_path <- "/media/qnap3/Covariates/totalcensus/"
#totalcensus_path <- "/home/edgar/totalcensus/"
totalcensus_path <- file.path("external", "totalcensus")

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
#  download_census("decennial", year, states = states)
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

# Plain Census values -----------------------------------------------------

cat("* Plain variables...")

census_vars <- read_decennial(
  year = year,
  states = c(state),
  table_contents = c(
    #"population = P0010001", # already included by totalcensus by default
    
    # these variables are actually population counts but will get mutated to
    # percentages in the `mutate_at() below``
    "pct_female = P0120026",
    "pct_white = P0030002",
    "pct_black = P0030003",
    "pct_asian = P0030005",
    "pct_hispanic_white = P0050011",
    "pct_hispanic_black = P0050012",
    "pct_hispanic_asian = P0050014",
    "pct_non_hispanic_white = P0050003",
    "pct_non_hispanic_black = P0050004",
    "pct_non_hispanic_asian = P0050006",
    "pct_hispanic = P0050010",
    # TODO: pct_two_or_more_races
    
    "pct_renting = H0110004"
  ),
  summary_level = geometry,
  show_progress = FALSE
) %>%
  mutate(
    # convert pct_* columns into actual percentages
    across(starts_with("pct_"), ~./population)
  ) %>%
  select(
    # extra variables added by totalcensus
    -acs_NAME, -GEOCOMP, -SUMLEV, -state, -STUSAB, -lon, -lat
  )

cat(" ok\n")

# Age distribution --------------------------------------------------------

cat("* Age Distribution...")

age_dist <- read_decennial(
  year = year,
  states = c(state),
  table_contents = sapply(
    c(3:25, 27:49), # 003-025: Male; 027-049: Female
    function(x) sprintf("P012%04d", x)
  ),
  summary_level = geometry,
  show_progress = FALSE
) %>%
  transmute(
    GEOID,
    pct_age_under_5 = (P0120003 + P0120027) / population,
    pct_age_5_to_9 = (P0120004 + P0120028) / population,
    pct_age_10_to_14 = (P0120005 + P0120029) / population,
    pct_age_15_to_17 = (P0120006 + P0120030) / population,
    pct_age_18_to_19 = (P0120007 + P0120031) / population,
    pct_age_20 = (P0120008 + P0120032) / population,
    pct_age_21 = (P0120009 + P0120033) / population,
    pct_age_22_to_24 = (P0120010 + P0120034) / population,
    pct_age_25_to_29 = (P0120011 + P0120035) / population,
    pct_age_30_to_34 = (P0120012 + P0120036) / population,
    pct_age_35_to_39 = (P0120013 + P0120037) / population,
    pct_age_40_to_44 = (P0120014 + P0120038) / population,
    pct_age_45_to_49 = (P0120015 + P0120039) / population,
    pct_age_50_to_54 = (P0120016 + P0120040) / population,
    pct_age_55_to_59 = (P0120017 + P0120041) / population,
    pct_age_60_to_61 = (P0120018 + P0120042) / population,
    pct_age_62_to_64 = (P0120019 + P0120043) / population,
    pct_age_65_to_66 = (P0120020 + P0120044) / population,
    pct_age_67_to_69 = (P0120021 + P0120045) / population,
    pct_age_70_to_74 = (P0120022 + P0120046) / population,
    pct_age_75_to_79 = (P0120023 + P0120047) / population,
    pct_age_80_to_84 = (P0120024 + P0120048) / population,
    pct_age_over_85 = (P0120025 + P0120049) / population,
  )

cat(" ok\n")

# Ethnic fractionalization ------------------------------------------------
# Alesina, A., Devleeschauwer, A., Easterly, W., Kurlat, S., & Wacziarg, R. (2003). Fractionalization. Journal of Economic Growth, 8(2), 155–194. https://doi.org/10.1023/A:1024471506938

cat("* Ethnic fractionalization...")

ethnic_fractionalization <- read_decennial(
  year = year,
  states = c(state),
  table_contents = c(
    "race_total = P0050001",
    "non_hispanic_white = P0050003",
    "non_hispanic_black = P0050004",
    "non_hispanic_asian = P0050006",
    "hispanic = P0050010"
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

cat("* Index of concentration at the extremes (race/ethnicity)...")

ice_race_ethnicity <- read_decennial(
  year = year,
  states = c(state),
  table_contents = c(
    "race_total = P0050001",
    "non_hispanic_white = P0050003",
    "non_hispanic_black = P0050004"
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
  output_subdirectory,
  sprintf("%d_%s.csv.gz", year, geometry_underscores)
)
cat(sprintf("Joining and writing to %s...", output_file))

Reduce(
  function(x, y) left_join(x, y, by = "GEOID"),
  list(
    census_vars,
    nullable_census_vars,
    currency_vars,
    age_dist,
    edu_dist,
    gini_index,
    ethnic_fractionalization,
    ice_income,
    ice_race_ethnicity
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
