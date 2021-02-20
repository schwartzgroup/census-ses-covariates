library(dplyr)
library(sf)
library(stringr)
library(testthat)

# Setup -------------------------------------------------------------------

# Note that we will be using Massachusetts data
measures <- read.csv("outputs/tables/ma/time_series_2009_to_2019_tracts_long.csv.gz")
crosswalk <- read.csv("outputs/crosswalks/ma_tracts_crosswalk.csv.gz")
shapefile <- st_read("outputs/crosswalks/ma_tracts.shp", quiet = TRUE)

merged <- left_join(
    crosswalk,
    mutate(shapefile, GEOID = as.numeric(GEOID)),
    by = c("GEOID", "amended")
  ) %>%
  left_join(
    mutate(measures, GEOID = as.numeric(str_sub(GEOID, 8))),
    by = c("GEOID", "year")
  ) %>%
  st_as_sf(crs = st_crs(shapefile))

percent_difference <- function(actual, expected) {
  return(abs(actual - expected) / expected)
}

# Tests -------------------------------------------------------------------

test_that("Correct number of tracts per year", {
  tracts_2010_to_2019 <- rep(1478, 2019-2010+1)
  
  # Actual tracts: 1367 in 2009; 1478 from 2010-2019 according to TIGER/Line
  expect_equal(as.numeric(table(crosswalk$year)), c(1367, tracts_2010_to_2019))
  
  # However, for actual summary tables, GEOID 25003935300 is not available from
  # the Census ACS 5-year 2009 API, so there would be 1366 in 2009 instead
  expect_equal(as.numeric(table(measures$year)), c(1366, tracts_2010_to_2019))
})

test_that("Similar overall population density", {
  # There will be some discrepancy due to use of ACS 5-year estimates rather
  # than 2010 Decennial Census data
  tolerance <- 0.05
  
  # Values from from US Census Guide to 2010 State and Local Census Geography
  actual_area <- 7800.1 # square miles
  actual_population_density <- 839.4 # population per square mile
  
  # Values from 2010 crosswalk, merged to the longitudinal measures, which
  # includes unchanged tracts from 2009
  merged_2010 <- filter(merged, year == 2010)
  merged_area <- sum(merged_2010$land_area) / 2.59e6 # need to convert to sq mi
  merged_population <- sum(merged_2010$population)
  
  expect_lt(
    percent_difference(
      actual_population_density,
      merged_population / merged_area
    ),
    tolerance
  )
})
