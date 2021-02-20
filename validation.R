library(dplyr)
library(sf)
library(stringr)
library(testthat)

# Some tests will make requests to websites to retrieve external data for
# validation - set this to FALSE if that is not desired
allow_internet <- TRUE

# Setup -------------------------------------------------------------------

# Note that we will be using Massachusetts data
cat("Reading data...\n")
measures <- read.csv("outputs/tables/ma/time_series_2009_to_2019_tracts_long.csv.gz")
crosswalk <- read.csv("outputs/crosswalks/ma_tracts_crosswalk.csv.gz")
shapefile <- st_read("outputs/crosswalks/ma_tracts.shp", quiet = TRUE)

cat("Merging data...\n")
merged_no_measures <- left_join(
    crosswalk,
    mutate(shapefile, GEOID = as.numeric(GEOID)),
    by = c("GEOID", "amended")
  ) %>%
  st_as_sf(crs = st_crs(shapefile))
merged <- left_join(
    merged_no_measures,
    mutate(measures, GEOID = as.numeric(str_sub(GEOID, 8))),
    by = c("GEOID", "year")
  ) %>%
  st_as_sf(crs = st_crs(shapefile))

percent_difference <- function(actual, expected) {
  return(abs(actual - expected) / expected)
}

# Tests -------------------------------------------------------------------

cat("Running tests...\n")

test_that("Correct number of tracts per year", {
  tracts_2010_to_2019 <- rep(1478, 2019-2010+1)
  
  # Actual tracts: 1367 in 2009; 1478 from 2010-2019 according to TIGER/Line
  expect_equal(as.numeric(table(crosswalk$year)), c(1367, tracts_2010_to_2019))
  
  # However, for actual summary tables, GEOID 25003935300 is not available from
  # the Census ACS 5-year 2009 API, so there would be 1366 in 2009 instead
  expect_equal(as.numeric(table(measures$year)), c(1366, tracts_2010_to_2019))
})

test_that("Similar overall population density to 2010 Census", {
  # There will be some discrepancy due to use of ACS 5-year estimates rather
  # than 2010 Decennial Census data
  tolerance <- 0.05 # percent difference
  
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

# Will rely on the `tigris` library for external validation here
if (allow_internet) {
  test_that("Matching GEOIDs and similar areas to 2010-2019 shapefiles", {
    tolerance <- 0.01 # percent difference
    
    # Having issues getting 2009 tracts via tigris so only testing 2010-2019
    for (year_ in c(2010:2019)) { 
      geoid_column <- "GEOID"
      if (year_ == 2010) {
        geoid_column <- "GEOID10"
      }
      
      tigris_tracts <- tigris::tracts("MA", year = year_) %>%
        mutate(GEOID = as.numeric(!!as.name(geoid_column))) %>%
        arrange(GEOID)
      
      tigris_areas <- st_area(tigris_tracts)
      
      crosswalk_tracts <- merged_no_measures %>%
        filter(year == year_) %>%
        arrange(GEOID)
      
      crosswalk_areas <- st_area(crosswalk_tracts)
      
      expect_equal(nrow(tigris_tracts), nrow(crosswalk_tracts))
      
      expect_equal(tigris_tracts$GEOID, crosswalk_tracts$GEOID)
      
      # Test that areas are similar
      expect_equal(
        percent_difference(tigris_areas, crosswalk_areas),
        rep(0, nrow(tigris_tracts)),
        tol = tolerance
      )
    }
  })
} else{
  cat("Skipped test that needed Internet access")
}