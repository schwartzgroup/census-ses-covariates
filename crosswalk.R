# Longitudinal Census cross walk generator

library(data.table)
library(dplyr)
library(glue)
library(sf)

output_directory <- "crosswalks"
shapefiles_directory <- "tiger"

years <- c(2009:2019)
geometries <- c("tract", "bg") # TODO: country is only available statewide

geometry_output_names <- list(
  tract = "tracts",
  bg = "block_groups"
)

urls_2009 <- "https://www2.census.gov/geo/tiger/TIGER2009/{statefp}_{state_caps}/tl_{year}_{statefp}_{geometry}00.zip"
urls_2010 <- "https://www2.census.gov/geo/tiger/TIGER2010/{geometry_caps}/{year}/tl_{year}_{statefp}_{geometry}10.zip"
urls_post_2010 <- "https://www2.census.gov/geo/tiger/TIGER{year}/{geometry_caps}/tl_{year}_{statefp}_{geometry}.zip"

# Setup -------------------------------------------------------------------

dir.create(output_directory, showWarnings = FALSE)

# State FIPS codes, used to pull state names
state_fips_codes <- read.csv("inputs/state_fips_codes.csv")

# FIPS codes to actually generate crosswalks for
# FIXME: some issues with virgin islands and northern mariana islands in 2009 so
# we are excluding those for now
fips_codes <- head(state_fips_codes, 50)$fips

# Precursor functions -----------------------------------------------------

# load a shapefile, downloading it first if it is not yet downloaded
load_shapefile <- function(statefp, year, geometry, verbose = FALSE) {
  state_name <- filter(state_fips_codes, fips == statefp)$name
  
  if (year == 2009) {
    url_template <- urls_2009
  } else if (year == 2010) {
    url_template <- urls_2010
  } else {
    url_template <- urls_post_2010
  }
  
  url <- glue(
    url_template,
    year = year,
    statefp = sprintf("%02d", statefp),
    state_caps = toupper(gsub(" ", "_", state_name)),
    geometry = geometry,
    geometry_caps = toupper(geometry)
  )
  
  zip_path <- file.path(shapefiles_directory, basename(url))
  shp_path <- gsub(".zip", ".shp", zip_path)
  
  if (file.exists(shp_path)) {
    return(st_read(shp_path, quiet = !verbose))
  } else {
    cat("downloading shapefile\n")
    download.file(url, zip_path, "auto")
    
    cat("extracting shapefile\n")
    unzip(zip_path, exdir = shapefiles_directory)
    file.remove(zip_path)
    
    return(st_read(shp_path, quiet = !verbose))
  }
}

# detect the `GEOID` column of a given tiger shapefile
geoid_column_name <- function(tiger_sf) {
  columns <- names(tiger_sf)
  if ("GEOID" %in% columns) {
    return("GEOID")
  } else if ("GEOID10" %in% columns) {
    return("GEOID10")
  } else {
    return(columns[grepl("IDFP00", columns)])
  }
}

# calculate the total areas of features in a given tiger shapefile, using the
# ALAND and AWATER columns alone
total_areas <- function(tiger_sf) {
  columns <- names(tiger_sf)
  if ("ALAND" %in% columns) {
    return(tiger_sf$ALAND + tiger_sf$AWATER)
  } else if ("ALAND00" %in% columns) {
    return(tiger_sf$ALAND00 + tiger_sf$AWATER00)
  } else {
    return(tiger_sf$ALAND10 + tiger_sf$AWATER10)
  }
}

# calculate the land areas of features in a given tiger shapefile, using the
# ALAND column alone
land_areas <- function(tiger_sf) {
  columns <- names(tiger_sf)
  if ("ALAND" %in% columns) {
    return(tiger_sf$ALAND)
  } else if ("ALAND00" %in% columns) {
    return(tiger_sf$ALAND00)
  } else {
    return(tiger_sf$ALAND10)
  }
}

# detect the changes between two different tiger shapefiles
#
# returns a list with 3 indices:
# * added - vector of geoids that are unique to tiger_2
# * removed - vector of geoids that are unique to tiger_1
# * changed - vector of geoids that whose geometries changed (present in both)
# * sf_amendments - sf object with all added or changed geoids
tiger_line_diffs <- function(tiger_1,
                             tiger_2,
                             area_change_threshold = 0.05,
                             amendment_year = NA
                             ) {
  geoid_column_1 <- geoid_column_name(tiger_1)
  geoid_column_2 <- geoid_column_name(tiger_2)
  
  # geoid changes
  new_geoids <- setdiff(tiger_2[[geoid_column_2]], tiger_1[[geoid_column_1]])
  removed_geoids <- setdiff(tiger_1[[geoid_column_1]], tiger_2[[geoid_column_2]])
  
  # area changes
  joined <- inner_join(
    st_drop_geometry(tiger_1) %>%
      transmute(
        GEOID = !!as.name(geoid_column_1),
        geometry = tiger_1$geometry,
        area = total_areas(tiger_1)
      ),
    st_drop_geometry(tiger_2) %>%
      transmute(
        GEOID = !!as.name(geoid_column_2),
        geometry = tiger_2$geometry,
        area = total_areas(tiger_2)
      ),
    by = "GEOID",
    suffix = c("_1", "_2")
  )
  
  changed_geoids <- filter(
    joined,
    abs(area_1 - area_2) / area_1 > area_change_threshold
  )$GEOID
  
  return(list(
    added = new_geoids,
    removed = removed_geoids,
    changed = changed_geoids,
    sf_amendments = filter(
      tiger_2,
      !!as.name(geoid_column_2) %in% c(new_geoids, changed_geoids)
    )
  ))
}

# "normalize" a tiger/line shapefile by returning only the geoid and geometry,
# named as GEOID and geometry (geoid column names very over time)
normalize_tiger_line <- function(tiger_sf) {
  return(
    transmute(
      tiger_sf,
      area = total_areas(tiger_sf),
      land_area = land_areas(tiger_sf),
      GEOID = !!as.name(geoid_column_name(tiger_sf))
    )
  )
}

# Main functions ----------------------------------------------------------

# calculate and return only the changes in tiger files between years
tiger_line_deltas <- function(statefp, years, geometry) {
  # the first shapefile is always represented in full, as a reference for
  # subsequent deltas
  sf_previous <- load_shapefile(statefp, years[1], geometry)
  sf_deltas <- list(sf_previous)
  
  # same with geoids
  deltas <- list(
    tiger_line_diffs(sf_previous[0,], sf_previous)
  )
  
  cat("Detecting changes:")
  for (i in c(2:length(years)))  {
    cat(sprintf("\n* %s-%s...", years[i - 1], years[i]))
    
    # load a new shapefile
    sf_current <- load_shapefile(statefp, years[i], geometry)
    
    # detect and append changes
    deltas[[i]] <- tiger_line_diffs(sf_previous, sf_current)
    
    # overwrite sf_previous
    sf_previous <- sf_current
    
    # print
    any_changes <- FALSE
    for (type in c("added", "removed", "changed")) {
      n <- length(deltas[[i]][[type]])
      if (n > 0) {
        if (any_changes) {
          cat(",")
        }
        cat(sprintf(" %d %s", n, type))
        any_changes <- TRUE
      }
    }
    if (!any_changes) {
      cat(" none")
    }
  }
  cat("\n")
  
  return(deltas)
}


# create a crosswalk and accompanying shapefile containing only changes and
# amendments between years
generate_crosswalk <- function(statefp, years, geometry) {
  # calculate deltas
  deltas <- tiger_line_deltas(statefp, years, geometry)
  
  # generate crosswalk
  cat("Generating crosswalk...")
  all_crosswalks <- list()
  latest_changes <- data.frame()
  for (i in c(1:length(years))) {
    delta <- deltas[[i]]
    year <- years[i]
    amendments <- c(delta$added, delta$changed)
    
    # amendments
    if (length(amendments) > 1) {
      latest_changes <- rbind(
        latest_changes,
        data.frame(
          GEOID = amendments,
          amended = year
        )
      )
    }
    
    # removals
    latest_changes <- filter(latest_changes, !(GEOID %in% delta$removed))
    
    # append to all crosswalks
    all_crosswalks[[i]] <- mutate(latest_changes, year = year)
  }
  crosswalk <- do.call("rbind", all_crosswalks)
  remove(all_crosswalks)
  cat(sprintf(" %d rows\n", nrow(crosswalk)))
  
  # generate normalized universal tiger sf
  cat("Generating sf...")
  sfs_normalized <- list()
  for (i in c(1:length(years))) {
    sfs_normalized[[i]] <- mutate(
      normalize_tiger_line(deltas[[i]]$sf_amendments),
      amended = years[i]
    )
  }
  
  sf_universal <- do.call("rbind", sfs_normalized)
  remove(sfs_normalized)
  cat(sprintf(" %d features\n", nrow(sf_universal)))
  
  return(list(
    df = crosswalk,
    sf = sf_universal
  ))
}

# Validation of change detection algorithm --------------------------------

if (FALSE) { # just to keep this from running under normal use
  
## https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2011/geography-changes.html
# tract changes in new york from 2010 to 2011:
# 2x(11 renames) + 3 merges = 25 changes in both directions
tiger_line_diffs(
  load_shapefile(36, 2010, "tract"),
  load_shapefile(36, 2011, "tract")
) %>%
    lapply(length)

## https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2012/geography-changes.html
# 7 tracts renamed in arizona from 2011 to 2012
tiger_line_diffs(
  load_shapefile(4, 2011, "tract"),
  load_shapefile(4, 2012, "tract")
) %>%
  lapply(length)

# 1 tract renamed in california from 2011 to 2012
tiger_line_diffs(
  load_shapefile(6, 2011, "tract"),
  load_shapefile(6, 2012, "tract")
) %>%
  lapply(length)

}

# Generate crosswalks -----------------------------------------------------

i <- 1
n <- length(fips_codes) * length(geometries)
for (statefp in fips_codes) {
  state_info <- filter(state_fips_codes, fips == statefp)
  for (geometry in geometries) {
    cat(sprintf("(%d/%d) %s %s\n", i, n, state_info$name, geometry))
    
    output_base = file.path(
      output_directory,
      sprintf(
        "%s_%s",
        tolower(state_info$abbreviation),
        geometry_output_names[[geometry]]
      )
    )
    
    crosswalk <- generate_crosswalk(statefp, years, geometry)
    
    fwrite(crosswalk$df, paste0(output_base, "_crosswalk.csv.gz"))
    
    # this complains: "GDAL Message 1: Value (...) of field land_area of feature
    # (...) not successfully written. Possibly due to too larger number with
    # respect to field width"; this message can safely be ignored
    st_write(crosswalk$sf, paste0(output_base, ".shp"), append = FALSE)
    
    cat("\n")
    
    i <- i + 1
  }
}
