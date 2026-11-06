
# PM2.5 Map: Simplify USFS Boundaries ----
#
# Purpose:
# Reduce USFS polygon complexity for the
# interactive Leaflet map while preserving
# the original processed boundaries.
#
# Input:
# data/processed/usfs_forests_grasslands.gpkg
#
# Output:
# data/processed/usfs_forests_grasslands_simplified.gpkg


# 1. Libraries ----

library(sf)
library(dplyr)


# 2. Load original boundaries ----

usfs_original <- st_read(
  "data/processed/usfs_forests_grasslands.gpkg",
  quiet = TRUE
)


# 3. Simplify boundaries ----

## Project to meters ----

usfs_projected <- st_transform(
  usfs_original,
  5070
)


## Simplify with 250-meter tolerance ----

usfs_simplified <- st_simplify(
  usfs_projected,
  dTolerance = 250,
  preserveTopology = TRUE
)


## Validate and return to geographic coordinates ----

usfs_simplified <- usfs_simplified %>%
  st_make_valid() %>%
  st_transform(4326)


# 4. Compare original and simplified boundaries ----


## Coordinate counts ----

original_points <- sum(
  vapply(
    st_geometry(usfs_original),
    function(g) nrow(st_coordinates(g)),
    integer(1)
  )
)

simplified_points <- sum(
  vapply(
    st_geometry(usfs_simplified),
    function(g) nrow(st_coordinates(g)),
    integer(1)
  )
)

cat("Original coordinates:", original_points, "\n")
cat("Simplified coordinates:", simplified_points, "\n")

cat(
  "Reduction:",
  round(100 * (1 - simplified_points / original_points), 1),
  "%\n"
)



## Object sizes ----

cat(
  "Original size:",
  round(as.numeric(object.size(usfs_original)) / 1024^2, 2),
  "MB\n"
)

cat(
  "Simplified size:",
  round(as.numeric(object.size(usfs_simplified)) / 1024^2, 2),
  "MB\n"
)


## Geometry validation ----

cat(
  "Invalid geometries:",
  sum(!st_is_valid(usfs_simplified)),
  "\n"
)


# 5. Export simplified boundaries ----

st_write(
  usfs_simplified,
  "data/processed/usfs_forests_grasslands_simplified.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
