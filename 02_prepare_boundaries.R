
# PM2.5 Map: EPA and USFS Spatial Boundaries ----
#
# Source:
# https://experience.arcgis.com/experience/85cd54a2fd504876b51152de641b8166
#
# Exported from the 2012 Annual PM2.5 standard layer
# using the 9.0 ug/m3 breakpoint.
#
# Retain areas with CurrentYearDesignValue > 9.0
# regardless of official designation status.


# 1. Libraries ----

library(sf)
library(dplyr)
library(readr)


# 2. Input files ----

input_file <- paste0(
  "data/spatial/",
  "Nonattainment_Areas_and_Designations_",
  "3768959098164540440.geojson"
)


# 3. Read EPA polygons ----

naa_raw <- st_read(
  input_file,
  quiet = TRUE
)


# 4. Prepare polygons ----

naa_polygons <- naa_raw %>%
  mutate(
    dv = as.numeric(CurrentYearDesignValue)
  ) %>%
  filter(
    !is.na(dv),
    dv > 9.0
  ) %>%
  select(
    area_name,
    state_name,
    pollutant_name,
    current_status,
    dv,
    dv_units,
    designation_effective_date,
    geometry
  ) %>%
  st_transform(4326) %>%
  st_make_valid()


# 5. Validate results ----

## Polygon counts ----

cat("Original polygons:", nrow(naa_raw), "\n")
cat("Polygons above 9.0:", nrow(naa_polygons), "\n")

## Geometry validation ----

cat("Coordinate system:", st_crs(naa_polygons)$epsg, "\n")
cat(
  "Invalid geometries:",
  sum(!st_is_valid(naa_polygons)),
  "\n"
)

## Review retained areas ----

print(
  st_drop_geometry(naa_polygons) %>%
    select(area_name, state_name, current_status, dv) %>%
    arrange(desc(dv))
)


# 6. Export processed polygons ----

st_write(
  naa_polygons,
  "data/processed/pm25_naa_polygons.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)



# 7. Read USFS administrative boundaries ----

## Source documentation ----

# USDA Forest Service
# Dataset: Forest Administrative Boundaries
# Includes National Forest and Grassland administrative units.
#
# Source:
# https://data-usfs.hub.arcgis.com/datasets/usfs::forest-administrative-boundaries-feature-layer


## Read GeoJSON ----

# Remove GDAL's per-feature GeoJSON size limit.
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = "0")

usfs_raw <- st_read(
  "data/spatial/Forest_Administrative_Boundaries_(Feature_Layer).geojson",
  quiet = TRUE
)


# 8. Prepare CONUS USFS boundaries ----

usfs_boundaries <- usfs_raw %>%
  filter(
    REGION != "10",
    FORESTNAME != "El Yunque National Forest"
  ) %>%
  select(
    ADMINFORESTID,
    REGION,
    FORESTNAME,
    GIS_ACRES,
    geometry
  ) %>%
  st_transform(4326) %>%
  st_make_valid()


# 9. Validate USFS boundaries ----

## Feature counts ----

cat("Original USFS units:", nrow(usfs_raw), "\n")
cat("CONUS USFS units:", nrow(usfs_boundaries), "\n")


## Geometry validation ----

cat(
  "Coordinate system:",
  st_crs(usfs_boundaries)$epsg,
  "\n"
)

cat(
  "Invalid geometries:",
  sum(!st_is_valid(usfs_boundaries)),
  "\n"
)


# 10. Export USFS boundaries ----

st_write(
  usfs_boundaries,
  "data/processed/usfs_forests_grasslands.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

