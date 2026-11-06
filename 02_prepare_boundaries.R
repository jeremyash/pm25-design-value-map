
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
# Forest Administrative Boundaries:
# https://data-usfs.hub.arcgis.com/datasets/usfs::forest-administrative-boundaries-feature-layer
#
# Individual National Forest and National Grassland units:
# data/spatial/land_units/land_units.shp
#
# Five aggregated administrative units are replaced
# with individual units from the land_units shapefile.


## Read administrative GeoJSON ----

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = "0")

usfs_raw <- st_read(
  "data/spatial/Forest_Administrative_Boundaries_(Feature_Layer).geojson",
  quiet = TRUE
)


## Read individual land units ----

land_units_raw <- st_read(
  "data/spatial/land_units/land_units.shp",
  quiet = TRUE
)


# 8. Prepare CONUS USFS boundaries ----

## Aggregated units to replace ----

replace_forests <- c(
  "National Forests in North Carolina",
  "National Forests in Alabama",
  "National Forests in Florida",
  "National Forests in Texas",
  "National Forests in Mississippi"
)


## Retain remaining administrative boundaries ----

usfs_remaining <- usfs_raw %>%
  filter(
    REGION != "10",
    FORESTNAME != "El Yunque National Forest",
    !FORESTNAME %in% replace_forests
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


## Select individual National Forest and Grassland units ----

land_units_selected <- land_units_raw %>%
  filter(
    NFSLANDU_1 %in% c(
      "National Forest",
      "National Grassland"
    )
  ) %>%
  st_transform(4326) %>%
  st_make_valid()




## Identify individual replacement units ----

replacement_units <- c(
  # North Carolina
  "Croatan National Forest",
  "Nantahala National Forest",
  "Pisgah National Forest",
  "Uwharrie National Forest",
  
  # Alabama
  "Conecuh National Forest",
  "Talladega National Forest",
  "Tuskegee National Forest",
  "William B. Bankhead National Forest",
  
  # Florida
  "Apalachicola National Forest",
  "Ocala National Forest",
  "Osceola National Forest",
  
  # Texas
  "Angelina National Forest",
  "Davy Crockett National Forest",
  "Sabine National Forest",
  "Sam Houston National Forest",
  "Caddo National Grassland",
  "Lyndon B. Johnson National Grassland",
  
  # Mississippi
  "Bienville National Forest",
  "De Soto National Forest",
  "Delta National Forest",
  "Holly Springs National Forest",
  "Homochitto National Forest",
  "Tombigbee National Forest"
)


## Select only named National Forests and Grasslands ----

land_units_replacement <- land_units_selected %>%
  filter(
    REGION == "08",
    NFSLANDU_2 %in% replacement_units
  )


## Validate replacement units ----

replacement_found <- unique(land_units_replacement$NFSLANDU_2)

cat("Replacement records:", nrow(land_units_replacement), "\n")
cat("Unique replacement units:", length(replacement_found), "\n")

cat("\nMissing units:\n")
print(setdiff(replacement_units, replacement_found))

cat("\nSelected units:\n")
print(
  land_units_replacement %>%
    st_drop_geometry() %>%
    count(NFSLANDU_1, NFSLANDU_2) %>%
    arrange(NFSLANDU_2)
)


# 9. Combine USFS administrative and individual units ----

## Standardize replacement unit attributes ----

land_units_standardized <- land_units_replacement %>%
  transmute(
    ADMINFORESTID = NA_character_,
    REGION = as.character(REGION),
    FORESTNAME = NFSLANDU_2,
    GIS_ACRES = as.numeric(GIS_ACRES)
  )


## Match attribute types and combine ----

usfs_remaining <- usfs_remaining %>%
  mutate(
    ADMINFORESTID = as.character(ADMINFORESTID),
    REGION = as.character(REGION),
    GIS_ACRES = as.numeric(GIS_ACRES)
  )

usfs_boundaries <- bind_rows(
  usfs_remaining,
  land_units_standardized
) %>%
  st_make_valid()


# 10. Validate combined boundaries ----

cat("Remaining administrative units:", nrow(usfs_remaining), "\n")
cat("Individual replacement units:", nrow(land_units_standardized), "\n")
cat("Total combined units:", nrow(usfs_boundaries), "\n")

cat(
  "Invalid geometries:",
  sum(!st_is_valid(usfs_boundaries)),
  "\n"
)

cat(
  "Coordinate system:",
  st_crs(usfs_boundaries)$epsg,
  "\n"
)

## Confirm aggregated units were removed ----

stopifnot(
  !any(usfs_boundaries$FORESTNAME %in% replace_forests)
)

## Confirm all 23 replacement units are present ----

stopifnot(
  all(replacement_units %in% usfs_boundaries$FORESTNAME)
)


# 11. Export combined USFS boundaries ----

st_write(
  usfs_boundaries,
  "data/processed/usfs_forests_grasslands.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)


