
# PM2.5 Annual Design Values: Interactive Map ----
#
# Purpose:
# Display valid 2023-2025 annual PM2.5 design values
# at CONUS monitoring sites, EPA nonattainment and
# maintenance areas above 9.0 ug/m3, and USFS
# administrative boundaries.
#
# Map framework: Leaflet
# Annual PM2.5 NAAQS: 9.0 ug/m3


# 1. Libraries ----

library(sf)
library(dplyr)
library(readr)
library(leaflet)
library(htmltools)
library(htmlwidgets)


# 2. Load processed data ----

## Monitor design values ----

monitors <- read_csv(
  "data/processed/pm25_monitor_dv_2023_2025.csv",
  show_col_types = FALSE
)

## EPA polygons ----

naa_polygons <- st_read(
  "data/processed/pm25_naa_polygons.gpkg",
  quiet = TRUE
)


## USFS boundaries ----

usfs_boundaries <- st_read(
  "data/processed/usfs_forests_grasslands_simplified.gpkg",
  quiet = TRUE
)




# 3. Define map colors ----

## Annual PM2.5 standard ----

pm25_standard <- 9.0
pm25_near_threshold <- pm25_standard * 0.90


## Assign monitor categories ----

monitors <- monitors %>%
  mutate(
    dv_category = case_when(
      dv_2023_2025 > pm25_standard ~ "Above standard",
      dv_2023_2025 >= pm25_near_threshold ~ "Within 10%",
      TRUE ~ "Below 90%"
    ),
    dv_color = case_when(
      dv_category == "Above standard" ~ "#D73027",
      dv_category == "Within 10%" ~ "#F59E0B",
      TRUE ~ "#3182BD"
    )
  )


# 4. Prepare popup content ----

## Monitor popups ----

monitors <- monitors %>%
  mutate(
    popup = paste0(
      "<b>", htmlEscape(site_name), "</b><br>",
      htmlEscape(county), ", ",
      htmlEscape(state), "<br>",
      "AQS Site ID: ", site_id, "<br>",
      "<b>2023-2025 DV: ",
      dv_2023_2025,
      " &micro;g/m&sup3;</b>"
    ),
    label = sprintf(
      "2023-2025 PM2.5 DV: %.1f µg/m³",
      dv_2023_2025
    )
  )


## EPA polygon popups ----

naa_polygons <- naa_polygons %>%
  mutate(
    popup = paste0(
      "<b>", htmlEscape(area_name), "</b><br>",
      htmlEscape(state_name), "<br>",
      "EPA status: ", htmlEscape(current_status), "<br>",
      "<b>Area DV: ", dv,
      " &micro;g/m&sup3;</b>"
    )
  )


## USFS boundary popups ----

usfs_boundaries <- usfs_boundaries %>%
  mutate(
    popup = paste0(
      "<b>", htmlEscape(FORESTNAME), "</b><br>",
      "USFS Region: ", REGION
    )
  )



# 5. Build Leaflet map ----

pm25_map <- leaflet(
  options = leafletOptions(
    preferCanvas = TRUE,
    zoomControl = FALSE
  )
) %>%
  addProviderTiles(
    providers$OpenStreetMap
  ) %>%
  
  
  
  ## USFS administrative boundaries ----

addPolygons(
  data = usfs_boundaries,
  group = "National Forests & Grasslands",
  layerId = ~FORESTNAME,
  color = "#006837",
  weight = 1.5,
  fillColor = "#A1D99B",
  fillOpacity = 0.55,
  opacity = 0.9,
  popup = ~popup,
  smoothFactor = 1
) %>%
  
  
  
  
  
  
  ## Preliminary nonattainment areas ----

addPolygons(
  data = naa_polygons,
  group = "Nonattainment Areas (preliminary)",
  color = "#B2182B",
  weight = 1.5,
  fillColor = "#F4A582",
  fillOpacity = 0.55,
  opacity = 0.9,
  popup = ~popup
) %>%
  
  
  
  
  ## PM2.5 monitoring sites ----

addCircleMarkers(
  data = filter(monitors, dv_category == "Above standard"),
  lng = ~longitude,
  lat = ~latitude,
  group = "Monitors: Above 9.0",
  radius = 7,
  stroke = TRUE,
  color = "#FFFFFF",
  weight = 2,
  opacity = 1,
  fillOpacity = 0.95,
  fillColor = ~dv_color,
  popup = ~popup,
  label = ~label
) %>%
  
  addCircleMarkers(
    data = filter(monitors, dv_category == "Within 10%"),
    lng = ~longitude,
    lat = ~latitude,
    group = "Monitors: 8.1 to 9.0",
    radius = 7,
    stroke = TRUE,
    color = "#FFFFFF",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.95,
    fillColor = ~dv_color,
    popup = ~popup,
    label = ~label
  ) %>%
  
  addCircleMarkers(
    data = filter(monitors, dv_category == "Below 90%"),
    lng = ~longitude,
    lat = ~latitude,
    group = "Monitors: Below 8.1",
    radius = 7,
    stroke = TRUE,
    color = "#FFFFFF",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.95,
    fillColor = ~dv_color,
    popup = ~popup,
    label = ~label
  ) %>%
  
  ## Hide blue monitors by default ----

hideGroup("Monitors: Below 8.1") %>%
  
  
  
  ## Initial Southeast map extent ----

fitBounds(
  lng1 = -100,
  lat1 = 27,
  lng2 = -77,
  lat2 = 37
) %>%
  
  
  
  ## Map legend ----

addControl(
  position = "bottomright",
  html = HTML(paste0(
    '<div style="
      background: white;
      padding: 12px 14px;
      border-radius: 5px;
      box-shadow: 0 1px 6px rgba(0,0,0,0.25);
      font-size: 13px;
      line-height: 1.9;
    ">',
    
    '<b>2023-2025 Annual PM<sub>2.5</sub> DV (µg/m³)</b><br>',
    
    '<span style="
      display:inline-block;
      width:12px;
      height:12px;
      border-radius:50%;
      background:#D73027;
      border:1px solid #333;
      margin-right:7px;
      vertical-align:middle;
    "></span> Above 9.0<br>',
    
    '<span style="
      display:inline-block;
      width:12px;
      height:12px;
      border-radius:50%;
      background:#F59E0B;
      border:1px solid #333;
      margin-right:7px;
      vertical-align:middle;
    "></span> 8.1–9.0<br>',
    
    '<span style="
      display:inline-block;
      width:12px;
      height:12px;
      border-radius:50%;
      background:#3182BD;
      border:1px solid #333;
      margin-right:7px;
      vertical-align:middle;
    "></span> Below 8.1<br>',
    
    '<hr style="margin:8px 0; border:0; border-top:1px solid #ddd;">',
    
    '<span style="
      display:inline-block;
      width:15px;
      height:12px;
      border:2px solid #006837;
      background:#A1D99B;
      margin-right:7px;
      vertical-align:middle;
    "></span> National Forests & Grasslands<br>',
    
    '<span style="
      display:inline-block;
      width:15px;
      height:12px;
      border:2px solid #B2182B;
      background:#F4A582;
      margin-right:7px;
      vertical-align:middle;
    "></span> Nonattainment Areas (preliminary)',
    
    '</div>'
  ))
)



## Custom expandable monitor layer control ----

pm25_map <- htmlwidgets::onRender(
  pm25_map,
  "
  function(el, x) {
    var map = this;

    // Zoom controls in upper-right
    L.control.zoom({
      position: 'topright'
    }).addTo(map);

    var control = L.control({position: 'topleft'});

    control.onAdd = function(map) {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control-layers leaflet-control'
      );

      div.style.padding = '10px 12px';
      div.style.background = 'white';
      div.style.minWidth = '205px';
      div.style.fontSize = '13px';

      div.innerHTML = `
        <div style='font-weight:bold; margin-bottom:6px;'>
          Map Layers
        </div>

        <details open>
          <summary style='cursor:pointer; font-weight:bold;'>
            PM<sub>2.5</sub> Monitors
          </summary>

          <div style='padding:7px 0 5px 12px;'>

            <label style='display:block; margin-bottom:8px; cursor:pointer;'>
              <input type='checkbox'
                     data-group='Monitors: Above 9.0' checked>
              <span style='display:inline-block; width:13px; height:13px;
                background:#D73027; border:1px solid #333;
                border-radius:50%; vertical-align:middle;
                margin:0 5px 0 3px;'></span>
              Above 9.0
            </label>

            <label style='display:block; margin-bottom:8px; cursor:pointer;'>
              <input type='checkbox'
                     data-group='Monitors: 8.1 to 9.0' checked>
              <span style='display:inline-block; width:13px; height:13px;
                background:#F59E0B; border:1px solid #333;
                border-radius:50%; vertical-align:middle;
                margin:0 5px 0 3px;'></span>
              8.1–9.0
            </label>

            <label style='display:block; cursor:pointer;'>
              <input type='checkbox'
                     data-group='Monitors: Below 8.1'>
              <span style='display:inline-block; width:13px; height:13px;
                background:#3182BD; border:1px solid #333;
                border-radius:50%; vertical-align:middle;
                margin:0 5px 0 3px;'></span>
              Below 8.1
            </label>

          </div>
        </details>

        <hr style='border:0; border-top:1px solid #ddd; margin:7px 0;'>

        <label style='display:block; cursor:pointer;'>
          <input type='checkbox'
                 data-group='Nonattainment Areas (preliminary)' checked>
          Nonattainment Areas (preliminary)
        </label>

        <hr style='border:0; border-top:1px solid #ddd; margin:7px 0;'>

        <label style='display:block; cursor:pointer;'>
          <input type='checkbox'
                 data-group='National Forests & Grasslands' checked>
          National Forests &amp; Grasslands
        </label>
      `;

      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);

      var checkboxes = div.querySelectorAll(
        'input[type=checkbox]'
      );

      checkboxes.forEach(function(checkbox) {
        checkbox.addEventListener('change', function() {
          var groupName = this.getAttribute('data-group');
          var layer = map.layerManager.getLayerGroup(groupName);

          if (this.checked) {
            map.addLayer(layer);
          } else {
            map.removeLayer(layer);
          }
        });
      });

      return div;
    };

    
    control.addTo(map);

    // Compact layer control on mobile devices
    if (window.innerWidth <= 600) {

      var layerControl = control.getContainer();

      layerControl.style.minWidth = '0';
      layerControl.style.padding = '7px 9px';
      layerControl.style.fontSize = '12px';

      // Collapse monitor categories on mobile
      var monitorDetails = layerControl.querySelector('details');

      if (monitorDetails) {
        monitorDetails.open = false;
      }

    }
  }
  "
)




## Zoom to forest when clicked ----

pm25_map <- htmlwidgets::onRender(
  pm25_map,
  "
  function(el, x) {
    var map = this;

    // Find the USFS forest polygon group
    var forestGroup = map.layerManager.getLayerGroup(
      'National Forests & Grasslands'
    );

    if (!forestGroup) return;

    // Attach a click handler to each forest polygon
    forestGroup.eachLayer(function(layer) {

      if (!layer.getBounds) return;

      layer.on('click', function() {

        var bounds = layer.getBounds();

        if (!bounds.isValid()) return;

        // Add 20% padding around the forest
        map.fitBounds(bounds.pad(0.20), {
          animate: true,
          maxZoom: 11
        });

      });

    });
  }
  "
)


# 6. Add map header ----

## Header styling ----

map_header <- tags$div(
  class = "map-header",
  style = paste(
    "display:flex;",
    "justify-content:space-between;",
    "align-items:center;",
    "flex-wrap:wrap;",
    "gap:10px;",
    "padding:12px 20px;",
    "background:#17324D;",
    "color:white;",
    "font-family:Arial,sans-serif;"
  ),
  
  ## Title and subtitle ----
  
  tags$div(
    tags$div(
      HTML("Annual PM<sub>2.5</sub> Design Values"),
      style = "font-size:20px;font-weight:600;"
    ),
    tags$div(
      "2023–2025 Monitoring Period",
      style = "font-size:12px;color:#D6E2EA;margin-top:3px;"
    )
  ),
  
  ## About link ----
  
  tags$a(
    "About this map",
    href = "about.html",
    style = paste(
      "color:white;",
      "font-size:13px;",
      "text-decoration:underline;"
    )
  )
)




# 7. Export map for GitHub Pages ----

## Set full-height map layout ----

map_page <- htmltools::browsable(
  htmltools::tagList(
    tags$head(
      tags$style(HTML("
        html, body {
          margin: 0;
          padding: 0;
          height: 100%;
          overflow: hidden;
        }

        body {
          display: flex;
          flex-direction: column;
        }

        .map-header {
          flex: 0 0 auto;
        }

        .html-widget {
          flex: 1 1 auto;
          min-height: 0;
          width: 100% !important;
          height: 100% !important;
        }
      "))
    ),
    map_header,
    htmltools::div(
      style = "flex:1 1 auto; min-height:0; display:flex;",
      pm25_map
    )
  )
)


## Export HTML and dependencies ----

htmltools::save_html(
  html = map_page,
  file = "docs/index.html",
  libdir = "index_files"
)


# 8. Display map ----

map_page





