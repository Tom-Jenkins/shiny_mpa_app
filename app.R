# --------------------------- #
#
# Devon, Cornwall & Isles of Scilly MPA Shiny App
#
# Description:
# Dashboard application that displays:
# (i) Interactive map of Marine Protected Areas
# (ii) Interactive table of Features within MPAs
# (iii) About app
#
# Author: Tom Jenkins
#
# --------------------------- #

# Load packages
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(DT)
library(tidyverse)
library(randomcoloR)

# Load data sets
load("data/mcz_sweng.RData")
load("data/sac_sweng.RData")
load("data/sac_porpoise.RData")
load("data/spa_sweng.RData")
load("data/seamap_sweng.RData")

# Import csv table
feature_table = read_csv("data/mpa_data.csv", col_types = "ffff")

# Sort feature levels
feature_table$`Protected Feature` = factor(feature_table$`Protected Feature`,
                                           levels = levels(feature_table$`Protected Feature`) %>% sort)

# Edit names of some habitat types
seamap_sub$JNCCName = str_replace(seamap_sub$JNCCName,"Infralittoral sandy mud or Infralittoral fine mud", "Infralittoral sandy / fine mud")
seamap_sub$JNCCName = str_replace(seamap_sub$JNCCName, "Offshore circalittoral fine sand or Offshore circalittoral muddy sand","Offshore circalittoral fine / muddy sand")


# ----------------- #
#
# Set up leaflet map
#
# ----------------- #

# Basemap
l = leaflet() %>%
  # fitBounds(lng1 = -5.0, lng2 = -4.5, lat1 = 49.8, lat2 = 50.8) %>% 
  setView(lng = -4.173326, lat = 50.63338, zoom = 8) %>%   
  # Add an inset minimap button
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) %>%
  # Add a zoom to level buttom
  addEasyButton(easyButton(
    icon = "fa-globe", title = "Zoom to Level 8",
    onClick = JS("function(btn, map){ map.setZoom(8); }"))) %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
  # select basemap http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  # addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI World Topo Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
  # addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI NatGeo World") %>%
  addProviderTiles(providers$Esri.OceanBasemap, group = "ESRI Ocean Basemap")

# Add Marine Protected Areas to basemap
l = l %>% 
    # Marine Conservation Zones
    addPolygons(data = mcz_sweng,
                group = "Marine Conservation Zones",
                color = "blue",
                weight = 2,
                fillColor = "white",
                fillOpacity = 0.5,
                dashArray = 3,
                highlight = highlightOptions(
                  color = "#666",
                  weight = 3,
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = sprintf(
                  "<strong>%s MCZ</strong><br/>Area: %g km<sup>2</sup>",
                  mcz_sweng$MCZ_NAME, mcz_sweng$Area_km2
                ) %>% lapply(htmltools::HTML),
                # label = paste(mcz_sweng$MCZ_NAME, "MCZ", sep = " "),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "5px"),
                  textsize = "14px",
                  direction = "auto")
                ) %>%
    # Special Areas of Conservation
    addPolygons(data = sac_sweng,
                group = "Special Areas of Conservation",
                color = "red",
                weight = 3,
                fillColor = "white",
                fillOpacity = 0.5,
                dashArray = 3,
                highlight = highlightOptions(
                  color = "#666",
                  weight = 3,
                  fillOpacity = 0.7),
                label = sprintf(
                  "<strong>%s SAC</strong><br/>Area: %g km<sup>2</sup>",
                  sac_sweng$SAC_NAME, sac_sweng$Area_km2
                ) %>% lapply(htmltools::HTML),
                # label = paste(sac_sweng$SAC_NAME, "SAC", sep = " "),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "5px"),
                  textsize = c("14px"),
                  direction = "auto")
                ) %>%
    # Special Protection Areas
    addPolygons(data = spa_sweng,
                group = "Special Protection Areas",
                color = "yellow",
                weight = 3,
                fillColor = "white",
                fillOpacity = 0.5,
                dashArray = 3,
                highlight = highlightOptions(
                  color = "#666",
                  weight = 3,
                  fillOpacity = 0.7,
                  sendToBack =  TRUE),
                label = sprintf(
                  "<strong>%s SPA</strong><br/>Area: %g km<sup>2</sup>",
                  spa_sweng$SPA_NAME, spa_sweng$Area_km2
                ) %>% lapply(htmltools::HTML),
                # label = paste(spa_sweng$SPA_NAME, "SPA", sep = " "),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "5px"),
                  textsize = "14px",
                  direction = "auto")
                ) %>%
  # Bristol Channel Approaches SAC (harbour porpoise)
  addPolygons(data = sac_porpoise,
              group = "Bristol Channel Approaches SAC",
              color = "orange",
              weight = 3,
              fillColor = "white",
              fillOpacity = 0.5,
              dashArray = 3,
              highlight = highlightOptions(
                color = "#666",
                weight = 3,
                fillOpacity = 0.7,
                sendToBack =  TRUE),
              label = sprintf(
                "<strong>%s SAC</strong><br/>Area: %g km<sup>2</sup>",
                sac_porpoise$SAC_NAME, sac_porpoise$Area_km2
              ) %>% lapply(htmltools::HTML),
              # label = paste(sac_porpoise$SAC_NAME, "SAC", sep = " "),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "5px"),
                textsize = "14px",
                direction = "auto")
  ) %>%
    # Legends
    addLegend(position = "bottomright", group = "Special Areas of Conservation",
              labels = c("Special Area of Conservation"), colors = c("red")) %>% 
    addLegend(position = "bottomright", group = "Marine Conservation Zones",
              labels = c("Marine Conservation Zone"), colors = c("blue")) %>% 
    addLegend(position = "bottomright", group = "Special Protection Areas",
              labels = c("Special Protection Area"), colors = c("yellow")) %>% 
    addLegend(position = "bottomright", group = "Bristol Channel Approaches SAC",
              labels = c("Bristol Channel Approaches SAC"), colors = c("orange"))
    # addLegend(position = "bottomleft", title = "MPA Designation",
    #           group = "Marine Protected Areas",
    #           labels = c("Marine Conservation Zone","Special Area of Conservation", "Special Protection Area"),
    #           colors = c("blue","red","yellow"))


# Define labels for each substrate type
# substrate_labs = paste(
#   "Subtrate:", "<br/>",
#   unique(seamap_sub$Substrate)
# ) %>% lapply(htmltools::HTML)

# Define colour palette for substrate layer
factpal = colorFactor("Dark2", domain = seamap_sub$Substrate)

# Add UKSeaMap substrate data to basemap
l = l %>% 
  # Add polygons
  addPolygons(data = seamap_sub, stroke = TRUE, weight = 0.1, color = "white", opacity = 1,
              # label = substrate_labs,
              fillOpacity = 0.7, fillColor = ~factpal(Substrate),
              group = "Broad-Scale Substrate Types"
              ) %>% 
  # Add legend
  addLegend(pal = factpal, values = seamap_sub$Substrate, opacity = 0.7,
            title = "Broad-Scale Substrate Types", position = "bottomleft",
            group = "Broad-Scale Substrate Types"
            )

# Define distinct colours using randomcoloR package
# substrate_cols = distinctColorPalette(k = length(unique(seamap_sub$Substrate)))
### ADD DATASETS HERE
# Define colour palette for JNCC layer
jncc_cols = distinctColorPalette(k = length(unique(seamap_sub$JNCCName)))

# Show colours using scales package
# library(scales)
# show_col(jncc_cols)

# Add UKSeaMap JNCC broad habitat data to basemap
l = l %>% 
  # Add polygons
  addPolygons(data = seamap_sub, stroke = TRUE, weight = 0.1, color = "white", opacity = 1,
              # label = substrate_labs,
              fillOpacity = 0.7, fillColor = jncc_cols,
              # highlight = highlightOptions(color = "#666",
              #                              weight = 1,
              #                              fillOpacity = 0.7,
              #                              sendToBack =  TRUE),
              group = "Broad-Scale Seabed Habitats"
  ) %>% 
  # Add legend
  addLegend(colors = jncc_cols, labels = unique(seamap_sub$JNCCName), opacity = 0.7,
            title = "Broad-Scale Seabed Habitats", position = "bottomleft",
            group = "Broad-Scale Seabed Habitats"
  )


# Layers control
l = l %>% addLayersControl(
      baseGroups = c("Open Street Map", "ESRI World Topo Map", "ESRI World Imagery",
                     "ESRI Ocean Basemap"),
      overlayGroups = c("Broad-Scale Substrate Types", "Broad-Scale Seabed Habitats",
                        "Special Protection Areas",
                        "Marine Conservation Zones", "Special Areas of Conservation",
                        "Bristol Channel Approaches SAC"),
      options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(c("Broad-Scale Substrate Types","Broad-Scale Seabed Habitats","Bristol Channel Approaches SAC"))


# ----------------- #
#
# Define server (Shiny) ####
#
# ----------------- #

server = function(input, output, session){
  # Leaflet map
  output$map1 = renderLeaflet({ l })
  # Table
  output$tableDT = DT::renderDataTable(feature_table,
                                       rownames = FALSE,
                                       filter = "top",
                                       extensions = 'Buttons',
                                       options = list(
                                         paging = FALSE,
                                         dom = "Bfrtip",
                                         buttons = list("copy",
                                                        list(
                                           extend = "collection",
                                           buttons = c("csv", "excel", "pdf"),
                                           text = "Download")
                                            )
                                         )
                                       )
}


# ----------------- #
#
# Define user interface (Shiny) ####
#
# ----------------- #

# ui = fluidPage(
#   theme = shinytheme("cerulean"),
#   titlePanel("Cornwall Inshore Substrate and MPA Map"),
#   leafletOutput("map1", height = "90vh")
# )

# Date updated
date_updated = format(Sys.Date(), "%d %B %Y")

ui = bootstrapPage(

  navbarPage("Devon, Cornwall & Isles of Scilly Inshore MPAs", theme = shinytheme("cerulean"), collapsible = TRUE,
             # Interactive map
             tabPanel("Map",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("map1", width="100%", height="100%")
                )
              ),
             # Interactive table
             tabPanel("Features",
                      DT::dataTableOutput("tableDT"),
                      # downloadButton(outputId = "download_table", label = "Download Table")
                      ),
             # About
             tabPanel("About",
                      tags$div(
                        tags$h4("About this app"),
                        "This app provides an interactive framework for users to visualise and retrieve information
                        about inshore Marine Protected Areas (MPAs) designated around Devon, Cornwall and the Isles of Scilly.
                        The Map contains layers for all types of MPAs (MCZs, SACs and SPAs) designated as of April 2021, as well as two layers
                        that show broad-scale substrate types and seabed habitats. The Features table contains
                        information about the feature(s) each MPA has been designated to protect and is intended
                        to enable users to quickly find out which MPAs cover a particular feature of interest or vice versa.",
                        "Click",
                          tags$a(href = "https://tomjenkins.shinyapps.io/devon_cornwall_scilly_mpa_app/",
                               "here"),
                        "to access this app.",
                        tags$br(), tags$br(),
                        tags$h4("Data sources"),
                        "JNCC: ", tags$a(href="https://jncc.gov.uk/our-work/marine-habitat-data-product-ukseamap/
", "UKSeaMap 2018 v1"),
                        tags$br(),
                        "Natural England: ", tags$a(href="https://naturalengland-defra.opendata.arcgis.com/datasets/marine-conservation-zones-england/data?page=7
", "Marine Conservation Zones (England)"),
                        tags$br(),
                        "Natural England: ", tags$a(href="https://naturalengland-defra.opendata.arcgis.com/datasets/e4142658906c498fa37f0a20d3fdfcff_0/data?geometry=-45.948%2C48.051%2C43.085%2C57.329&orderBy=SAC_AREA
", "Special Areas of Conservation (England)"),
                        tags$br(),
                        "Natural England: ", tags$a(href="https://naturalengland-defra.opendata.arcgis.com/datasets/special-protection-areas-england/data
", "Special Protection Areas (England)"),
                        tags$br(),
                        "Office for National Statistics: ", tags$a(href="https://geoportal.statistics.gov.uk/datasets/b216b4c8a4e74f6fb692a1785255d777_0/data?geometry=-24.112%2C48.109%2C17.702%2C52.990&orderBy=ctyua19nm&page=4
", "Counties and Unitary Authorities (December 2019) Boundaries UK BUC"),
                        tags$br(), tags$br(),
                        tags$h4("Code"),
                        "R code used to generate this Shiny app is available on ",
                        tags$a(href="https://github.com/Tom-Jenkins/shiny_mpa_app", "GitHub"),
                        tags$br(), tags$br(),
                        tags$h4("Author"), 
                        tags$a(href="https://tomjenkins.netlify.app/", "Tom Jenkins"), tags$br(),
                        "tom.l.jenkins@outlook.com (suggestions / comments welcome)",
                        tags$br(), tags$br(),
                        tags$h4("Last updated"),
                        paste0(date_updated),
                        )
                      )
             )
)

shinyApp(ui, server)

