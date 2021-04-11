# --------------------------- #
#
# Devon, Cornwall & Isles of Scilly MPA Shiny App
#
# Description:
# Prepare spatial data for shiny app
#
# Author: Tom Jenkins
#
# --------------------------- #

# Load packages
library(sf)
library(tidyverse)
library(rmapshaper)
library(pryr)


# ----------------- #
#
# Prepare boundary
#
# ----------------- #

# Data link:
# https://geoportal.statistics.gov.uk/datasets/b216b4c8a4e74f6fb692a1785255d777_0/data?geometry=-24.112%2C48.109%2C17.702%2C52.990&orderBy=ctyua19nm&page=4

# Import shapefile 
county = file.path("county_boundaries/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp") %>% st_read()
st_is_valid(county)
names(county)
str(county)
"Devon" %in% county$ctyua19nm
"Plymouth" %in% county$ctyua19nm
"Cornwall" %in% county$ctyua19nm
"Isles of Scilly" %in% county$ctyua19nm

# Select only names and geometry
county = county %>% dplyr::select(ctyua19nm)
plot(county)

# Extract geometry data
sweng_names = c("Devon","Plymouth","Cornwall","Isles of Scilly")
sweng = county %>% 
  dplyr::filter(ctyua19nm %in% sweng_names)
sweng
plot(sweng)

# Combine into one polygon
sweng = st_union(sweng)
# sweng = st_combine(sweng)
plot(sweng)


# ----------------- #
#
# Marine Conservation Zones
#
# ----------------- #

# Data link:
# https://naturalengland-defra.opendata.arcgis.com/datasets/marine-conservation-zones-england/data?page=7

# Import shapefile 
mcz = file.path("protected_area_data/Marine_Conservation_Zones___Natural_England_and_JNCC.shp") %>% st_read()
st_is_valid(mcz)
mcz = st_make_valid(mcz)
names(mcz)
str(mcz)

# Simplify polygons using rmapshaper
mcz_simpl = ms_simplify(mcz, keep_shapes = TRUE)
object_size(mcz)
object_size(mcz_simpl)

# Extract polygons which are within 6 nautical miles (11112m) of the boundary
mcz_sweng = mcz_simpl[st_is_within_distance(sweng, mcz_simpl, dist = 11112)[[1]], ] %>%
  dplyr::select(MCZ_NAME, SHAPE_Area)
mcz_sweng
object_size(mcz_sweng)
dplyr::select(mcz_sweng, MCZ_NAME) %>% plot

# Convert metres squared to kilometres squared
mcz_sweng = mcz_sweng %>% 
  mutate(SHAPE_Area_km2 = round(SHAPE_Area / 1000000, digits = 1))

# Extract only the MCZ name and geometry then transform to WGS84 CRS (EPSG = 4326)
mcz_sweng = mcz_sweng %>% 
  st_transform(crs = 4326)
mcz_sweng

# Merge polygons from the same MCZ
mcz_sweng = mcz_sweng %>% group_by(MCZ_NAME) %>% summarise(Area_km2 = sum(SHAPE_Area_km2))
mcz_sweng

# Export MCZ RData
save(mcz_sweng, file = "../devon_cornwall_scilly_mpa_app/data/mcz_sweng.RData")


# ----------------- #
#
# Special Areas of Conservation
#
# ----------------- #

# Data link:
# https://naturalengland-defra.opendata.arcgis.com/datasets/e4142658906c498fa37f0a20d3fdfcff_0/data?geometry=-45.948%2C48.051%2C43.085%2C57.329&orderBy=SAC_AREA

# Import shapefile 
sac = file.path("protected_area_data/Special_Areas_of_Conservation__England____Natural_England.shp") %>% st_read()
st_is_valid(sac) %>% summary
sac = st_make_valid(sac)
names(sac)
str(sac)

# Simplify polygons using rmapshaper
sac_simpl = ms_simplify(sac, keep_shapes = TRUE)
object_size(sac)
object_size(sac_simpl)

# Extract polygons which overlap with the sweng boundary
sac_sweng = sac_simpl[st_overlaps(sweng, sac_simpl)[[1]], ] %>% 
  dplyr::select(SAC_NAME, SHAPE_Area)
sac_sweng
unique(sac_sweng$SAC_NAME)

# Convert metres squared to kilometres squared
sac_sweng = sac_sweng %>% 
  mutate(SHAPE_Area_km2 = round(SHAPE_Area / 1000000, digits = 1))
sac_sweng

# Create individual layer for Bristol Channge Approaches (harbour porpoise)
sac_porpoise = sac_sweng %>% 
  dplyr::filter(SAC_NAME == "Bristol Channel Approaches / Dynesfeydd Mor Hafren") %>% 
  group_by(SAC_NAME) %>%
  summarise(Area_km2 = sum(SHAPE_Area_km2)) %>% 
  st_transform(crs = 4326)
sac_porpoise
plot(sac_porpoise)
object_size(sac_porpoise)

# Remove some SACs 
sac_sweng = sac_sweng %>% 
  dplyr::filter(SAC_NAME != "Bristol Channel Approaches / Dynesfeydd Mor Hafren" & SAC_NAME != "The Lizard" & SAC_NAME != "Exmoor Heaths" & SAC_NAME != "Exmoor & Quantock Oakwoods")
unique(sac_sweng$SAC_NAME)

# Extract only the sac name and geometry then transform to WGS84 CRS (EPSG = 4326)
sac_sweng = sac_sweng %>% 
  st_transform(crs = 4326)
sac_sweng

# Merge polygons from the same SAC
sac_sweng = sac_sweng %>% group_by(SAC_NAME) %>% summarise(Area_km2 = sum(SHAPE_Area_km2))
sac_sweng

# Export SAC RData
save(sac_sweng, file = "../devon_cornwall_scilly_mpa_app/data/sac_sweng.RData")
save(sac_porpoise, file = "../devon_cornwall_scilly_mpa_app/data/sac_porpoise.RData")


# ----------------- #
#
# Special Protection Areas
#
# ----------------- #

# Data link:
# https://naturalengland-defra.opendata.arcgis.com/datasets/special-protection-areas-england/data

# Import shapefile 
spa = file.path("protected_area_data/Special_Protection_Areas__England____Natural_England.shp") %>% st_read()
st_is_valid(spa) %>% summary
spa = st_make_valid(spa)
names(spa)
str(spa)

# Simplify polygons using rmapshaper
spa_simpl = ms_simplify(spa, keep_shapes = TRUE)
object_size(spa)
object_size(spa_simpl)

# Extract polygons which overlap with the sweng boundary
spa_sweng = spa_simpl[st_overlaps(sweng, spa_simpl)[[1]], ] %>% 
  dplyr::select(SPA_NAME, SHAPE_Area)
spa_sweng
unique(spa_sweng$SPA_NAME)

# Convert metres squared to kilometres squared
spa_sweng = spa_sweng %>% 
  mutate(SHAPE_Area_km2 = round(SHAPE_Area / 1000000, digits = 1))
spa_sweng

# Extract only the spa name and geometry then transform to WGS84 CRS (EPSG = 4326)
spa_sweng = spa_sweng %>% 
  st_transform(crs = 4326)
plot(spa_sweng)

# Merge polygons from the same SPA
spa_sweng = spa_sweng %>% group_by(SPA_NAME) %>% summarise(Area_km2 = sum(SHAPE_Area_km2))
spa_sweng

# Export spa RData
save(spa_sweng, file = "../devon_cornwall_scilly_mpa_app/data/spa_sweng.RData")


# ----------------- #
#
# JNCC Marine Habitat Data (UKSeaMap 2018 v1)
#
# ----------------- #

# Data link:
# https://jncc.gov.uk/our-work/marine-habitat-data-product-ukseamap/

# Print layers of geodatabase
st_layers("C20181212_UKSeaMap2018_WGS84/C20181212_UKSeaMap2018_WGS84.gdb")

# Import geodatabase layer
seamap = st_read(dsn = "C20181212_UKSeaMap2018_WGS84/C20181212_UKSeaMap2018_WGS84.gdb",
                 layer = "C20181217_UKSeaMap2018_WGS84")
seamap
names(seamap)
st_crs(seamap)
seamap_valid = st_make_valid(seamap)

# Crop data set to extent
st_transform(sweng, crs = 4326) %>% st_bbox()
box = c(xmin = -6.85, xmax = -2.886641, ymin = 49.65, ymax = 51.40)
seamap_crop = st_crop(seamap_valid, box)
seamap_crop

# Simplify polygons using rmapshaper
# Shapes not kept because output file too large (> 1.5 GB)
seamap_simpl = ms_simplify(seamap_crop, keep_shapes = FALSE)
object_size(seamap_crop)
object_size(seamap_simpl)

# Filter data
seamap_sub = seamap_simpl %>% 
  dplyr::select(Substrate, EUNIScombD, MSFDBBHT, JNCCName) %>%
  dplyr::filter(Substrate != "Seabed")
  # dplyr::select(EUNIScombD)
  # dplyr::select(JNCCName)
  # dplyr::select(MSFDBBHT)
plot(seamap_sub)
object_size(seamap_sub)

# Remove NAs
seamap_sub = seamap_sub %>% drop_na
seamap_sub

# Export RData
save(seamap_sub, file = "../devon_cornwall_scilly_mpa_app/data/seamap_sweng.RData")


# ----------------- #
#
# Static map
#
# ----------------- #

# Plot static map of MPAs
ggplot()+
  geom_sf(data = st_transform(county, crs = 4326))+
  geom_sf(data = mcz_sweng, fill = "white", alpha = 0.7, colour = "blue")+
  geom_sf(data = sac_sweng, fill = "white", alpha = 0.7, colour = "red")+
  geom_sf(data = spa_sweng, fill = "white", alpha = 0.7, colour = "purple")+
  coord_sf(xlim = c(-6,-4), ylim = c(49.9,51.2))

# Plot static map of UKSeaMap data
ggplot()+
  geom_sf(data = st_transform(county, crs = 4326))+
  geom_sf(data = seamap_sub, aes(fill = JNCCName))+
  coord_sf(xlim = c(-6,-4), ylim = c(49.9,51.2))

