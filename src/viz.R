library(tidyverse)
library(leaflet)
library(geosphere)
source("educ-real_estate/global.R")

# read in data
data_path <- "data/processed/"

okm_filename <- "okm_gps.csv"
okm_gps_filename <- "okm_distinct_gps.csv"
real_estate_filename <- "real_estate_df.csv"

okm <- read.csv(paste0(data_path, okm_filename))
okm <- okm %>% arrange(omid, evfolyam)

coords_df <- read.csv(paste0(data_path, okm_gps_filename))

real_estate <- read.csv(paste0(data_path, real_estate_filename))

# try out funcs
myviz_df <- yearly_educ(year_sel = 2018, district_sel = "1. kerület", okm_df = okm, coords_df = coords_df)


# filter for one year
year <- 2018
okm_grouped <- okm %>% 
  filter(year == 2018) %>% # TODO this is harcdoed
  mutate(m_decile = ntile(m_stat,10),
         o_decile = ntile(o_stat, 10)) %>% 
  mutate(educ_stat = (m_decile + o_decile) / 2) %>% 
  group_by(omid) %>% 
  summarise(educ_stat = mean(educ_stat, na.rm = T),
            m_stat = mean(m_stat, na.rm = T),
            o_stat = mean(o_stat, na.rm = T)) %>% 
  filter(!is.na(educ_stat))

# get distinct omids and coords
# coords_df <- okm %>% 
#   select(omid, lat, lon, nev_isk) %>% 
#   group_by(omid) %>% 
#   filter(row_number(omid) == 1) %>% 
#   filter(!is.na(lat))

myviz_df <- inner_join(okm_grouped, coords_df, by = "omid")

inner_join(myviz_df, okm, by=c("omid", "year"))

qpal <- colorNumeric("RdYlBu", myviz_df$educ_index)

# try different tiles
leaflet(myviz_df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 19.040236, lat = 47.497913, zoom = 11) %>% 
  addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = 250,
             color = ~qpal(educ_index), fillOpacity = 0.8,
             popup = ~as.character(nev_isk), label = ~as.character(nev_isk)) %>%
  addLegend("bottomright", pal = qpal, values = ~educ_index, 
            title = "Educ", opacity = 0.85)


# viz real-estate
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "cadetblue"
)

sample_n(real_estate, 100) %>% 
  leaflet() %>% 
  addTiles() %>%
  addAwesomeMarkers(~lon, ~lat, icon=icons)


# return real estate within 5 km range
myres <- houses_within_price_dist(price_low = 35, price_high = 40, isk_sel = "Budapest I. Kerületi Batthyány Lajos Általános Iskola - 1015", 
                                  real_estate_df = real_estate, coords_df = coords_df)


leaflet(myres) %>% 
addTiles() %>%
addAwesomeMarkers(~lon, ~lat, icon=icons,
                popup = paste("Address:", myres$address, "<br>",
                              "Price:", myres$price, "<br>",
                              "Price sqm:", myres$price_sqm, "<br>"))




# color icons
leaflet(myviz_df) %>% addTiles() %>%
  addAwesomeMarkers(~lon, ~lat, icon=icons)


# adding circles: alternative to markers
qpal <- colorQuantile("YlOrRd", myviz_df$educ_index, n = 3)
leaflet(myviz_df) %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = 300,
             color = ~qpal(educ_index), fillOpacity = 0.8,
             popup = ~as.character(omid), label = ~as.character(omid)) %>%
  addLegend("bottomright", pal = qpal, values = ~educ_index, 
            title = "Pop", opacity = 1)



# Markers

getColor <- function(viz_df) {
  sapply(viz_df$educ_stat, function(educ_stat) {
    if(educ_stat <= 4) {
      "#67001f"
    } else if(educ_stat <= 5) {
      "#67001f"
    } else {
      "#67001f"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(myviz_df)  #"cadetblue"
)

# color icons
leaflet(myviz_df) %>% addTiles() %>%
  addAwesomeMarkers(~lon, ~lat, icon=icons)


leaflet(myviz_df) %>% addTiles() %>%
  addMarkers(~lon, ~lat, icon=icons)

# visualize on map
leaflet(myviz_df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, popup = ~as.character(omid), label = ~as.character(omid))


# sample viz
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= 19.03568, lat=47.50678, popup="The birthplace of R")

#
missing_omid <- okm_grouped %>% filter(is.na(educ_stat))
okm %>% filter(year == 2018) %>% 
  filter(omid %in% missing_omid$omid)
