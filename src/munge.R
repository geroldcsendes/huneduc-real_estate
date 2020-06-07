library(tidyverse)
library(data.table)
library(readxl)
library(hereR)

source("src/helpers.R")

# read in data
years <- seq(2008,2018)

okm_data <-lapply(years, get_okm_data) 
okm_data <- rbindlist(okm_data)

# fix dtypes
okm_data$irsz_isk <- as.numeric(okm_data$irsz_isk)
okm_data$m_stat <- as.numeric(okm_data$m_stat)
okm_data$m_se <- as.numeric(okm_data$m_se)
okm_data$o_stat <- as.numeric(okm_data$o_stat)
okm_data$o_se <- as.numeric(okm_data$o_se)

# save 
# write.csv(okm_data, "data/okm.csv", row.names = F)

# okm_data <- read.csv("data/okm.csv")

# convert columns into address
okm_cim <- okm_data %>% 
  group_by(omid) %>% 
  summarise(irsz_isk=last(irsz_isk),
            telepules_isk=last(telepules_isk),
            utca_isk=last(utca_isk)) %>% 
 unite("cim", irsz_isk, telepules_isk, utca_isk, sep = " ")

okm_cim$lat <- c()
okm_cim$lon <- c()

# Geocode addresses
okm_coords <- geocode_df(okm_cim)

# find missing coords
okm_coords %>% 
  filter(is.na(lat) | is.na(lon))

# fill in missing coords
okm_coords <- okm_coords %>% 
  mutate(lat = replace(lat, omid == 200768, 47.416665),
         lon = replace(lon, omid == 200768,  19.054779))

# join gps to okm data
okm_data <- merge(okm_data, okm_coords, by = "omid", all.x = T, all.y = F)


okm_data <- read.csv("data/processed/okm_gps.csv")

# add district and isk id cols
okm_data <- okm_data %>% 
  mutate(isk_id = str_c(nev_isk, irsz_isk, sep = " - "))
         
okm_data$district <- paste0(as.numeric(substr(okm_data$irsz_isk, 2, 3)), ". kerület")
# save data
write.csv(okm_data, "data/processed/okm_gps.csv", row.names = F)


# Munge a bit more
okm_data <- read.csv("data/processed/okm_gps.csv")

coords_df <- okm_data %>% 
  arrange(omid, year) %>% 
  select(omid, lat, lon, nev_isk, irsz_isk, utca_isk) %>% 
  group_by(omid) %>% 
  summarise_all(last) %>% 
  filter(!is.na(lat)) %>% 
  mutate(isk_id = str_c(nev_isk, irsz_isk, sep = " - "))

# add kerület col
coords_df$district <- paste0(as.numeric(substr(coords_df$irsz_isk, 2, 3)), ". kerület")

write.csv(coords_df, "data/processed/okm_distinct_gps.csv", row.names = F)
