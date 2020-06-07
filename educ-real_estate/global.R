library(tidyverse)
library(geosphere)

yearly_educ <- function(year_sel, district_sel, okm_df, coords_df, grade) {
  
  res <- okm_df %>% 
    filter(year == year_sel) %>% # TODO this is harcdoed
    mutate(m_decile = ntile(m_stat,10),
           o_decile = ntile(o_stat, 10)) %>% 
    mutate(educ_index = (m_decile + o_decile) / 2) %>% 
    group_by(omid, year) %>% 
    summarise(educ_index = mean(educ_index, na.rm = T),
              m_stat = mean(m_stat, na.rm = T),
              o_stat = mean(o_stat, na.rm = T)) %>% 
    mutate(pooled_stat = (m_stat + o_stat ) / 2) %>% 
    filter(!is.na(educ_index))
  
  res <-  inner_join(res, coords_df, by = "omid")
  
  if (district_sel != "ALL") {
    res <- res %>% 
      filter(district == district_sel)
  } else {
  }
  
  return(res)
}

# dashboard second Pane
# list houses within a price range within a distance of a CHOSEN school
houses_within_price_dist <- function(price_low, price_high, isk_sel, real_estate_df, coords_df) {
  
  DISTANCE <- 3000 # distance to track from school
  
  # extract school coordinates
  school <- coords_df %>% 
    filter(isk_id == isk_sel)
  
  school_lat <- school$lat
  school_lon <- school$lon
  
  # filter for price range
  priced_real_estate <- real_estate_df %>% 
    filter(price <= price_high, price >= price_low)
  
  # initiate distance column
  priced_real_estate$dist <- c()
  
  # calculate distance for each real_estate
  for (row in seq(1, nrow(priced_real_estate))) {
    estate_lat <-  priced_real_estate[row, "lat"]
    estate_lon <- priced_real_estate[row, "lon"]
    
    priced_real_estate[row, "dist"] <- distm(
      c(school_lon, school_lat), c(estate_lon, estate_lat), fun = distHaversine)
  }
  
  # filter for those within 3 km range
  houses_within <- priced_real_estate %>% 
    filter(dist <= DISTANCE)
  
  return(houses_within)
}
