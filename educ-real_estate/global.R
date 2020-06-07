library(tidyverse)

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
