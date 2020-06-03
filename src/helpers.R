library(tidyverse)
library(data.table)
library(readxl)

# okm tranformation and filter for Budapest
okm_transform <- function(path, sheet, keep_cols, year) {
  
  # read in data
  data <- read_excel(path = path, sheet = sheet, skip = 1) 
  
  # lowercase colnames
  names(data) <- tolower(names(data))
  
  # FILTER for Budapest
  data <- data %>% 
    filter(startsWith(telepules_isk, "Budapest"))
  
  # assign Budapest to city names like Budapest XX. kerület	
  data$telepules_isk <- c("Budapest")
  
  # select relevant cols
  data <- data %>% 
    select(keep_cols)
  
  print(head(data))
  
  # assign year
  data$year <- c(year)
  
  return(data)
}

# iterate over years
get_okm_data <- function(year) {
  
  print(year)
  
  data_lib <- "data/"
  
  main_lib <- paste0(data_lib, year, "_OKM/")
  sub_lib <- paste0("OKM_", year, "_jelentesfajlok/")
  filename <- paste0("intezmenyi adatok ", year, ".xlsx")
  
  data_path <- paste0(main_lib, sub_lib, filename)
  
  print(data_path)
  
  grade6_sheet <-  paste0("intézményi adat, 6. évf., ", year)
  grade8_sheet <-  paste0("intézményi adat, 8. évf., ", year)
  grade10_sheet <-  paste0("intézményi adat, 10. évf., ", year)
  
  # define which columns to keep
  keep_cols <- c("omid", "evfolyam",  "nev_isk", "irsz_isk", "telepules_isk", "utca_isk", "fenntarto_kod",
                 "m_stat", "m_se", "o_stat", "o_se")
  
  grade6_df <- okm_transform(path = data_path, sheet = grade6_sheet, 
                             keep_cols = keep_cols, year = year)
  
  grade8_df <- okm_transform(path = data_path, sheet = grade8_sheet, 
                             keep_cols = keep_cols, year = year)
  
  grade10_df <- okm_transform(path = data_path, sheet = grade10_sheet, 
                              keep_cols = keep_cols, year = year)  
  
  return(rbind(grade6_df, grade8_df,grade10_df))
  
}

# import api secret
my_secrets <- function() {
  path <- "../here_api_key.txt"
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }
  
  read_file(path)
}

# get geocodes for df
geocode_df <- function(df, address_var) {
  
  # assign lat, lon cols
  df$lat <- c()
  df$lon <- c()
  
  for (row in 1:nrow(df)) {
    
    address <- as.character(df[row, address_var])
    
    print(row)
    print(address)
    
    tryCatch(
      {
        coordinate_object <- hereR::geocode(address)
        coordinates <- st_coordinates(coordinate_object$geometry)
        df[row, "lat"] <- coordinates[1, "Y"]
        df[row, "lon"] <-  coordinates[1, "X"]},
      
      error = function(e){
        print("not found")
      }
    )
  }
  return(df)
}

# scrape data from ingatlan.com
scrape_real_estate <- function(page) {
  
  print(page)
  
  # different user agent
  uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
  httr::user_agent(uastring)
  
  tryCatch ({
    # query server
    res <- httr::GET("https://ingatlan.com/lista/elado+lakas+Budapest", user_agent("httr"),
                     query = list(
                       page = page))
    
    # parse html
    t <- read_html(res)
    
    price <- t %>% 
      html_nodes(".price") %>% 
      html_text()
    
    price_sqm <- t %>% 
      html_nodes(".price--sqm") %>% 
      html_text()
    
    address <- t %>% 
      html_nodes(".listing__address") %>% 
      html_text()
    
    area <- t %>% 
      html_nodes(".listing__data--area-size") %>% 
      html_text()
    
    df <- data.frame(price = price,
                     price_sqm = price_sqm,
                     address = address,
                     area = area,
                     page=c(page))},
    
    error = function(e){
      print(paste("page", page, "not found"))
      df <- data.frame(page=c(page),
                       price = NA,
                       price_sqm = NA,
                       address = NA,
                       area = NA)
    }
  )
  Sys.sleep(1) # doze some time
  return (df)
}
