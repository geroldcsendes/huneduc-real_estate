library(rvest)

base_url <- "https://ingatlan.com/lista/elado+lakas"

# different user agent
httr::user_agent(uastring)
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

t <- read_html(myres)
t %>% 
  html_nodes(".price") %>% 
  html_text()

t %>% 
  html_nodes(".listing__address") %>% 
  html_text()

t %>% 
  html_nodes(".listing__data--area-size") %>% 
  html_text()


myres <- httr::GET("https://ingatlan.com/lista/elado+lakas+Budapest", user_agent("httr"),
                   query = list(
                     page = 1))

t <- read_html(myres)
t %>% 
  html_nodes(".price") %>% 
  html_text()

t %>% 
  html_nodes(".listing__address") %>% 
  html_text()

t %>% 
  html_nodes(".listing__data--area-size") %>% 
  html_text()


scrape_real_estate <- function(page) {
  
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
  Sys.sleep(2) # doze some time
  return (df)
}


# tempres <- scrape_real_estate(page = 2000000)
real_estate_df <- lapply(1:600, scrape_real_estate)
real_estate_df <- rbindlist(real_estate_df)

# munge real estate data
real_estate_df <- real_estate_df %>% 
  # factors to strings
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, trimws) %>% 
  mutate(price = str_replace_all(price, " ", ""),
         price_sqm = str_replace_all(price_sqm, " ", ""),
         area = str_replace_all(area, " ", "")) %>% 
  # remove characters like M Ft from the end of strings
  mutate(price = as.numeric(substr(price, 1, nchar(price) - 3)),
         price_sqm = as.numeric(substr(price_sqm, 1, nchar(price_sqm) - 5)),
         area = as.numeric(substr(area, 1, nchar(area) - 9))) %>% 
  # filter for Budapest
  filter(str_detect(tolower(address), "kerület") | (str_detect(tolower(address), "budapest")))

write.csv(real_estate_df, "data/processed/real_estate_df.csv", row.names = F)

# later add links too
t <- read_html(myres)
myurls <- t %>% 
  html_nodes(".js-listing-active-area") %>% 
  html_attr('href')

t %>% 
  html_nodes(".price") %>% 
  html_text()
