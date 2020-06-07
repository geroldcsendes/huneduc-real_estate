library(rvest)
source("src/helpers.R")

base_url <- "https://ingatlan.com/lista/elado+lakas"

# different user agent
httr::user_agent(uastring)
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

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


real_estate_df <- geocode_df(real_estate_df, address_var = "address")

write.csv(real_estate_df, "data/processed/real_estate_df.csv", row.names = F)



# later add links
t <- read_html(myres)
myurls <- t %>% 
  html_nodes(".js-listing-active-area") %>% 
  html_attr('href')

t %>% 
  html_nodes(".price") %>% 
  html_text()

# PLAYGROUND
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

