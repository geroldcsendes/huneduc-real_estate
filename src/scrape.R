library(rvest)

base_url <- "https://ingatlan.com/lista/elado+lakas"

t <- read_html("https://ingatlan.com/lista/elado+lakas?page=2")


se <- html_session( "https://httpbin.org/user-agent", user_agent("httr"))
se$response$request$options$useragent

# different user agent
httr::user_agent(uastring)
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"


se_changed <- GET("http://httpbin.org/user-agent", user_agent("httr"))
se_changed$response$request$options$useragent

myres <- httr::GET("https://ingatlan.com/lista/elado+lakas?page=2", user_agent("httr"))

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
                     page = 3))

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
  return (df)
  Sys.sleep(2) # doze some time
}


# tempres <- scrape_real_estate(page = 2000000)
real_estate_df <- lapply(1:600, scrape_real_estate)
real_estate_df <- rbindlist(real_estate_df)

# temp save
#write.csv(real_estate_df, "data/processed/real_estate_df.csv", row.names = F)

# munge real estate data
trimws(real_estate_df$price)