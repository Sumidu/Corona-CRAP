# test to download local data from aachen

library(tidyverse)
library(rvest)
library(lubridate)



dataAachen <- function(cache_time = "4h") {
  cache_file <- "aachendatacache.rds"
  raw <- NULL
  
  get_save_cache <- function() {
    message("Refreshed data from online source.")
    raw <- get_aachen_data()
    write_rds(x = raw, file = cache_file)  
  }
  
  if (file.exists(cache_file)) {
    mod_date <- file.info(cache_file)$mtime
    now <- now()
    if (!(now - mod_date) < lubridate::as.duration(cache_time)) {
      get_save_cache()
    }
    message("Using cached data.")
    raw <- read_rds(cache_file)
  } else {
    get_save_cache()
  }
  raw
}

get_aachen_data_urls <- function() {
  url <-
    "http://www.aachen.de/DE/stadt_buerger/politik_verwaltung/pressemitteilungen/index.html"
  
  txt <- read_html(url)
  
  
  txt %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    tibble() -> urls
  
  names(urls) <- c("url")
  
  full_urls <-
    urls %>% dplyr::filter(str_detect(url, "corona_[0-9]+\\.")) %>%
    mutate(
      url = paste0(
        "http://www.aachen.de/DE/stadt_buerger/politik_verwaltung/pressemitteilungen/",
        url
      )
    )
  full_urls
}

get_aachen_data <- function() {

  full_urls <- get_aachen_data_urls()  
  all_data <- data.frame()
  
  for (i in 1:nrow(full_urls)) {
    #i = 1
    message_url <- full_urls[i, ]$url
    temp_txt <- read_html(message_url)
    
    
    data_day <- str_match(message_url, "[0-9]+")
    
    if (str_length(data_day) == 6) {
      full_day <- ymd(data_day)
    } else {
      full_day <- dmy(data_day)
    }
    
    if (full_day == dmy("20-09-2021")) {
      full_day <- dmy("21-09-2020")
    }
    
    print(full_day)
    
    all_tables <-
      temp_txt %>% html_nodes("table") %>% rvest::html_table(header = TRUE, fill = TRUE)
    
    if (length(all_tables) > 0) {
      tl <- length(all_tables)
      all_data <-
        all_tables[[tl]] %>%
        select(-starts_with("Sieben-Tage-Inzidenz")) %>%
        select(-starts_with("Gesamt")) %>%
        #filter(Kommune != "Gesamtergebnis") %>%
        pivot_longer(cols = c(-Kommune)) %>% mutate(date = full_day) %>%
        bind_rows(all_data)
    }
    
  }
  all_data
}
  







