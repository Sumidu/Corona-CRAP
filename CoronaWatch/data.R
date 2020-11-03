# this file aims to provide the data used in the other files
library(tidyverse)
library(lubridate)

dataRKI <- function(cache_time = "12h") {
  cache_file <- "rkidatacache.rds"
  raw <- NULL
  
  get_save_cache <- function() {
    message("Refreshed data from online source.")
    raw <- read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data") 
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
  
  raw %>% 
    mutate(Meldedatum = as_date(Meldedatum)) %>% 
    mutate(Refdatum = as_date(Refdatum)) %>% 
    mutate(Altersgruppe = factor(Altersgruppe, levels = c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt")))
}


ageGroupLabels <- function(df) {
  df %>% mutate(Altersgruppe = factor(Altersgruppe, levels = c("A00-A04", "A05-A14", "A15-A34",
                                                               "A35-A59", "A60-A79", "A80+", "unbekannt"),
                                      labels = c("Kleinkinder (0-4)", "Kinder (5-14)", 
                                                 "Junge Erwachsene (15-34)", "Erwachsene (35-59)", 
                                                 "Senioren (60-79)", "Ältere (80+)", "unbekannt")))
}


dataPopulation <- function(){
  # read population data ----
  de_DE <- locale("de", decimal_mark = ",")
  
  parse_chr_xls <- function(x) {
    str_remove_all(x, " ") %>% parse_number(locale = de_DE)
  }
  
  population <- read_csv2(here::here("kreise.csv")) %>% 
    mutate_at(c("Flaeche", "Gesamt", "Maenner", "Frauen", "Dichte"), parse_chr_xls) 
  
  population
}


dailyCases <- function(df){
  df %>% group_by(Meldedatum) %>% 
    summarise(AnzahlFall = sum(AnzahlFall),
              AnzahlTodesfall = sum(AnzahlTodesfall),
              AnzahlGenesen = sum(AnzahlGenesen)) %>% ungroup()
}


dailyCasesAlter <- function(df){
  df %>% group_by(Altersgruppe, Meldedatum) %>% 
    summarise(AnzahlFall = sum(AnzahlFall),
              AnzahlTodesfall = sum(AnzahlTodesfall),
              AnzahlGenesen = sum(AnzahlGenesen)) %>% ungroup()
}



dailyCasesAlterLandkreis <- function(df){
  df %>% group_by(IdBundesland, Bundesland, IdLandkreis, Landkreis, Altersgruppe, Meldedatum) %>% 
    summarise(AnzahlFall = sum(AnzahlFall),
              AnzahlTodesfall = sum(AnzahlTodesfall),
              AnzahlGenesen = sum(AnzahlGenesen)) %>% ungroup()
}




#read_delim("lk_age_pop_flat.csv", delim = ";") %>% select(`1_Auspraegung_Code`, BEVSTD__Bevoelkerungsstand__Anzahl)





