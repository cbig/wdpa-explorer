library(dplyr)

source("config.R")

wdpa_db <- src_postgres(dbname = DBNAME, host = HOST, port = PORT, user = USER,
                        password = PASSWORD, options="-c search_path=wdpa")

wdpa_poly <- tbl(wdpa_db, "wdpa_poly")
wdpa_point <- tbl(wdpa_db, "wdpa_point")

summarize_pa_per_iso3 <- function(data){
  summary_data <- data %>%
    group_by(iso3, iucn_cat) %>%
    summarise(
      count = n(),
      area_km = sum(rep_area)
    ) %>%
    ungroup() %>% 
    as.data.frame() %>% 
    group_by(iso3) %>% 
    mutate(perc = round(area_km / sum(area_km) * 100, 2)) %>% 
    arrange(iso3, iucn_cat)
  return(summary_data)
}

pa_per_iso3_poly <- summarize_pa_per_iso3(wdpa_poly)
pa_per_iso3_poly$type <- "polygon"
pa_per_iso3_point <- summarize_pa_per_iso3(wdpa_point) 
pa_per_iso3_point$type <- "point"

pa_per_iso3 <- bind_rows(pa_per_iso3_poly, pa_per_iso3_point)

pa_per_iso3 <- pa_per_iso3 %>% 
  select(iso3, type, iucn_cat, count, area_km, perc) %>% 
  arrange(iso3, type, iucn_cat)
