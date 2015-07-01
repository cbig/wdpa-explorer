library(countrycode)
library(dplyr)

source("config.R")

wdpa_db <- src_postgres(dbname = DBNAME, host = HOST, port = PORT, user = USER,
                        password = PASSWORD, options="-c search_path=wdpa")

wdpa_poly <- tbl(wdpa_db, "wdpa_poly")
wdpa_point <- tbl(wdpa_db, "wdpa_point")

summarize_land_pa_per_iso3 <- function(data){
  summary_data <- data %>%
    filter(marine == "0") %>% 
    group_by(iso3, iucn_cat, status) %>%
    summarise(
      count = n(),
      area_km = sum(rep_area)
    ) %>%
    ungroup() %>% 
    as.data.frame()
  return(summary_data)
}

pa_per_iso3_poly <- summarize_pa_per_iso3(wdpa_poly)
pa_per_iso3_poly$type <- "polygon"
pa_per_iso3_point <- summarize_pa_per_iso3(wdpa_point) 
pa_per_iso3_point$type <- "point"

pa_per_iso3 <- bind_rows(pa_per_iso3_poly, pa_per_iso3_point)

# Get real names
country_names <- countrycode(pa_per_iso3$iso3, "iso3c", "country.name")
for (i in 1:length(country_names)){
  if (is.na(country_names[i])) {
    country_names[i] <- pa_per_iso3$iso3[i]
  } else {
    country_names[i] <- paste0(pa_per_iso3$iso3[i], " (", country_names[i],
                               ")")
  }
}

pa_per_iso3$iso3_country_name <- country_names

pa_per_iso3 <- pa_per_iso3 %>% 
  select(iso3_country_name, type, iucn_cat, status, count, area_km) %>% 
  arrange(iso3_country_name, type, iucn_cat)

pa_cat_global <- pa_per_iso3 %>% 
  group_by(iucn_cat, type) %>% 
  summarise(
    count = sum(count),
    area_km = sum(area_km)
  ) %>% 
  ungroup() %>% 
  mutate(iso3_country_name = "Global") %>% 
  select(iso3_country_name, type, iucn_cat, count, area_km)

pa_stat_global <- pa_per_iso3 %>% 
  group_by(status) %>% 
  summarise(
    count = sum(count),
    area_km = sum(area_km)
  ) %>% 
  ungroup() %>% 
  mutate(perc = round(area_km / sum(area_km) * 100, 2), 
         iso3_country_name = "Global") %>% 
  select(iso3_country_name, status, count, area_km, perc)
