library(readr)
library(dplyr)
library(osmdata)
library(sf)
library(ggspatial)
library(ggplot2)

save_osm_apt <- function(
  .gg,
  .apt_icao,
  .format = ".png",
  .dir="./data-ad-charts/"){
  #filename
  fn <- paste(.apt_icao, .format, sep = "")
  # save plot
  ggsave(plot = .gg, filename = fn, path = .dir)
}

osm_apt <- function(
  .bb_lonlat,
  .title = NULL,
  .add_north = TRUE) {
  q <- opq(bbox = .bb_lonlat) %>%
    add_osm_feature(
      key = "aeroway",
      value = c(
        "aerodrome", "apron", "control_tower", "gate", "hangar",
        "helipad", "runway", "taxiway", "terminal"
      )
    ) %>%
    osmdata_sf() %>%
    unique_osmdata()

  gg <- ggplot() +
    geom_sf(
      data = q$osm_polygons,
      inherit.aes = FALSE,
      color = "lightblue"
      # ,fill  = "lightblue"
    ) +
    geom_sf(
      data = q$osm_lines %>% filter(aeroway != "runway"),
      color = "grey"
    ) +
    geom_sf(
      data = q$osm_lines %>% filter(aeroway == "runway"),
      inherit.aes = FALSE,
      color = "black",
      size = 2 # .4
      , alpha = .8
    ) +
    theme_void()

  # if(!is.null(.title)){
  #   gg <- gg + labs(title = .title)
  # }
  # add north arrow with ggspatial
  if (!is.null(.add_north)) {
    gg <- gg +
      ggspatial::annotation_north_arrow(height = unit(0.7, "cm"), width = unit(0.7, "cm"))
  }
  # return aerodrome layout map
  return(gg)
}

bb_coerce <- function(.bb_lnglat_string){
  bb <- strsplit(.bb_lnglat_string, ",")
  bb <- unlist(bb)
  bb <- as.numeric(bb)
  return(bb)
}

geom_airport <- function(apt_icao) {
  apt <- aaa %>%
    filter(APT_ICAO == apt_icao)
  apt_name <- apt["APT_NAME"]
  bb_lonlat <- apt %>% pull(BBOX) %>% bb_coerce()
  
  rwy <- rrr %>% 
    filter(airport_ident == apt_icao)
  
  q <- opq(bbox = bb_lonlat) %>% 
    add_osm_feature(
      key = "aeroway"
      ,value =c("aerodrome", "apron", "control_tower", "gate", "hangar"
                ,"helipad", "runway", "taxiway", "terminal") ) %>% 
    osmdata_sf() %>%
    unique_osmdata()
  
  gg <- osm_apt(bb_lonlat, .add_north = TRUE) +
    geom_label(data = rwy, mapping = aes(x = le_longitude_deg, y = le_latitude_deg, label = le_ident, angle = le_heading_degT)) +
    geom_label(data = rwy, mapping = aes(x = he_longitude_deg, y = he_latitude_deg, label = he_ident, angle = he_heading_degT))
  
  save_osm_apt(gg, apt_icao, .dir = "data-ad-rwy-charts")  
}

# apt_url <- "https://ourairports.com/data/airports.csv"
apt_url <- here::here("data", "airports.csv")
# rwy_url <- "https://ourairports.com/data/runways.csv"
rwy_url <- here::here("data", "runways.csv")

apdf_apts <- readxl::read_excel(
  here::here("data-ad-charts", "APT_BBOX.xlsx"),
  sheet = "Monitored")

apts_pru <- "STAT_AIRPORT_INFO.csv" %>%
  here::here("data", .) %>%
  read_csv2()

apts <- read_csv(apt_url)
rwys <- read_csv(rwy_url)

aaa <- apts_pru %>%
  left_join(apts, by = c("APT_ICAO" = "ident")) %>%
  left_join(apdf_apts, by = c("APT_ICAO" = "AIRPORT"), suffix = c("", "y")) %>%
  select(APT_ICAO, APT_IATA, APT_NAME, APT_COUNTRY, ICAO_LABEL, IATA_LABEL,
         longitude_deg, latitude_deg, elevation_ft, BBOX)

rrr <- rwys %>%
  # exclude EGPD rwys apart from the "16" (and "34")
  filter(!(airport_ident == "EGPD" & !le_ident %in% c("16"))) %>%
  filter(!(airport_ident == "LKPR" & le_ident %in% c("4"))) %>%
  filter(!(airport_ident == "LPPT" & le_ident %in% c("17")))

# generate ALL
aaa %>%
  pull(APT_ICAO) %>%
  purrr::walk(.f = geom_airport)

