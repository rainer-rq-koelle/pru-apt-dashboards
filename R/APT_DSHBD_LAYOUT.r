# ..........................................................................----
# --- SET UP ----
# ---

cat("\014")        # Clear Environment           ----
rm(list = ls())    # Clear Console               ----


library(readr)
library(dplyr)
library(osmdata) 
library(sf)
library(ggspatial)
library(ggplot2)
library(forcats)
library(stringr)

save_osm_apt <- function(   .gg,
                            .apt_icao,
                            .format = ".png",
                            .dir="./layouts/")
  { 
    fn <- paste(.apt_icao, .format, sep = "")
    
    ggsave(plot = .gg, filename = fn, path = .dir)
  }


osm_apt <- function(  .bb_lonlat,
                      .title = NULL,
                      .add_north = TRUE) 
  {  
    q <- opq(bbox = .bb_lonlat) %>%
          add_osm_feature(key = "aeroway",
                          value = c("aerodrome", "apron", "control_tower", "gate", "hangar","helipad", "runway", "taxiway", "terminal")) %>%
          osmdata_sf() %>%
          unname_osmdata_sf() %>%
          unique_osmdata()

    gg <- ggplot() +
      geom_sf(
        data = q$osm_polygons,
        inherit.aes = FALSE,
        color = "lightblue"
      ) +
      # ----multipolygon ----
    geom_sf(
      data = q$osm_multipolygons,
      #inherit.aes = FALSE,
      color = "lightblue") +
      ##
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
      # ----- multipolygon runway
      geom_sf(
        data = q$osm_polygons %>% filter(aeroway == "runway"),
        inherit.aes = FALSE,
        fill = "black",
        alpha = .8) +
      theme_void()


  # add north arrow with ggspatial
  if (!is.null(.add_north)) {
    gg <- gg +
      ggspatial::annotation_north_arrow(height = unit(0.7, "cm"), width = unit(0.7, "cm"))}

  # return aerodrome layout map
  return(gg)

  }



bb_coerce <- function(.bb_lnglat_string)
  {
  bb <- strsplit(.bb_lnglat_string, ",")
  bb <- unlist(bb)
  bb <- as.numeric(bb)
  return(bb)
  }

nth_group <- function(x, n) 
  {
    x %>%
    select(group_cols()) %>%
    distinct %>%
    ungroup %>%
    slice(n) %>%
    { semi_join(x, .)}
  }

apt_list <- read_csv2(here("data","APT_DSHBD_AIRPORT.csv"))
apt_list <- apt_list %>%
  select(
    ICAO     = ICAO_CODE,
    APT_NAME = APDF_NAME  )

geom_airport <- function(apt_list) 
  {
    apt_icao  <- apt_list %>% 
                  pull(.data$icao) %>%
                  unique()
  
    apt_name  <- apt_list %>% 
                  pull(.data$APT_NAME) %>% 
                  unique()

    bb_lonlat <- apt_list %>%
                  pull(.data$BBOX)%>% 
                  unique() %>%
                  bb_coerce()

    cat(apt_icao)
  
    gg_base <- osm_apt(bb_lonlat,
                       .add_north = TRUE)

    gg <- gg_base + 
            coord_sf(crs = "+proj=longlat +datum=WGS84") +
            geom_label(data = apt_list,
                       mapping = aes(x = le_longitude_deg, 
                                     y = le_latitude_deg,
                                     label = le_ident, 
                                     angle = le_heading_degT)) +
            geom_label(data = apt_list,
                       mapping = aes(x = he_longitude_deg, 
                                     y = he_latitude_deg,
                                     label = he_ident, 
                                     angle = he_heading_degT))
  
    save_osm_apt(gg, apt_icao, .dir = "layouts")  
}


apt_url <- "https://ourairports.com/data/airports.csv"
#apt_url <- here::here("data", "airports.csv.gz")

rwy_url <- "https://ourairports.com/data/runways.csv"
#rwy_url <- here::here("data", "runways.csv.gz")


apts_pru <- "APT_DSHBD_AIRPORT.csv" %>%
  here::here("data", .) %>%
  read_csv2() %>%
  mutate(APT_ICAO = ICAO_CODE)

apts <- read_csv(apt_url) %>%
  select(ident, latitude_deg, longitude_deg)


apdf_apts <- readxl::read_excel(
  here::here("data", "APT_BBOX.xlsx"),
  sheet = "Monitored")


rwys <- read_csv(rwy_url) %>%
  mutate(closed = as.logical(closed)) %>%
  # prefix 1-digit RWY id with a 0
  mutate(le_ident = if_else(str_length(le_ident) == 1 & str_detect(le_ident, "\\d"),
                            paste0("0", le_ident),
                            le_ident))

rrr <- rwys %>%
  # exclude closed runways
  filter(closed == FALSE) %>%
  # exclude errors regarding runways
  filter(!(airport_ident == "LYPG" & le_ident %in% c("08"))) %>%
  filter(!(airport_ident == "UGTB" & le_ident %in% c("13L"))) %>%
  # keep only certain attributes
  select(airport_ident,
         le_ident, le_longitude_deg, le_latitude_deg, le_ident, le_heading_degT,
         he_ident, he_longitude_deg, he_latitude_deg, he_ident, he_heading_degT)

aaa <- apts_pru %>%
  left_join(apts, by = c("APT_ICAO" = "ident")) %>%
  left_join(apdf_apts, by = c("APT_ICAO" = "AIRPORT"), suffix = c("", ".y")) %>%
  select(-ends_with(".y")) %>%
  left_join(rrr, by = c("APT_ICAO" = "airport_ident"))


# generate ALL

  problematic_ones <- c("EDDT")
  # EDDT issue
  # Error in UseMethod("filter_") : 
  # no applicable method for 'filter_' applied to an object of class "NULL"
  # No runway info in the map layout ? (=> aeroway == "runway")


aaa %>%
  group_by(APT_ICAO) %>%
  mutate(icao = APT_ICAO) %>%
  filter(!icao %in% problematic_ones) %>%
  #filter(icao == "EBBR") %>%  
  group_walk(~ geom_airport(.x))

