# copy over the Excel files 

library(fs)

# AIRPORT from AIU Portal
basedir <- "../pru-portal/static/download/xls"
filenames <- c(
  "Airport_Traffic.xlsx",
  "Airport_Arrival_ATFM_Delay.xlsx",
  "ATFM_Slot_Adherence.xlsx",
  "ASMA_Additional_Time.xlsx",
  "Taxi-Out_Additional_Time.xlsx"
)

# for Sharepoint mapped drive
# basedir <- "https://coll.eurocontrol.int/sites/pru/dashboard/Data"
# files_in <- paste(basedir, filenames, sep = "/")
# purrr::walk2(
#   files_in,
#   files_out,
#   ~ download.file(.x, .y, method = "wininet", quiet = TRUE))

files_in <- fs::path_abs(filenames, start = basedir)
files_out <- fs::path_abs(filenames, start = here::here("data"))

fs::file_copy(files_in, files_out, overwrite = TRUE)


# AIRPORT from COVID
basedir <- 'G:/HQ/dgof-pru/Data/DataProcessing/Covid19'
# file_in <- fs::path_abs("1_Top_100_Airport_dep+arr_traffic_LTFM+LTBA (Synthesis).xlsx", start = basedir)
file_in <- fs::path_abs("1_Top_100_Airport_dep+arr_traffic(Synthesis).xlsx", start = basedir)
file_out <- fs::path_abs("COVID-AIRPORT.xlsx", start = here::here("data"))

fs::file_copy(file_in, file_out, overwrite = TRUE)

# process COVID data
library(ROracle)
library(readxl)
library(dplyr)
library(readr)

export_query <- function(schema, query) {
  usr <- Sys.getenv(paste0(schema, "_USR"))
  pwd <- Sys.getenv(paste0(schema, "_PWD"))
  dbn <- Sys.getenv(paste0(schema, "_DBNAME"))
  
  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )
  
  con %>%
    dbSendQuery(query) %>%
    fetch(n = -1)
  
}
export_airport_covid <- function() {
  query <- "select code as arp_code,
        id as arp_id ,
        dashboard_name as arp_name,
       SUBSTR (CODE, 1, 2) AS ICAO2LETTER
 from prudev.pru_airport where code in (
   'EBLG',
'EBBR','EDDF','EDDL','EDDM','EFHK','EGCC','EGKK','EGLL','EGSS','EHAM','EIDW','EKCH','ENGM','EPWA','ESSA','LEBL','LEMD','LEPA','LFMN','LFPG','LFPO','LGAV',
'LIMC','LIRF','LLBG','LOWW','LPPT','LSGG','LSZH','EYVI','LBSF','LIME','LTBJ','EDDP'
,'EGLC','EGPD','LFBO','LGTS','LICC','LTAC','LTFJ','UKBB','EBCI',
'EDDK','EDDN','EGAA','EGPF','GCLP','LCLK','LGIR','LICJ','LKPR','LMML','LTAI','EDDH','EGGD','EPKK','ESSB','EVRA','GCXO','LFBD','LFLL','LROP','EGNX','EGPH',
'GMMX','LEVC','LEZL','LYBE','LPPR','EDDB','GMMN','LIRA','LPFR','EGBB','ELLX','LEAL','LHBP','LIPE','ENZV','LEMG','LIML','EGGW','ENVA','ENBR','EDDS','GCTS','LFSB',
'EDDV','ESGG','LFRS','LEIB','LFML','LEBB','LFPB','LIPZ','LIRN','GCRR','LTFM','LTBA','LATI', 'UDYZ', 'LDZA', 'EETN', 'LJLJ', 'LZIB', 'LUKK', 'LWSK','LYPG', 'LQSA','UGTB')
 --,'EDDT'-- Berlin Tegel remove dec 2020 
 "
  
  export_query("PRU_DEV", query)
}

export_airport_covid() %>%
  readr::write_csv(here::here("data", "COVID_AIRPORT_CODE.csv"))

ds <- readxl::read_excel(
  path  = here::here("data", "COVID-AIRPORT.xlsx"),
  sheet = "DATA",
  range = cell_cols("O:X"),
  skip = 10
)

ccc <- read_csv("data/COVID_AIRPORT_CODE.csv")

nms <- readxl::read_excel(
  path  = here::here("data", "COVID-AIRPORT.xlsx"),
  sheet = "MAP_STATE_AIRPORT",
  range = cell_cols("B:D")
  ) %>%
  # add header col to find and filter empty cells not captured correctly
  # du to merge cell (i.e. skip = x) stumbles over cell with formula
  mutate(
    HEADER = if_else(.[[1]] == "ARP_CODE", 1, 0),
    HEADER = tidyr::replace_na(HEADER, 0),
    HEADER = cumsum(HEADER)
  ) %>%
  filter(HEADER == 1) %>%
  select(-HEADER)

# now make first row the header row and remove it
names(nms) <- nms[1, ]
nms <- nms[-1, ]
nms <- nms %>% rename(ARP_NAME = ARP_CODE)
nms <- nms %>%
  left_join(ccc) %>%
  mutate(ARP_CODE = if_else(ARP_NAME == "Berlin-Tegel", "EDDT", ARP_CODE)) %>%
  rename(APT_ICAO = ARP_CODE)

ds <- ds %>%
  rename(
    "ARP_NAME" = "Entity", "WEEK" = "Week", "DAY" = "Day",
    "FLTS_2019" = "Flights 2019", "FLTS_2020" = "Flights 2020",
    "VAR_DLY" = "Daily Variation", "VAR_WKLY" = "Weekly Variation",
    "MOV_AVG_WK" = "Weekly Moving Average"
    # ,"DAY2019" = "...11"
  ) %>% 
  mutate(DAY = lubridate::as_date(DAY))

# merge ICAO with ds
ds <- ds %>%
  left_join(nms)

write_csv(ds, here::here("data", "COVID_AIRPORT.csv.gz"))
