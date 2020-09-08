library(ROracle)
library(dplyr)
library(stringr)
library(withr)
library(readr)

export_airport_info <- function() {
  usr <- Sys.getenv("PRU_AIRPORT_USR")
  pwd <- Sys.getenv("PRU_AIRPORT_PWD")
  dbn <- Sys.getenv("PRU_AIRPORT_DBNAME")
  
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
  
  query <- "
    SELECT
      AIRPORT, APT_ICAO, APT_IATA, APT_NAME, APT_COUNTRY
    FROM
      STAT_AIRPORT_INFO
    WHERE
      APT_IN_PIP = 'Y'
  "
  
  con %>%
    dbSendQuery(query) %>%
    fetch(n = -1) %>% 
    dplyr::mutate(
      ICAO_LABEL = stringr::str_glue("{APT_NAME} ({APT_ICAO})"), 
      IATA_LABEL = stringr::str_glue("{APT_NAME} ({APT_IATA})")
    )
}

export_airport_info() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_INFO.csv"))

