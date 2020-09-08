library(ROracle)
library(magrittr)
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
    A.AIRPORT,
    A.APT_ICAO,
    A.APT_IATA,
    A.APT_NAME,
    A.APT_COUNTRY,
    A.APT_NAME || ' (' || A.APT_ICAO || ')' AS ICAO_LABEL,
    A.APT_NAME || ' (' || A.APT_IATA || ')' AS IATA_LABEL
  FROM
    STAT_AIRPORT_INFO A
  WHERE A.APT_IN_PIP = 'Y'
  "
  
  con %>%
    dbSendQuery(query) %>%
    fetch(n = -1)
}

export_airport_info() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_INFO.csv"))

