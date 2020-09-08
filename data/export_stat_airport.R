library(ROracle)
library(magrittr)
library(withr)
library(readr)


# export to CSV all STAT_AIRPORT data sets

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

export_stat_airport_info <- function() {
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
  
  export_query("PRU_AIRPORT", query)
}

export_stat_airport_monthly_data <- function() {
  query <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_MONTHLY_DATA"
  
  export_query("PRU_AIRPORT", query)
}

export_stat_airport_turn_around <- function() {
  query <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_TURN_ARROUND"
  
  export_query("PRU_AIRPORT", query)
}

export_stat_airport_throughput <- function() {
  query <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_THROUGHPUT"
  
  export_query("PRU_AIRPORT", query)
}

export_stat_airport_configuration <- function() {
  query <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_CONFIGURATION"
  
  export_query("PRU_AIRPORT", query)
}




export_stat_airport_info() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_INFO.csv"))

export_stat_airport_monthly_data() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_MONTHLY_DATA.csv"))

export_stat_airport_turn_around() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_TURN_AROUND.csv"))

export_stat_airport_configuration() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_CONFIGURATION.csv"))

export_stat_airport_throughput() %>% 
  readr::write_csv2(here::here("data","STAT_AIRPORT_THROUGHPUT.csv"))

