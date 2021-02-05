library(dplyr)
library(readxl)
library(readr)
library(DBI)
library(ROracle)

# ..........................................................................----
# ORACLE DB EXPORT FUNCTION ----
# ..........................................................................----
EXPORT_QUERY <- function(schema, query) {
  USR <- Sys.getenv(paste0(schema, "_USR"))
  PWD <- Sys.getenv(paste0(schema, "_PWD"))
  DBN <- Sys.getenv(paste0(schema, "_DBNAME"))
  withr::local_envvar(c("TZ" = "UTC", "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(DBI::dbDriver("Oracle"),
                   USR, PWD, dbname = DBN,
                   timezone = "UTC"))
  con %>%
    dbSendQuery(query) %>%
    fetch(n = -1)
}


# ..........................................................................----
# 1 - EXTRACT NM DATA SETS FROM ORACLE DB ----
# ..........................................................................----

#***********************************************************************
# ---- TOP 100 AIRPORT INFOS (2019) ----
#***********************************************************************
EXPORT_AIRPORT_INFO <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_STAT_TOP100_APT_INFO"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}
#***************************************************
# ---- NM APT TRAFFIC ----
#***************************************************
EXPORT_NM_APT_TRAFFIC <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_STAT_NM_APT_TRAFFIC"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}
#***************************************************
# ---- NM APT THROUGHPUT ----
#***************************************************
EXPORT_NM_APT_THROUGHPUT <- function() {
  QUERY <- "
    SELECT
      *
    FROM
      PRU_AIRPORT.STAT_NM_APT_THROUGHPUT
    WHERE
      AIRPORT IN (SELECT DISTINCT AIRPORT FROM PRU_AIRPORT.V_STAT_TOP100_APT_INFO)"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

# .----
# > WRITING CSV FILES IN DATA FOLDER ----

EXPORT_AIRPORT_INFO() %>%
  readr::write_csv2(here::here("data", "PRU_AIRPORT_INFO.csv"))

EXPORT_NM_APT_TRAFFIC() %>%
  readr::write_csv2(here::here("data", "NM_APT_TRAFFIC.csv"))

EXPORT_NM_APT_THROUGHPUT() %>%
  readr::write_csv2(here::here("data", "NM_APT_THROUGHPUT.csv"))

# .----


# ..........................................................................----
# 2 - DOWNLOAD PIP EXCEL FILES FROM AIU PORTAL ----
# ..........................................................................----

#************************************
# ---- SETTING DATA SOURCE (URL) ----
#************************************
BASEDIR <- "https://coll.eurocontrol.int/sites/pru/dashboard/Data"
# .----

#***************************************************
# ---- DOWNLOAD Airport_Arrival_ATFM_Delay.xlsx ----
#***************************************************
FILENAME <- c("Airport_Arrival_ATFM_Delay.xlsx")
FILE_IN  <- paste(BASEDIR, FILENAME, sep = "/")
FILE_OUT <- fs::path_abs(paste0("PIP_", FILENAME), start = here::here("data"))
download.file(FILE_IN, FILE_OUT, mode = "wb")

#*********************************************
# ---- DOWNLOAD ATFM_Slot_Adherence.xlsx" ----
#*********************************************
FILENAME <- c("ATFM_Slot_Adherence.xlsx")
FILE_IN  <- paste(BASEDIR, FILENAME, sep = "/")
FILE_OUT <- fs::path_abs(paste0("PIP_", FILENAME), start = here::here("data"))
download.file(FILE_IN, FILE_OUT, mode = "wb")
# .----


# ..........................................................................----
# 3 - EXTRACT APDF DATA SETS FROM ORACLE DB ----
# ..........................................................................----

#***********************************************************************
# ---- APT RWY CONFIG (2019)  ----
#***********************************************************************
EXPORT_STAT_APDF_RWY_CONFIGURATION <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_CONFIGURATION"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

#***********************************************************************
# ---- ASMA / TAXI OUT / TAXI IN / PREDEP DLY  ----
#***********************************************************************
EXPORT_STAT_APDF_MONTLHY_DATA <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_STAT_APDF_MONTHLY_DATA"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

#***********************************************************************
# ---- TURNAROUND  ----
#***********************************************************************
EXPORT_STAT_APDF_TURNAROUND_DATA <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.STAT_AIRPORT_TURN_ARROUND"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

# .----
#***********************************************************************
# > WRITING CSV FILES IN DATA FOLDER ----
#***********************************************************************
EXPORT_STAT_APDF_RWY_CONFIGURATION() %>%
  readr::write_csv2(here::here("data", "APDF_RWY_CONFIGURATION.csv"))

EXPORT_STAT_APDF_MONTLHY_DATA() %>%
  readr::write_csv2(here::here("data", "APDF_MONTHLY_DATA.csv"))

EXPORT_STAT_APDF_TURNAROUND_DATA() %>%
  readr::write_csv2(here::here("data", "APDF_TURNAROUND_DATA.csv"))
# .----
