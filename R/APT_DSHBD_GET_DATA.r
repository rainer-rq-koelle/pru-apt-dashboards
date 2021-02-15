# ..........................................................................----
# --- SET UP ----
# ---

cat("\014")        # Clear Environment           ----
rm(list = ls())    # Clear Console               ----


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
# 1 - EXTRACT DATA FROM ORACLE DATABASE  ----
# ..........................................................................----


#***********************************************************************
# ---- APT LIST ----
#***********************************************************************
EXPORT_APT_DSHBD_AIRPORT <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_AIRPORT"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_AIRPORT() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_AIRPORT.csv"))


#***********************************************************************
# ---- APT RWY CONFIG ----
#***********************************************************************
EXPORT_APT_DSHBD_RWY_CONFIG <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_RWY_CONFIG"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_RWY_CONFIG() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_RWY_CONFIG.csv"))


#***************************************************
# ---- APT TRAFFIC ----
#***************************************************
EXPORT_APT_DSHBD_TRAFFIC <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TRAFFIC"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TRAFFIC() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TRAFFIC.csv"))


#***************************************************
# ---- APT TRAFFIC EVO ----
#***************************************************
EXPORT_APT_DSHBD_TRAFFIC_EVO <- function() {
  QUERY <- "SELECT * FROM PRUDEV.V_APT_DSHBD_TRAFFIC_EVO"
  EXPORT_QUERY("PRU_DEV", QUERY)
}

EXPORT_APT_DSHBD_TRAFFIC_EVO() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TRAFFIC_EVO.csv"))


#***************************************************
# ---- APT THROUGHPUT ----
#***************************************************
EXPORT_APT_DSHBD_THROUGHPUT <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_THROUGHPUT"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_THROUGHPUT() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_THROUGHPUT.csv"))


#***********************************************************************
# ---- APT APDF DATA (ASMA / TAXI OUT / TAXI IN / PREDEP DLY)  ----
#***********************************************************************
EXPORT_APT_DSHBD_APDF_DATA <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_APDF_DATA"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_APDF_DATA() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_APDF_DATA.csv"))


#***********************************************************************
# ---- APT TURNAROUND  ----
#***********************************************************************
EXPORT_APT_DSHBD_TURNAROUND <- function() {
  QUERY <- "SELECT * FROM PRU_AIRPORT.V_APT_DSHBD_TURNAROUND"
  EXPORT_QUERY("PRU_AIRPORT", QUERY)
}

EXPORT_APT_DSHBD_TURNAROUND() %>%
  readr::write_csv2(here::here("data", "APT_DSHBD_TURNAROUND.csv"))




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
FILE_OUT <- fs::path_abs("APT_DSHBD_ATFM.xlsx", start = here::here("data"))
download.file(FILE_IN, FILE_OUT, mode = "wb")

#*********************************************
# ---- DOWNLOAD ATFM_Slot_Adherence.xlsx" ----
#*********************************************
FILENAME <- c("ATFM_Slot_Adherence.xlsx")
FILE_IN  <- paste(BASEDIR, FILENAME, sep = "/")
FILE_OUT <- fs::path_abs("APT_DSHBD_SLOT_AD.xlsx", start = here::here("data"))
download.file(FILE_IN, FILE_OUT, mode = "wb")
# .----
