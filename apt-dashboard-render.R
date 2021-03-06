# AIRPORT FLEXDASHBOARD RENDER SCRIPT
# This scrips reads in the data files (i.e. monthly files currently stored on
# ansperformance.eu) and additional data summaries. 
# These input files are currently saved to the project folder sub-folder data.
# The render script reads all "input data" trims it for the ICAO location 
# and supplies it to the parameterized dashboard Rmd.
#
# IMPORTANT: install htmlwidgets >= 1.5.2 from CRAN in order to avoid CSS units issues
# DONE:
# 1. processing pipeline read/extract data from PRISME to remove manual download.
#    SEE: data/export_stat_airport.R
# 1a.copy data from blog portal repo:
#    https://github.com/euctrl-pru/portal/tree/master/static/download/xls
#    SEE: data/copy_excel_files.R ; it assumes they are in the repo pru-portal
#         located at ../pru-portal
#
# To-Do
# 2. reduce data load by cleaning the input data tables
#    note: as the dashboard is under development this will be a clean-up task
#    once it is known what the "minimal needed payload data" is
# 3. post-processing: rendered boards are stored as html-pages in the docs
#    sub-folder
#
# load required packages
library("dplyr")
library("lubridate")
library("purrr")
library("readxl")
library(formattable)
library(sparkline)
library(flexdashboard)
library(plotly)

min_year <- 2016 # by definition 5 years, i.e. 2016-2020

# PICK AIRPORTS -------------------------------------------------
# TO LIMIT TEST LOAD SUBSET FOR "TEST" AIRPORTS
# apts <- c("EGLL","EBBR", "LEMD")
# TO-DO TROUBLE SHOOTING ----------------------------------------
# THE FOLLOWING AIRPORT DBs DO NOT RENDER - CHECK WHAT THROWS ERRORS
nope <- c("EGNT","ENBR","ENVA","ENZV","GCFV","LCLK","LFBO","LFML","LIMF","WSSS")
# nope <- c("XXXX")
apts <- list.files("./data-ad-rwy-charts/", pattern = "[A-Z]{4}\\.png") %>% strtrim(4)
# apts <- list.files("./data-ad-charts/", pattern = "[A-Z]{4}\\.png") %>% strtrim(4)
apts <- setdiff(apts, nope)

## ------------ READ IN DATA TABLES FROM DOWNLOAD POINT ------------------
#
# Thierry's dashboard table
db_df <- readr::read_csv2("./data/STAT_AIRPORT_MONTHLY_DATA.csv.gz")

db_conf <- readr::read_csv2("./data/STAT_AIRPORT_CONFIGURATION.csv.gz")

# traffic counts and ids
tfc_df <- readxl::read_excel("./data/Airport_Traffic.xlsx", sheet = "DATA"
                             # readxl style to force all chr # , col_types = "text" #
) %>%
  mutate(FLT_DATE = lubridate::date(FLT_DATE))

ids_df <- tfc_df %>% select(apt = APT_ICAO, name = APT_NAME, state = STATE_NAME)

config_df <- db_conf %>%
  na.omit() %>%    # currently only 2019 data, rest NA
  select(APT_ICAO, YEAR, CONFIGURATION, SHARE_PCT) %>%
  arrange(desc(SHARE_PCT))

thru_df <- readr::read_csv2("./data/STAT_AIRPORT_THROUGHPUT.csv.gz") %>% rename(APT_ICAO = AIRPORT)

atfm_df <- readxl::read_excel("./data/Airport_Arrival_ATFM_Delay.xlsx", sheet = "DATA") %>%
  mutate(FLT_DATE = lubridate::date(FLT_DATE)) %>%
  mutate_at(vars(starts_with("DLY_APT_ARR_")), tidyr::replace_na, 0) %>%
  mutate( AD_DISRUPTION = DLY_APT_ARR_A_1 + DLY_APT_ARR_E_1 + DLY_APT_ARR_N_1 +
            DLY_APT_ARR_O_1 + DLY_APT_ARR_NA_1
          ,AD_CAPACITY   = DLY_APT_ARR_G_1 + DLY_APT_ARR_M_1 + DLY_APT_ARR_R_1 +
            DLY_APT_ARR_V_1
          ,AD_WEATHER    = DLY_APT_ARR_D_1 + DLY_APT_ARR_W_1
          ,AD_DISRUPTION_ATC = DLY_APT_ARR_I_1 + DLY_APT_ARR_T_1
          ,AD_CAPACITY_ATC   = DLY_APT_ARR_C_1
          ,AD_STAFFING_ATC   = DLY_APT_ARR_S_1
          ,AD_EVENTS     = DLY_APT_ARR_P_1
  ) %>%
  # trim payload
  select( APT_ICAO, YEAR, MONTH_NUM, FLT_DATE, FLT_ARR_1, DLY_APT_ARR_1
          ,starts_with("AD_"), FLT_ARR_1_DLY, FLT_ARR_1_DLY_15)

# idea was to read out the meta data to map causes to groups
#atfm_reg <- readxl::read_excel("./data-test/Airport_Arrival_ATFM_Delay.xlsx", sheet = "META"
#                               , skip = 8  # skip first rows
#                               ) %>%
#  select(1, 4) %>%
#  filter(!is.na(`Reason Group`))
#
slot_df <- readxl::read_excel("./data/ATFM_Slot_Adherence.xlsx", sheet = "DATA")

asma_df <- readxl::read_excel("./data/ASMA_Additional_Time.xlsx", sheet = "DATA")

txot_df <- readxl::read_excel("./data/Taxi-Out_Additional_Time.xlsx", sheet = "DATA")

#txit_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TXIT.csv") %>% rename(APT_ICAO = APT)
txit_df <- db_df %>%
  select( APT_ICAO, YEAR, MONTH_NUM
          ,N_SMPL  = NB_TAXI_IN_FL
          ,TOT_REF = TAXI_IN_REF_TIME_MIN
          ,TOT_ADD_TXIT = ADD_TAXI_IN_TIME_MIN)

#pddly_df<- readxl::read_excel("./data-test/STAT_AIRPORT_DATA.xlsx", sheet = "DATA") %>%
#  select(APT_ICAO, APT_IATA, YEAR, MONTH_NUM, APT_FLT_DEP
#         ,TOTAL_DLY_89, TOTAL_DLY_999, TOTAL_DLY_ZZZ
#         ,TOTAL_DLY_OTHER, TOTAL_UN_RPTED_DLY, TOTAL_OV_RPTED_DLY)
pddly_df <- db_df %>%
  select(APT_ICAO, YEAR, MONTH_NUM, APT_FLT_DEP = NB_DLY_DEP_FL
         ,TOTAL_DLY_89 = DLY_89_MIN, TOTAL_DLY_999 = DLY_999_MIN
         ,TOTAL_DLY_ZZZ= DLY_ZZZ_MIN
         ,TOTAL_DLY_OTHER = DLY_OTHER_MIN
         ,TOTAL_UN_RPTED_DLY = UN_RPTED_DLY_MIN
         ,TOTAL_OV_RPTED_DLY = OV_RPTED_DLY_MIN
  )

#turn_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TURN.csv") %>% rename(APT_ICAO = APT)
turn_df <- readr::read_csv2("./data/STAT_AIRPORT_TURN_AROUND.csv.gz") %>%
  select(APT_ICAO, YEAR, MONTH_NUM, AC_CLASS
         ,N = NB_TURN_ARROUND
         ,TOT_SDTT_MIN, TOT_ACTT_MIN, TOT_ADTT_MIN) %>%
  mutate(AVG_ATTT = TOT_ACTT_MIN / N, AVG_STTT = TOT_SDTT_MIN / N) %>%
  select(APT_ICAO, YEAR, MONTH = MONTH_NUM, AC_CLASS, N, AVG_ATTT, AVG_STTT)

# punc_df <-  readr::read_csv("./data-test/STAT_AIRPORT_DATA_PUNC.csv") %>% rename(APT_ICAO = APT)

covid_df <- readr::read_csv("./data/COVID_AIRPORT.csv.gz")

## ------------ UTILITY FUNCTIONS -------------------------------------
#
source(here::here("R", "utils.R"), encoding = "UTF8")

#
## ------------ RENDER DASHBOARDS -------------------------------------
## TEST ##
# apt <- "EBBR"
# cucu <- prepare_params(apt)
# rmarkdown::render(
#   input  = "apt-dashboard.Rmd",   # master flexdashboard Rmd
#   params = cucu,
#   output_file = here::here("docs", paste0(apt, ".html"))
# )


# apt_range <- 2:2
apt_range <- c(1, 5, 7)
apt_range <- 1:length(apts)
apts %>%
  magrittr::extract(apt_range) %>%
  # under DEBUG: filter
  # c("EHAM", "LEMD", "LSZH") %>% 
  purrr::walk(
    .f = ~rmarkdown::render(
      input = "apt-dashboard.Rmd"   # master flexdashboard Rmd
      , params = prepare_params(.)
      # output_dir DEACTIVATED and included in output_file name
      # brittle as reported in stackoverflow #  , output_dir = "./boards"
      #, output_file = paste0("./boards/", ., ".html")
      , output_file = paste0("./docs/", ., ".html")
    )
  )


