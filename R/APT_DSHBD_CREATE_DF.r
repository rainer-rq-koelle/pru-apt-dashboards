library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
library(here)
# ****************************----

# APT NAMES    ----

APT_DF <- read_csv2(here("data","APT_DSHBD_AIRPORT.csv"))
APT_DF <- APT_DF %>%
  select(
    AIRPORT  = AIRPORT,
    APT_NAME = APDF_NAME,
    STATE    = CTRY_ABBREVIATION,
    ICAO     = ICAO_CODE,
    IATA     = IATA_CODE,
    IS_APDF  = IS_APDF
  )


# RWY CONFIGURATION ----

CONFIG_DF <- read_csv2(here("data", "APT_DSHBD_RWY_CONFIG.csv"))
CONFIG_DF <- CONFIG_DF %>%
  drop_na() %>%
  select(AIRPORT, YEAR, CONFIGURATION, SHARE_PCT) %>%
  arrange(desc(SHARE_PCT))


# TRAFFIC ----

TFC_DF <- read_csv2(here("data", "APT_DSHBD_TRAFFIC.csv"))


# TRAFFIC VARIATION ----

TFC_VAR_DF <- read_csv2(here("data", "APT_DSHBD_TRAFFIC_EVO.csv"))

TFC_VAR_DF <- TFC_VAR_DF %>%
  mutate(
    DAY        = lubridate::as_date(DAY),
    FLTS       = as.numeric(FLTS),
    FLTS_2019  = as.numeric(FLTS_2019),
    MOV_AVG_WK = as.numeric(MOV_AVG_WK)
  ) %>%
  select(APT_ICAO, ARP_NAME, DAY, FLTS, FLTS_2019, MOV_AVG_WK)


# THROUGHPUT ----

THRU_DF <- read_csv2(here("data", "APT_DSHBD_THROUGHPUT.csv")) %>%
  mutate(APT_ICAO = AIRPORT)


# ****************************----
# APDF MONTLHY DATA ----
# ****************************----
APDF_MM_DF <- read_csv2(here("data","APT_DSHBD_APDF_DATA.csv"))


# ..ASMA YEARLY DATA ----
ASMA_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_ASMA_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )


# ..ASMA MONTHLY DATA ----
ASMA_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_ASMA_FL,
    ASMA_REF_TIME_MIN,
    ADD_ASMA_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(ASMA_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_ASMA_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-OUT YEARLY DATA ----
TXOT_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_OUT_FL,
    TAXI_OUT_REF_TIME_MIN,
    ADD_TAXI_OUT_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_OUT_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-OUT MONTHLY DATA  ----
TXOT_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_OUT_FL,
    TAXI_OUT_REF_TIME_MIN,
    ADD_TAXI_OUT_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_OUT_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_OUT_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )
# ..TAXI-IN YEARLY DATA ----
TXIN_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_IN_FL,
    TAXI_IN_REF_TIME_MIN,
    ADD_TAXI_IN_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_IN_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_IN_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..TAXI-IN MONTHLY DATA  ----
TXIN_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_TAXI_IN_FL,
    TAXI_IN_REF_TIME_MIN,
    ADD_TAXI_IN_TIME_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_UNIMP_TIME = sum(TAXI_IN_REF_TIME_MIN, na.rm = TRUE),
    TOT_ADD_TIME = sum(ADD_TAXI_IN_TIME_MIN, na.rm = TRUE),
    TOT_FLT = sum(NB_TAXI_IN_FL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_UNIMP_TIME = TOT_UNIMP_TIME / TOT_FLT,
    AVG_ADD_TIME = TOT_ADD_TIME / TOT_FLT
  )

# ..PDDLY YEARLY DATA ----
PDDLY_YY_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_DLY_DEP_FL,
    DLY_89_MIN,
    DLY_999_MIN,
    DLY_ZZZ_MIN,
    DLY_OTHER_MIN,
    UN_RPTED_DLY_MIN,
    OV_RPTED_DLY_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR) %>%
  summarise(
    TOT_FLT_DEP = sum(NB_DLY_DEP_FL, na.rm = TRUE),
    TOT_DLY_89 = sum(DLY_89_MIN, na.rm = TRUE),
    TOT_DLY_999 = sum(DLY_999_MIN, na.rm = TRUE),
    TOT_DLY_ZZZ = sum(DLY_ZZZ_MIN, na.rm = TRUE),
    TOT_DLY_OTHER = sum(DLY_OTHER_MIN, na.rm = TRUE),
    TOT_DLY_UNREPORTED = sum(UN_RPTED_DLY_MIN, na.rm = TRUE),
    TOT_DLY_OVREPORTED = sum(OV_RPTED_DLY_MIN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_DLY_UNID = TOT_DLY_999 + TOT_DLY_ZZZ + TOT_DLY_UNREPORTED,
    AVG_PREDEP_DLY = TOT_DLY_89 / TOT_FLT_DEP
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, TOT_DLY_89, TOT_DLY_OTHER, TOT_DLY_UNID, AVG_PREDEP_DLY)

# ..PDDLY MONTHLY DATA  ----
PDDLY_MM_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_DLY_DEP_FL,
    DLY_89_MIN,
    DLY_999_MIN,
    DLY_ZZZ_MIN,
    DLY_OTHER_MIN,
    UN_RPTED_DLY_MIN,
    OV_RPTED_DLY_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_FLT_DEP = sum(NB_DLY_DEP_FL, na.rm = TRUE),
    TOT_DLY_89 = sum(DLY_89_MIN, na.rm = TRUE),
    TOT_DLY_999 = sum(DLY_999_MIN, na.rm = TRUE),
    TOT_DLY_ZZZ = sum(DLY_ZZZ_MIN, na.rm = TRUE),
    TOT_DLY_OTHER = sum(DLY_OTHER_MIN, na.rm = TRUE),
    TOT_DLY_UNREPORTED = sum(UN_RPTED_DLY_MIN, na.rm = TRUE),
    TOT_DLY_OVREPORTED = sum(OV_RPTED_DLY_MIN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_DLY_UNID = TOT_DLY_999 + TOT_DLY_ZZZ + TOT_DLY_UNREPORTED,
    AVG_PREDEP_DLY = TOT_DLY_89 / TOT_FLT_DEP
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, TOT_DLY_89, TOT_DLY_OTHER, TOT_DLY_UNID, AVG_PREDEP_DLY)

# ..PDDLY MONTLHY AVERAGE  ----
PDDLY_AVG_DF <- APDF_MM_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    # ..........
    NB_DLY_DEP_FL,
    DLY_89_MIN,
    DLY_999_MIN,
    DLY_ZZZ_MIN,
    DLY_OTHER_MIN,
    UN_RPTED_DLY_MIN,
    OV_RPTED_DLY_MIN
    # ..........
  ) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM) %>%
  summarise(
    TOT_FLT_DEP = sum(NB_DLY_DEP_FL, na.rm = TRUE),
    TOT_DLY_89 = sum(DLY_89_MIN, na.rm = TRUE),
    TOT_DLY_999 = sum(DLY_999_MIN, na.rm = TRUE),
    TOT_DLY_ZZZ = sum(DLY_ZZZ_MIN, na.rm = TRUE),
    TOT_DLY_OTHER = sum(DLY_OTHER_MIN, na.rm = TRUE),
    TOT_DLY_UNREPORTED = sum(UN_RPTED_DLY_MIN, na.rm = TRUE),
    TOT_DLY_OVREPORTED = sum(OV_RPTED_DLY_MIN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_DLY_UNID = TOT_DLY_999 + TOT_DLY_ZZZ + TOT_DLY_UNREPORTED,
    AVG_PREDEP_DLY = TOT_DLY_89 / TOT_FLT_DEP
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, AVG_PREDEP_DLY)



# ****************************----
# APDF TURNAROUND DATA ----
# ****************************----

APDF_TURN_DF <- read_csv2(here("data","APT_DSHBD_TURNAROUND.csv"))


# ..TURN YEARLY DATA ----
TURN_YY_DF <- APDF_TURN_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    AC_CLASS,
    # ..........
    NB_TURN_ARROUND,
    TOT_SDTT_MIN,
    TOT_ACTT_MIN,
    TOT_ADTT_MIN
    # ..........
  ) %>%
  filter(AC_CLASS %in% c("H", "MJ", "MT")) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, AC_CLASS) %>%
  summarise(
    TOT_SDTT_MIN = sum(TOT_SDTT_MIN, na.rm = TRUE),
    TOT_ACTT_MIN = sum(TOT_ACTT_MIN, na.rm = TRUE),
    TOT_ADTT_MIN = sum(TOT_ADTT_MIN, na.rm = TRUE),
    TOT_TURN = sum(NB_TURN_ARROUND, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_SDTT = TOT_SDTT_MIN / TOT_TURN,
    AVG_ACTT = TOT_ACTT_MIN / TOT_TURN,
    AVG_ADTT = TOT_ADTT_MIN / TOT_TURN
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, AC_CLASS, TOT_TURN, AVG_SDTT, AVG_ACTT, AVG_ADTT)

# ..TURN MONTHLY DATA ----
TURN_MM_DF <- APDF_TURN_DF %>%
  select( # ..........
    AIRPORT,
    APT_ICAO,
    YEAR,
    MONTH_NUM,
    AC_CLASS,
    # ..........
    NB_TURN_ARROUND,
    TOT_SDTT_MIN,
    TOT_ACTT_MIN,
    TOT_ADTT_MIN
    # ..........
  ) %>%
  filter(AC_CLASS %in% c("H", "MJ", "MT")) %>%
  group_by(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, AC_CLASS) %>%
  summarise(
    TOT_SDTT_MIN = sum(TOT_SDTT_MIN, na.rm = TRUE),
    TOT_ACTT_MIN = sum(TOT_ACTT_MIN, na.rm = TRUE),
    TOT_ADTT_MIN = sum(TOT_ADTT_MIN, na.rm = TRUE),
    TOT_TURN = sum(NB_TURN_ARROUND, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    AVG_SDTT = TOT_SDTT_MIN / TOT_TURN,
    AVG_ACTT = TOT_ACTT_MIN / TOT_TURN,
    AVG_ADTT = TOT_ADTT_MIN / TOT_TURN
  ) %>%
  select(AIRPORT, APT_ICAO, YEAR, MONTH_NUM, AC_CLASS, TOT_TURN, AVG_SDTT, AVG_ACTT, AVG_ADTT)


# ****************************----
# PIP FILES  ----
# ****************************----

# ..PIP ATFM DELAY ----

ATFM_DF <- readxl::read_excel(
  here("data","APT_DSHBD_ATFM.xlsx"),sheet = "DATA") %>%
  mutate(AIRPORT = APT_ICAO,
         FLT_DATE = lubridate::date(FLT_DATE),
         across(starts_with("DLY_APT_ARR_"),
                ~ tidyr::replace_na(.x, 0)),
         AD_DISRUPTION = DLY_APT_ARR_A_1 +
           DLY_APT_ARR_E_1 +
           DLY_APT_ARR_N_1 +
           DLY_APT_ARR_O_1 +
           DLY_APT_ARR_NA_1,
         #
         AD_CAPACITY = DLY_APT_ARR_G_1 +
           DLY_APT_ARR_M_1 +
           DLY_APT_ARR_R_1 +
           DLY_APT_ARR_V_1,
         #
         AD_WEATHER = DLY_APT_ARR_D_1 +
           DLY_APT_ARR_W_1,
         #
         AD_DISRUPTION_ATC = DLY_APT_ARR_I_1 +
           DLY_APT_ARR_T_1,
         #
         AD_CAPACITY_ATC = DLY_APT_ARR_C_1,
         #
         AD_STAFFING_ATC = DLY_APT_ARR_S_1,
         #
         AD_EVENTS = DLY_APT_ARR_P_1
  ) %>%
  select(
    AIRPORT,
    YEAR,
    MONTH_NUM,
    FLT_DATE,
    FLT_ARR_1,
    DLY_APT_ARR_1,
    starts_with("AD_"),
    FLT_ARR_1_DLY,
    FLT_ARR_1_DLY_15
  )


# ..PIP SLOT ADHERENCE ----

SLOT_DF <- read_xlsx(
  here("data","APT_DSHBD_SLOT_AD.xlsx"),
  sheet = "DATA")

