# ..........................................................................----
# --- SET UP ----
# ---

# .----

# ..............................----
# ---- SUMMARY FOR APDF APT     ----
# ..............................----


prepare_front_page_DT <- function(.icao,
                                  .tfc,
                                  .asma,
                                  .txot,
                                  .atfm,
                                  .ldgsum,
                                  .latest) {
  library(tidyverse)
  library(DT)
  library(formattable)
  library(sparkline)

  icao <- .icao
  tfc <- .tfc
  asma <- .asma
  txot <- .txot
  atfm <- .atfm
  ldgsum <- .ldgsum
  latest <- .latest

  # utility function to extract last 24 months for overview table
  extract_last_months <- function(df,
                                  n_months = 24,
                                  now_year = lubridate::year(Sys.Date()),
                                  now_month = lubridate::month(Sys.Date())) {
    df <- df %>%
      filter(MOF < paste0(now_year, "-", sprintf("%02d", now_month))) %>%
      arrange(desc(MOF)) %>%
      head(n_months)

    if (!is.integer(df$YEAR)) {
      df$YEAR <- as.integer(df$YEAR)
    }

    if (!is.integer(df$MONTH_NUM)) {
      df$MONTH_NUM <- as.integer(df$MONTH_NUM)
    }

    return(df)
  }

  tfc2 <- tfc %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, FLT_DEP, FLT_ARR, FLT_TOT) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  asma2 <- asma %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, ADD_ASMA = TOT_ADD_TIME, N_ASMA = TOT_FLT) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  txot2 <- txot %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, ADD_TXOT = TOT_ADD_TIME, N_TXOT = TOT_FLT) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  atfm2 <- atfm %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, ATFM_ADLY = DLY_APT_ARR_1, N_ATFM_A = FLT_ARR_1) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(ATFM_ADLY = sum(ATFM_ADLY, na.rm = TRUE), N_ATFM_A = sum(N_ATFM_A, na.rm = TRUE)) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  perf <- tfc2 %>%
    left_join(asma2, by = c("MOF", "YEAR", "MONTH_NUM")) %>%
    left_join(txot2, by = c("MOF", "YEAR", "MONTH_NUM")) %>%
    left_join(atfm2, by = c("MOF", "YEAR", "MONTH_NUM")) %>%
    ungroup()

  calc_current_month <- function(.perf) {
    df <- .perf %>%
      mutate(
        DIFF_FLT = FLT_TOT - lead(FLT_TOT),
        DIFF_FLT_P = DIFF_FLT / lead(FLT_TOT),
        AVG_ADD_ASMA = ADD_ASMA / N_ASMA,
        DIFF_ASMA = AVG_ADD_ASMA - lead(AVG_ADD_ASMA),
        DIFF_ASMA_P = DIFF_ASMA / lead(AVG_ADD_ASMA),
        AVG_ADD_TXOT = ADD_TXOT / N_TXOT,
        DIFF_TXOT = AVG_ADD_TXOT - lead(AVG_ADD_TXOT),
        DIFF_TXOT_P = DIFF_TXOT / lead(AVG_ADD_TXOT),
        AVG_ATFM_ADLY = ATFM_ADLY / N_ATFM_A,
        DIFF_ATFM_A = AVG_ATFM_ADLY - lead(AVG_ATFM_ADLY),
        DIFF_ATFM_A_P = DIFF_ATFM_A / lead(AVG_ATFM_ADLY)
      ) %>%
      select(
        YEAR, MONTH_NUM, MOF, FLT_TOT, DIFF_FLT, DIFF_FLT_P,
        AVG_ADD_ASMA, DIFF_ASMA, DIFF_ASMA_P, AVG_ADD_TXOT,
        DIFF_TXOT, DIFF_TXOT_P, AVG_ATFM_ADLY, DIFF_ATFM_A, DIFF_ATFM_A_P
      ) %>%
      mutate_if(is.double, round, 3)

    df$CHECK <- NA

    df$CHECK[1] <- "NOW"

    df$CHECK[13] <- "AGO"

    return(df)
  }

  perf2 <- perf %>% calc_current_month()

  tfc_spk <- sparkline::spk_chr(rev(perf2$FLT_TOT[1:13]),
    spotColor = NULL,
    minSpotColor = NULL,
    maxSpotColor = NULL
  )

  asma_spk <- sparkline::spk_chr(rev(perf2$AVG_ADD_ASMA[1:13]), type = "bar")

  txot_spk <- sparkline::spk_chr(rev(perf2$AVG_ADD_TXOT[1:13]), type = "bar")

  atfm_spk <- sparkline::spk_chr(rev(perf2$AVG_ATFM_ADLY[1:13]), type = "bar")

  spark_tbl <- tibble(
    IND = c("TFC", "ASMA", "TXOT", "ATFM_A"),
    SPK = c(tfc_spk, asma_spk, txot_spk, atfm_spk)
  )

  r_tfc <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "TFC") %>%
    select(IND, CURRENT = FLT_TOT, CHANGE_M2M_P = DIFF_FLT_P)

  r_asma <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "ASMA") %>%
    select(IND, CURRENT = AVG_ADD_ASMA, CHANGE_M2M_P = DIFF_ASMA_P)

  r_txot <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "TXOT") %>%
    select(IND, CURRENT = AVG_ADD_TXOT, CHANGE_M2M_P = DIFF_TXOT_P)

  r_atfm <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "ATFM_A") %>%
    select(IND, CURRENT = AVG_ATFM_ADLY, CHANGE_M2M_P = DIFF_ATFM_A_P)

  IND <- c("TFC", "ASMA", "TXOT", "ATFM_A")

  NAM <- c(
    "Total traffic (annual & recent month)",
    "Average additional ASMA time [min/arr]",
    "Average additional taxi-out time [min/dep]",
    "Average arrival ATFM delay [min/arr]"
  )

  nam <- tibble(IND = IND, NAM = NAM)

  out <- bind_rows(r_tfc, r_asma, r_txot, r_atfm)

  out <- out %>%
    left_join(spark_tbl, by = "IND")

  out <- out %>%
    left_join(nam, by = "IND")

  out <- out %>%
    select(NAM, CURRENT, CHANGE_M2M_P, SPK)


  out <- out %>% mutate(YEAR2019 = 0)

  out[1, "YEAR2019"] <- ldgsum$NB_NM_TOT

  out[2, "YEAR2019"] <- ldgsum$AVG_ADD_ASMA

  out[3, "YEAR2019"] <- ldgsum$AVG_ADD_TXOT

  out[4, "YEAR2019"] <- ldgsum$AVG_ARR_ATFM

  out$CHANGE_M2M_P <- NULL

  out <- out %>% mutate(CURRENT = "0")

  out[1, "CURRENT"] <- latest$TFC

  out[2, "CURRENT"] <- formatC(latest$AVG_ADD_ASMA, digits = 2, format = "f")

  out[3, "CURRENT"] <- formatC(latest$AVG_ADD_TXOT, digits = 2, format = "f")

  out[4, "CURRENT"] <- formatC(latest$AVG_ARR_ATFM, digits = 2, format = "f")

  out <- out %>% select(NAM, YEAR2019, CURRENT, SPK)

  return(out)
}


# ..............................----
# ---- SUMMARY FOR NON APDF APT ----
# ..............................----


prepare_front_page_NON_APDF <- function(.icao,
                                        .tfc,
                                        .atfm,
                                        .ldgsum,
                                        .latest) {
  library(tidyverse)
  library(DT)
  library(formattable)
  library(sparkline)

  icao <- .icao
  tfc <- .tfc
  atfm <- .atfm
  ldgsum <- .ldgsum
  latest <- .latest

  # utility function to extract last 24 months for overview table
  extract_last_months <- function(df,
                                  n_months = 24,
                                  now_year = lubridate::year(Sys.Date()),
                                  now_month = lubridate::month(Sys.Date())) {
    df <- df %>%
      filter(MOF < paste0(now_year, "-", sprintf("%02d", now_month))) %>%
      arrange(desc(MOF)) %>%
      head(n_months)

    if (!is.integer(df$YEAR)) {
      df$YEAR <- as.integer(df$YEAR)
    }

    if (!is.integer(df$MONTH_NUM)) {
      df$MONTH_NUM <- as.integer(df$MONTH_NUM)
    }

    return(df)
  }

  tfc2 <- tfc %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, FLT_DEP, FLT_ARR, FLT_TOT) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  atfm2 <- atfm %>%
    filter(AIRPORT == icao) %>%
    select(YEAR, MONTH_NUM, ATFM_ADLY = DLY_APT_ARR_1, N_ATFM_A = FLT_ARR_1) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(ATFM_ADLY = sum(ATFM_ADLY, na.rm = TRUE), N_ATFM_A = sum(N_ATFM_A, na.rm = TRUE)) %>%
    mutate(MOF = paste0(YEAR, "-", sprintf("%02d", MONTH_NUM))) %>%
    extract_last_months()

  perf <- tfc2 %>%
    left_join(atfm2, by = c("MOF", "YEAR", "MONTH_NUM")) %>%
    ungroup()

  calc_current_month <- function(.perf) {
    df <- .perf %>%
      mutate(
        DIFF_FLT = FLT_TOT - lead(FLT_TOT),
        DIFF_FLT_P = DIFF_FLT / lead(FLT_TOT),
        AVG_ATFM_ADLY = ATFM_ADLY / N_ATFM_A,
        DIFF_ATFM_A = AVG_ATFM_ADLY - lead(AVG_ATFM_ADLY),
        DIFF_ATFM_A_P = DIFF_ATFM_A / lead(AVG_ATFM_ADLY)
      ) %>%
      select(
        YEAR, MONTH_NUM, MOF, FLT_TOT, DIFF_FLT, DIFF_FLT_P,
        AVG_ATFM_ADLY, DIFF_ATFM_A, DIFF_ATFM_A_P
      ) %>%
      mutate_if(is.double, round, 3)

    df$CHECK <- NA

    df$CHECK[1] <- "NOW"

    df$CHECK[13] <- "AGO"

    return(df)
  }

  perf2 <- perf %>% calc_current_month()

  tfc_spk <- sparkline::spk_chr(rev(perf2$FLT_TOT[1:13]),
    spotColor = NULL,
    minSpotColor = NULL,
    maxSpotColor = NULL
  )

  atfm_spk <- sparkline::spk_chr(rev(perf2$AVG_ATFM_ADLY[1:13]), type = "bar")

  spark_tbl <- tibble(
    IND = c("TFC", "ATFM_A"),
    SPK = c(tfc_spk, atfm_spk)
  )

  r_tfc <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "TFC") %>%
    select(IND, CURRENT = FLT_TOT, CHANGE_M2M_P = DIFF_FLT_P)

  r_atfm <- perf2 %>%
    filter(CHECK == "NOW") %>%
    mutate(IND = "ATFM_A") %>%
    select(IND, CURRENT = AVG_ATFM_ADLY, CHANGE_M2M_P = DIFF_ATFM_A_P)

  IND <- c("TFC", "ATFM_A")

  NAM <- c(
    "Total traffic (annual & recent month)",
    "Average arrival ATFM delay [min/arr]"
  )

  nam <- tibble(IND = IND, NAM = NAM)

  out <- bind_rows(r_tfc, r_atfm)

  out <- out %>%
    left_join(spark_tbl, by = "IND")

  out <- out %>%
    left_join(nam, by = "IND")

  out <- out %>%
    select(NAM, CURRENT, CHANGE_M2M_P, SPK)


  out <- out %>% mutate(YEAR2019 = 0)

  out[1, "YEAR2019"] <- ldgsum$NB_NM_TOT

  out[2, "YEAR2019"] <- ldgsum$AVG_ARR_ATFM

  out$CHANGE_M2M_P <- NULL

  out <- out %>% mutate(CURRENT = "0")

  out[1, "CURRENT"] <- latest$TFC

  out[2, "CURRENT"] <- formatC(latest$AVG_ARR_ATFM, digits = 2, format = "f")

  out <- out %>% select(NAM, YEAR2019, CURRENT, SPK)

  return(out)
}