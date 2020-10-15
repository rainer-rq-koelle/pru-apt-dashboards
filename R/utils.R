filter_df_by_apt <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt) %>%
    filter(YEAR >= min_year)  # ensure only 5 years of data
}
#
pick_apt_name <- function(.df, .apt){
  name <- .df %>% filter(APT_ICAO == .apt)
  name <- name$APT_NAME[1]
}
#
pick_state_name <- function(.df, .apt){
  state <- .df %>% filter(APT_ICAO == .apt)
  state <- state$STATE_NAME[1]
}
#
pick_apt_iata <- function(.df, .apt){
  iata <- .df %>% filter(APT_ICAO == .apt)
  iata <- iata$APT_IATA[1]
}
#
landing_page_indicators <- function(.df=db_df, .atfm=atfm_df, .apt){
  inds <- .df %>% filter(APT_ICAO == .apt)
  
  ind_tfc_2019 <- inds %>%
    select(APT_ICAO, YEAR, NB_NM_TOT) %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise(NB_NM_TOT = sum(NB_NM_TOT, na.rm = TRUE)) %>% ungroup()
  
  ind_txot_2019 <- inds %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise( ADD_TAXI_OUT_TIME_MIN = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE)
               ,NB_TAXI_OUT_FL        = sum(NB_TAXI_OUT_FL,        na.rm = TRUE)
    )%>% ungroup() %>%
    mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2) ) %>%
    select(AVG_ADD_TXOT)
  
  ind_asma_2019 <- inds %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise( ADD_ASMA_TIME_MIN = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE)
               ,NB_ASMA_FL       = sum(NB_ASMA_FL,        na.rm = TRUE)
    )%>% ungroup() %>%
    mutate(AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL,2) ) %>%
    select(AVG_ADD_ASMA)
  
  ind_atfm_2019 <- .atfm %>% filter(APT_ICAO == .apt, YEAR == 2019) %>%
    select(FLT_ARR_1, DLY_APT_ARR_1) %>%
    summarise( FLT_ARR_1     = sum(FLT_ARR_1,     na.rm = TRUE)
               ,DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
    mutate(AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1,2) ) %>%
    select(AVG_ARR_ATFM)
  
  out <- ind_tfc_2019 %>%
    bind_cols(ind_txot_2019, ind_asma_2019, ind_atfm_2019)
}
#
latest_month_indicators <- function(.df=db_df, .atfm=atfm_df, .apt){
  inds <- .df %>% filter(APT_ICAO == .apt)
  
  mth_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  ind_tfc_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, NB_NM_TOT) %>% na.omit() %>%
    filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate( MONTH = mth_name[MONTH_NUM]
            ,  TFC = paste0(NB_NM_TOT," (", MONTH, " ", YEAR,")")
    ) %>%
    select(APT_ICAO, TFC)
  
  ind_txot_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, ADD_TAXI_OUT_TIME_MIN, NB_TAXI_OUT_FL) %>%
    na.omit() %>% filter(YEAR == max(YEAR) ) %>%
    filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2)
           ,AVG_ADD_TXOT= paste0(AVG_ADD_TXOT," (", mth_name[MONTH_NUM], " ", YEAR,")")
    ) %>%
    select(AVG_ADD_TXOT)
  
  ind_asma_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, ADD_ASMA_TIME_MIN, NB_ASMA_FL) %>%
    na.omit() %>% filter(YEAR == max(YEAR) ) %>%
    filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate( AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL, 2)
            ,AVG_ADD_ASMA = paste0(AVG_ADD_ASMA," (", mth_name[MONTH_NUM], " ", YEAR,")")
    ) %>%
    select(AVG_ADD_ASMA)
  
  ind_atfm_lm <- .atfm %>% filter(APT_ICAO == .apt) %>%
    select(YEAR, MONTH_NUM, FLT_ARR_1, DLY_APT_ARR_1) %>%
    filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    na.omit() %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise( FLT_ARR_1     = sum(FLT_ARR_1, na.rm = TRUE)
               ,DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
    mutate( AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1, 2)
            ,AVG_ARR_ATFM = paste0(AVG_ARR_ATFM," (", mth_name[MONTH_NUM], " ", YEAR,")")) %>%
    select(AVG_ARR_ATFM)
  
  inds_lm <- ind_tfc_lm %>%
    bind_cols(ind_txot_lm, ind_asma_lm, ind_atfm_lm)
  
}
#
trim_covid <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt) %>%
    select(DAY, FLTS_2020, FLTS_2019, MOV_AVG_WK)
}
#
pack_thru <- function(.df, .apt){
  df <- .df %>% dplyr::filter(APT_ICAO == .apt) %>%
    dplyr::mutate(
      # DATE = lubridate::dmy(DAY, tz="UTC")
      DATE = DAY
      ,YEAR = year(DATE), MONTH_NUM = month(DATE)
      , WEEKDAY = lubridate::wday(DATE, label=TRUE)) %>%
    dplyr::filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    dplyr::select(APT_ICAO, YEAR, MONTH_NUM, DATE, WEEKDAY, TIME, ROLLING_HOUR_MVT, PHASE) %>%
    dplyr::group_by(YEAR, MONTH_NUM, TIME, PHASE) %>% summarise(ROLLING_HOUR_MVT = mean(ROLLING_HOUR_MVT)) %>%
    dplyr::ungroup()
}

prepare_params <- function(apt_icao) {
  list( #------ start params -------------------------
        icao  = apt_icao
        ,iata  = pick_apt_iata(   db_df ,   .apt = apt_icao)   # merge iata code with other source
        ,name  = pick_apt_name(   tfc_df,   .apt = apt_icao)
        ,state = pick_state_name( tfc_df,   .apt = apt_icao)
        ,config= filter_df_by_apt(config_df,.apt = apt_icao)
        ,ldgsum= landing_page_indicators(db_df, atfm_df, .apt = apt_icao)
        ,latest= latest_month_indicators(db_df, atfm_df, .apt = apt_icao)
        ,covid = trim_covid(      covid_df, .apt = apt_icao)
        ,tfc   = filter_df_by_apt(tfc_df,   .apt = apt_icao)
        ,thru  = pack_thru(       thru_df,  .apt = apt_icao)
        ,atfm  = filter_df_by_apt(atfm_df,  .apt = apt_icao)
        ,slot  = filter_df_by_apt(slot_df,  .apt = apt_icao)
        ,asma  = filter_df_by_apt(asma_df,  .apt = apt_icao)
        ,txot  = filter_df_by_apt(txot_df,  .apt = apt_icao)
        ,txit  = filter_df_by_apt(txit_df,  .apt = apt_icao)
        ,pddly = filter_df_by_apt(pddly_df, .apt = apt_icao)
        ,turn  = filter_df_by_apt(turn_df,  .apt = apt_icao)
        # ,punc  = filter_df_by_apt(punc_df,  .apt = apt_icao)
  ) #----------------- end params ---------------------------
  
}

