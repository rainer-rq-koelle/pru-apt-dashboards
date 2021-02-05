# ..........................................................................----
# --- SET UP ----
# ---

#.----

MIN_YEAR <- lubridate::year( Sys.Date())-5


filter_df_by_apt <- function(.df, .apt)
                        {
                        DF <- .df %>% 
                                filter(AIRPORT == .apt) %>%
                                filter(YEAR >= MIN_YEAR)
                        }

#
pick_apt_name    <- function(.df, .apt)
{
        NAME <- .df %>% 
                filter(AIRPORT == .apt)
        
        NAME <- NAME$APT_NAME[1]
}
#
pick_state_name  <- function(.df, .apt)
{
        STATE <- .df %>% 
                filter(AIRPORT == .apt)
        
        STATE <- STATE$STATE[1]
}
#
pick_apt_iata   <-  function(.df, .apt)
{
        IATA <- .df %>% 
                filter(AIRPORT == .apt)
        IATA <- IATA$IATA[1]
}
#
pick_apt_apdf   <-  function(.df, .apt)
{
        APDF <- .df %>% 
                filter(AIRPORT == .apt)
        APDF <- APDF$IS_APDF[1]
}




#
landing_page_indicators <- function(.tfc = TFC_DF, .df = APDF_MM_DF, .atfm = ATFM_DF, .apt)
{
        inda         <- .tfc %>% 
                filter(APT_ICAO == .apt)
        
        ind_tfc_2019 <- inda %>%
                select(APT_ICAO, YEAR, FLT_TOT) %>%
                filter(YEAR == 2019) %>%
                group_by(APT_ICAO, YEAR) %>%
                summarise(NB_NM_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
                ungroup()
        
        indb         <- .df %>% 
                filter(APT_ICAO == .apt)
                
        ind_txot_2019 <- indb %>%
                filter(YEAR == 2019) %>%
                group_by(APT_ICAO, YEAR) %>%
                summarise( ADD_TAXI_OUT_TIME_MIN = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE),
                           NB_TAXI_OUT_FL        = sum(NB_TAXI_OUT_FL,        na.rm = TRUE))%>%
                ungroup() %>%
                mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2) ) %>%
                select(AVG_ADD_TXOT)
        
        ind_asma_2019 <- indb %>%
                filter(YEAR == 2019) %>%
                group_by(APT_ICAO, YEAR) %>%
                summarise( ADD_ASMA_TIME_MIN = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE),
                           NB_ASMA_FL        = sum(NB_ASMA_FL,        na.rm = TRUE))%>%
                ungroup() %>%
                mutate(AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL,2) ) %>%
                select(AVG_ADD_ASMA)
        
        ind_atfm_2019 <- .atfm%>% 
                filter(AIRPORT == .apt, YEAR == 2019) %>%
                select(FLT_ARR_1, DLY_APT_ARR_1) %>%
                summarise( FLT_ARR_1     = sum(FLT_ARR_1,     na.rm = TRUE),
                           DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
                mutate(AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1,2) ) %>%
                select(AVG_ARR_ATFM)

        if (pick_apt_apdf(APT_DF,.apt)=='Y')
        {        
        out           <- ind_tfc_2019 %>%
                         bind_cols(ind_txot_2019, 
                                   ind_asma_2019, 
                                   ind_atfm_2019)
        } else
        {
        out           <- ind_tfc_2019 %>%
                         bind_cols(ind_atfm_2019)
        }

}


#
latest_month_indicators <- function(.tfc = TFC_DF, .df = APDF_MM_DF, .atfm = ATFM_DF, .apt)
{

        mth_name      <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        
        indt          <- .tfc %>%
                filter(APT_ICAO == .apt)
                
        ind_tfc_lm    <- indt %>%
                select(APT_ICAO, YEAR, MONTH_NUM, FLT_TOT) %>%
                na.omit() %>%
                group_by(APT_ICAO, YEAR, MONTH_NUM) %>%
                summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>% 
                ungroup %>% 
                filter(YEAR == max(YEAR)) %>% 
                filter(MONTH_NUM == max(MONTH_NUM)) %>%
                mutate( MONTH = mth_name[MONTH_NUM],  
                        TFC   = paste0(FLT_TOT," (", MONTH, " ", YEAR,")")) %>%
                select(APT_ICAO, TFC)

        inds          <- .df %>%
                filter(APT_ICAO == .apt)
        
        
        ind_txot_lm   <- inds %>%
                select(APT_ICAO, YEAR, MONTH_NUM, ADD_TAXI_OUT_TIME_MIN, NB_TAXI_OUT_FL) %>%
                na.omit() %>% 
                filter(YEAR == max(YEAR) ) %>%
                filter(MONTH_NUM == max(MONTH_NUM)) %>%
                mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2),
                       AVG_ADD_TXOT = paste0(AVG_ADD_TXOT," (", mth_name[MONTH_NUM], " ", YEAR,")")) %>%
                select(AVG_ADD_TXOT)
        
        if (length(table(ind_txot_lm)) == 0) {ind_txot_lm = 'NA'}
        
        ind_asma_lm   <- inds %>%
                select(APT_ICAO, YEAR, MONTH_NUM, ADD_ASMA_TIME_MIN, NB_ASMA_FL) %>%
                na.omit() %>%
                filter(YEAR == max(YEAR) ) %>%
                filter(MONTH_NUM == max(MONTH_NUM)) %>%
                mutate( AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL, 2),
                        AVG_ADD_ASMA = paste0(AVG_ADD_ASMA," (", mth_name[MONTH_NUM], " ", YEAR,")")) %>%
                select(AVG_ADD_ASMA)

        if (length(table(ind_asma_lm)) == 0) {ind_asma_lm = 'NA'}
                
        ind_atfm_lm   <- .atfm %>% 
                filter(AIRPORT == .apt) %>%
                select(YEAR, MONTH_NUM, FLT_ARR_1, DLY_APT_ARR_1) %>%
                filter(YEAR == max(YEAR)) %>% 
                filter(MONTH_NUM == max(MONTH_NUM)) %>%
                na.omit() %>%
                group_by(YEAR, MONTH_NUM) %>%
                summarise( FLT_ARR_1     = sum(FLT_ARR_1, na.rm = TRUE),
                           DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
                mutate( AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1, 2),
                        AVG_ARR_ATFM = paste0(AVG_ARR_ATFM," (", mth_name[MONTH_NUM], " ", YEAR,")")) %>%
                select(AVG_ARR_ATFM)
        
        if (length(table(ind_atfm_lm)) == 0) {ind_atfm_lm = 'NA'}

        
        if (pick_apt_apdf(APT_DF,.apt)=='Y')
        {        
                inds_lm       <- ind_tfc_lm %>%
                        bind_cols(ind_txot_lm, ind_asma_lm, ind_atfm_lm)
        } else
        {
                inds_lm       <- ind_tfc_lm %>%
                        bind_cols(ind_atfm_lm)
        }
                
}

#
trim_tfcvar  <- function(.df, .apt)
{
        df   <- .df %>% 
                filter(APT_ICAO == .apt) %>%
                select(DAY, FLTS, FLTS_2019, MOV_AVG_WK)
}

#
pack_thru     <- function(.df, .apt)
{
        df   <- .df %>%
                
                dplyr::filter(APT_ICAO == .apt) %>%
                
                dplyr::mutate(DATE      = DAY,
                              YEAR      = year(DAY), 
                              MONTH_NUM = month(DAY), 
                              WEEKDAY   = lubridate::wday(DAY, label=TRUE)) %>%
                
                dplyr::filter(YEAR == max(YEAR)) %>% 
                
                filter(MONTH_NUM == max(MONTH_NUM)) %>%
                
                dplyr::select(APT_ICAO, YEAR, MONTH_NUM, DATE, WEEKDAY, TIME, ROLLING_HOUR_MVT, PHASE) %>%
                
                dplyr::group_by(YEAR, MONTH_NUM, TIME, PHASE) %>% 
                
                summarise(ROLLING_HOUR_MVT = mean(ROLLING_HOUR_MVT)) %>%
                
                dplyr::ungroup()
}

prepare_params <- function(apt_icao)
{
        list( 
                icao      = apt_icao,
                iata      = pick_apt_iata(   APT_DF,      .apt = apt_icao),
                name      = pick_apt_name(   APT_DF,      .apt = apt_icao),
                state     = pick_state_name( APT_DF,      .apt = apt_icao),
                apdf      = pick_apt_apdf(   APT_DF,      .apt = apt_icao),
                #
                config    = filter_df_by_apt(CONFIG_DF,   .apt = apt_icao),
                #
                ldgsum    = landing_page_indicators(TFC_DF, APDF_MM_DF, ATFM_DF, .apt = apt_icao),
                latest    = latest_month_indicators(TFC_DF, APDF_MM_DF, ATFM_DF, .apt = apt_icao),
                #
                tfcvar    = trim_tfcvar(      TFC_VAR_DF, .apt = apt_icao),                
                #
                tfc       = filter_df_by_apt(TFC_DF,      .apt = apt_icao),
                thru      = pack_thru(       THRU_DF,     .apt = apt_icao),
                atfm      = filter_df_by_apt(ATFM_DF,     .apt = apt_icao),
                #
                asma_yy   = filter_df_by_apt(ASMA_YY_DF,  .apt = apt_icao),
                asma_mm   = filter_df_by_apt(ASMA_MM_DF,  .apt = apt_icao),
                #
                txot_yy   = filter_df_by_apt(TXOT_YY_DF,  .apt = apt_icao),
                txot_mm   = filter_df_by_apt(TXOT_MM_DF,  .apt = apt_icao),
                #
                txin_yy   = filter_df_by_apt(TXIN_YY_DF,  .apt = apt_icao),
                txin_mm   = filter_df_by_apt(TXIN_MM_DF,  .apt = apt_icao),
                #
                pddly_yy  = filter_df_by_apt(PDDLY_YY_DF, .apt = apt_icao),
                pddly_mm  = filter_df_by_apt(PDDLY_MM_DF, .apt = apt_icao),
                pddly_avg = filter_df_by_apt(PDDLY_AVG_DF,.apt = apt_icao),
                #
                turn_yy   = filter_df_by_apt(TURN_YY_DF,  .apt = apt_icao),
                turn_mm   = filter_df_by_apt(TURN_MM_DF,  .apt = apt_icao)
                #
                #slot   = filter_df_by_apt(SLOT_DF,  .apt = apt_icao)
        ) 
}

