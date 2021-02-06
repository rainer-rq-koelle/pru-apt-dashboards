with LIST_AIRPORT as 											
(select code as arp_code,											
        id as arp_id ,											
       dashboard_name as arp_name,											
       SUBSTR (CODE, 1, 2) AS ICAO2LETTER											
 from prudev.pru_airport where code in 											
 ( 											
'EDDF',	'EHAM',	'LFPG',	'EGLL',	'LEMD',	'EDDM',	'LEBL',	'LTFM',	'LIRF',	'EGKK',	'LOWW',	
'LSZH',	'EKCH',	'ENGM',	'EIDW',	'LIMC',	'ESSA',	'EBBR',	'LTFJ',	'EDDL',	'LFPO',	'LPPT',	
'LGAV',	'LEPA',	'LTAI',	'EGCC',	'EGSS',	'EFHK',	'EPWA',	'EDDT',	'LSGG',	'LLBG',	'LKPR',	
'EDDH',	'LFMN',	'EGGW',	'EDDK',	'LEMG',	'LTBA',	'EDDS',	'EGPH',	'GCLP',	'LROP',	'LHBP',	
'LFLL',	'UKBB',	'EGBB',	'LFML',	'LEAL',	'LPPR',	'LFBO',	'LIPZ',	'LIME',	'GMMN',	'LTAC',	
'EDDB',	'ENBR',	'EVRA',	'LFSB',	'LIML',	'LIRN',	'EGPF',	'EGLC',	'LIPE',	'LTBJ',	'ELLX',	
'EDDP',	'LICC',	'LFBD',	'LEIB',	'GCXO',	'LEVC',	'LYBE',	'ESGG',	'GCTS',	'EGNX',	'EGGD',	
'ENZV',	'LFRS',	'EDDV',	'EPKK',	'LPFR',	'LCLK',	'LBSF',	'GCRR',	'LEZL',	'LMML',	'LGTS',	
'LGIR',	'EBCI',	'ENVA',	'LICJ',	'LFPB',	'ESSB',	'EGAA',	'EGPD',	'LIRA',	'EDDN',	'GMMX',	'LEBB',   
--# Extended APT via Layout 											
'EDDC', 'EDDE', 'EDDG', 'EDDR', 'EDDW', 'EETN', 'EGNT', 'EICK', 'EYVI',  'GCFV','LDZA',											
'LIMF', 'LJLJ', 'LZIB',											
--# Extended APT via Covid study											
'EBLG', 'LATI', 'LQSA', 'LUKK', 'LWSK', 'LYPG', 'UDYZ', 'UGTB'											
 )											
 ),											
											
 AIRPORT_DAY 											
as 											
(select a.arp_id,											
       a.arp_code,											
       a.arp_name,											
       a.icao2letter,											
       t.day_date,											
       t.year,											
       t.month, 											
       t.WEEK_NB_YEAR,											
       t.DAY_TYPE,											
       t.day_name,											
       t.WEEK,											
       TO_CHAR ( t.day_date, 'd')  AS day_of_week,											
       TO_CHAR ( t.day_date, 'mmdd') AS MMDD											
 from LIST_AIRPORT a, pru_time_references t											
where											
      t.day_date >= '01-jan-2019'											
  AND t.day_date < TRUNC (SYSDATE)											
),											
											
ARP_SYN_ARR											
    AS											
(SELECT SUM (nvl(f.ades_day_all_trf,0)) AS ttf_arr,											
                 u.arp_code,											
                 u.day_date  AS FLIGHT_DATE,											
                 u.arp_id, u.arp_name, u.ICAO2LETTER,											
                 u.day_date,											
       u.year,											
       u.month, 											
       u.WEEK_NB_YEAR,											
       u.DAY_TYPE,											
       u.day_name,											
       u.WEEK, 											
       u.day_of_week											
 FROM AIRPORT_DAY u left join ARU_SYN.AGG_ARR_DAY f 											
        on trunc(f.ades_DAY_FLT_DATE) = u.day_date											
       and f.ades_day_ades_ctfm = u.arp_code 											
 GROUP BY											
       u.arp_code, u.day_date,u.arp_id, u.arp_name, u.ICAO2LETTER,u.day_date,											
       u.year,											
       u.month, 											
       u.WEEK_NB_YEAR,											
       u.DAY_TYPE,											
       u.day_name,											
       u.WEEK, 											
       u.day_of_week  ),											
                  											
ARP_SYN_DEP											
    AS											
        (  SELECT SUM (nvl(adep_day_all_trf,0)) AS TTF_DEP,											
                   u.ARP_CODE,											
                  u.day_date  AS FLIGHT_DATE,											
                   u.arp_id, u.arp_name, u.ICAO2LETTER ,											
                     u.year,											
       u.month, 											
       u.WEEK_NB_YEAR,											
       u.DAY_TYPE,											
       u.day_name,											
       u.WEEK, 											
       u.day_of_week											
              FROM AIRPORT_DAY u left join ARU_SYN.AGG_DEP_DAY f 											
               on trunc(f.adep_DAY_FLT_DATE) = u.day_date											
                  and f.adep_day_adep = u.arp_code 											
             and f.adep_day_adep = u.arp_code 											
            GROUP BY   u.arp_code, u.day_date,u.arp_id, u.arp_name, u.ICAO2LETTER,u.year,											
       u.month, 											
       u.WEEK_NB_YEAR,											
       u.DAY_TYPE,											
       u.day_name,											
       u.WEEK, 											
       u.day_of_week  ), 											
                  											
 ARP_SYN_DEP_ARR											
 as 											
 ( select d.arp_code, d.flight_date,  d.arp_id, d.arp_name, d.icao2letter, d.ttf_dep, a.ttf_arr, nvl(d.ttf_dep,0)+nvl(a.ttf_arr,0) as ttf_dep_arr,  d.month, 											
       d.WEEK_NB_YEAR,											
       d.DAY_TYPE,											
       d.day_name,											
       d.WEEK, 											
       d.day_of_week											
 from arp_syn_dep d, arp_syn_arr a                											
 where d.arp_id = a.arp_id and d.flight_date = a.flight_date											
 and    d.flight_date = a.flight_date											
          ), 											
       											
  											
 AIRP_CALC as											
(select arp_code,											
        arp_name,											
       flight_date,											
       icao2letter,											
       ttf_dep,											
       ttf_arr,											
       ttf_dep_arr,											
       month, 											
       WEEK_NB_YEAR,											
       DAY_TYPE,											
       day_name,											
       WEEK, 											
       day_of_week,   											
       LAG (TTF_DEP_ARR, 364) OVER (PARTITION BY arp_code ORDER BY flight_date)  TTF_DEP_ARR_PREV_YEAR,											
       LAG (flight_date, 364) OVER (PARTITION BY arp_code ORDER BY flight_date)FLIGHT_DATE_PREV_YEAR,											
       LAG (TTF_DEP_ARR, (extract (year from flight_date)-2019) *364) OVER (PARTITION BY arp_code ORDER BY flight_date) TTF_DEP_ARR_2019,											
       LAG (flight_date, (extract (year from flight_date)-2019) *364) OVER (PARTITION BY arp_code ORDER BY flight_date) FLIGHT_DATE_2019,											
       AVG (TTF_DEP_ARR)  OVER (PARTITION BY arp_code ORDER BY flight_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS ROLLING_WEEK_YEAR,											
       AVG (TTF_DEP_ARR)  OVER (PARTITION BY arp_code,week_nb_year,week  ORDER BY week)AS avg_week											
      FROM ARP_SYN_DEP_ARR)  ,     											
 											
  AIRP_CACL_PREV  as											
  ( 											
      select arp_code,											
        arp_name,											
       flight_date,											
       icao2letter,											
       ttf_dep,											
       ttf_arr,											
       ttf_dep_arr,											
       month, 											
       WEEK_NB_YEAR,											
       DAY_TYPE,											
       day_name,											
       WEEK, 											
       day_of_week,   											
       TTF_DEP_ARR_PREV_YEAR,											
       FLIGHT_DATE_PREV_YEAR,											
       TTF_DEP_ARR_2019,											
       FLIGHT_DATE_2019,											
       ROLLING_WEEK_YEAR,											
       avg_week,											
       AVG (TTF_DEP_ARR_PREV_YEAR)  OVER (PARTITION BY arp_code ORDER BY flight_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS ROLLING_WEEK_PREV_YEAR,											
       AVG (TTF_DEP_ARR_PREV_YEAR)  OVER (PARTITION BY arp_code,week_nb_year,week  ORDER BY week)AS avg_week_PREV_YEAR,											
       AVG (TTF_DEP_ARR_2019)  OVER (PARTITION BY arp_code ORDER BY flight_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS ROLLING_WEEK_2019,											
       AVG (TTF_DEP_ARR_2019)  OVER (PARTITION BY arp_code,week_nb_year,week  ORDER BY week)AS avg_week_2019											
      FROM AIRP_CALC											
  )											
											
											
SELECT											
ARP_CODE                                                                        AS APT_ICAO,											
ARP_NAME                                                                        AS ARP_NAME,											
TO_CHAR(FLIGHT_DATE,'YYYY-MM-DD')                                               AS DAY,											
TTF_DEP_ARR                                                                     AS FLTS,   											
TTF_DEP_ARR_2019                                                                AS FLTS_2019,											
CASE WHEN ROLLING_WEEK_2019 <> 0											
     THEN ROLLING_WEEK_YEAR/ROLLING_WEEK_2019 -1											
     ELSE NULL											
END                                                                             AS MOV_AVG_WK       											
FROM AIRP_CACL_PREV											
WHERE FLIGHT_DATE >=TO_DATE('01-09-2020','DD-MM-YYYY')											
