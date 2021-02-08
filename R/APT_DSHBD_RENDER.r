# ..........................................................................----
# --- SET UP ----
# ---
# IMPORTANT: install htmlwidgets >= 1.5.2 from CRAN in order to avoid CSS units issues

# IMPORTANT: Data frames need to be generated via "APT_DSHBD_CREATE_DF.r" before execution
library(here)
source(here("R","APT_DSHBD_CREATE_DF.r"), encoding = "UTF8")

library(dplyr)
library(lubridate)
source(here("R","APT_DSHBD_UTILS.R"), encoding = "UTF8")
#.----

# ---- CALL RENDER DASHBOARDS ----
APT_DF <- APT_DF %>% arrange(AIRPORT) %>% mutate(idx = row_number())
problematic_airports <- c("EBCI", "EBLG",
                          "EGAA", "EGNX",
                          "ENBR", "ENVA", "ENZV",
                          "EPKK",
                          "ESSB",
                          "GMMN", "GMMX",
                          "LATI",
                          "LCLK",
                          "LFBD", "LFBO", "LFPB", "LFRS",
                          "LGIR","LGTS",
                          "LICJ",
                          "LLBG",
                          "LQSA",
                          "LTAC", "LTAI", "LTBA", "LTBJ", "LTFJ", "LTFM",
                          "LUKK",
                          "LWSK",
                          "LYBE", "LYPG",
                          "UDYZ",
                          "UGTB",
                          "UKBB"
                          )
APT_DF %>%
  filter(!AIRPORT %in% problematic_airports) %>%   # for debug
  # filter(idx > 120) %>%   # for debug
  # filter(AIRPORT %in% c("EBLG")) %>%   # for debug
  pull(AIRPORT) %>%
  # magrittr::extract(seq(3:5)) %>%        # for debug
  purrr::walk(
    .f = function(icao) {
      cat(paste0("==>", icao, "...\n"))
      rmarkdown::render(
      input       = here("APT_DSHBD_RENDER.Rmd"),
      params      = prepare_params(icao), 
      output_file = here("docs", paste0(icao, ".html")))
      cat(paste0("==>", icao, "...end\n"))
      })
