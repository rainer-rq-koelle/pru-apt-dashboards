# ..........................................................................----
# --- SET UP ----
# ---
# IMPORTANT: install htmlwidgets >= 1.5.2 from CRAN in order to avoid CSS units issues
library(here)
library(dplyr)
library(lubridate)

# IMPORTANT: Data frames need to be generated via "APT_DSHBD_CREATE_DF.r" before execution
#source(here("R","APT_DSHBD_CREATE_DF.r"), encoding = "UTF8")

source(here("R","APT_DSHBD_UTILS.R"), encoding = "UTF8")

# ---- CALL RENDER DASHBOARDS ----

APT_DF <- APT_DF %>% arrange(AIRPORT) %>% mutate(idx = row_number())

APT_DF %>%
  #filter( AIRPORT == "EBBR") %>%   # for debug
  pull(AIRPORT) %>%
  purrr::walk(
    .f = function(icao) {
      cat(paste0("==>", icao, "...\n"))
      rmarkdown::render(
      input       = here("APT_DSHBD_RENDER.Rmd"),
      params      = prepare_params(icao), 
      output_file = here("docs", paste0(icao, ".html")))
      cat(paste0("==>", icao, "...end\n"))
      })
