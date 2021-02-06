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

# AIRPORT CODES SELECTION ----

APTS <- APT_DF %>%  pull(AIRPORT)
#APTS <- 'EBBR'    # DEBUG

# ---- CALL RENDER DASHBOARDS ----

APT_RANGE <- 1:length(APTS)
APT_RANGE <- 2:3
APTS %>%
  magrittr::extract(APT_RANGE) %>%
  purrr::walk(
    .f = ~rmarkdown::render(
      input       = here("APT_DSHBD_RENDER.Rmd"),
      params      = prepare_params(.), 
      output_file = here("docs", paste0(., ".html"))
  ))
