# ..........................................................................----
# --- SET UP ----
# ---
# IMPORTANT: install htmlwidgets >= 1.5.2 from CRAN in order to avoid CSS units issues
# IMPORTANT: Dataframes need to be generated via "APT_DSHBD_CREATE_DF.r" before execution


library(dplyr)     # Loading library [dplyr]     ----
library(lubridate) # Loading library [dplyr]     ----
source(here::here("./APT_DSHBD_UTILS.R"), encoding = "UTF8") # Call dedicated utils functions ----
#.----

# AIRPORT CODES SELECTION ----

APTS <- APT_DF %>%  select (AIRPORT)
APTS <- as.vector(t(APTS))
#APTS <- 'EBBR'    # DEBUG

# ---- CALL RENDER DASHBOARDS ----

APT_RANGE <- 1:length(APTS)

APTS %>%
  magrittr::extract(APT_RANGE) %>%
  purrr::walk(
              .f = ~rmarkdown::render(
                                      input       = "./APT_DSHBD_RENDER.Rmd",
                                      params      = prepare_params(.), 
                                      output_file = paste0("./docs/", ., ".html")
                                     )
             )
