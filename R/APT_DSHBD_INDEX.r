# ..........................................................................----
# --- SET UP ----
# ---

cat("\014")        # Clear Environment           ----
rm(list = ls())    # Clear Console               ----

library(dplyr)     # Loading library [dplyr]     ----
library(lubridate) # Loading library [lubridate] ----
library(readxl)    # Loading library [readxl]    ----
library(stringr)   # Loading library [stringr]   ----
#.----

APT         <- readr::read_csv2("./data/PRU_AIRPORT_INFO.csv")

APT         <- APT%>%
                select(icao     = ICAO_CODE,
                       country  = CTRY_ABBREVIATION, 
                       name     = APDF_NAME)

summary_section <- function(df) 
                  {
                    country <- df %>% 
                                pull(country) %>% 
                                unique() %>% 
                                sort() 
    
                    id <- country %>% 
                            tolower() %>% 
                            str_replace_all(" ", "-")
  
                    str_glue("* [{country}](#{id})",country = country,id = id) %>% 
                      str_c(collapse = "\n")
                  }



airport_section <- function(df) 
                  {
                    ent <- df %>%
                            mutate(ggg = "* [NAME (APT)](APT.html)",
                                   ggg = str_replace_all(ggg, "APT", .data$icao),
                                   ggg = str_replace_all(ggg, "NAME", .data$name)) %>%
                            pull(ggg)

                    str_c(ent, collapse = "\n")
                  }

all_section     <- function(x, y) 
                  {
                    cc <- str_c(str_glue("## {country}", country = y$country[[1]]))
                    
                    aa <- airport_section(x)
                    
                    str_c(cc, "\n\n", aa , collapse = "\n")
                  }


country_section <- function(df) 
                  {
                    df %>%
                      group_by(country) %>%
                      arrange(name) %>%
                      group_map(.f = all_section) %>% 
                      unlist() %>% 
                      str_c(collapse = "\n\n")
                  }


writeUTF8 <- function(x, file, bom = FALSE) 
              {
                con <- file(file, "wb")
                
                if(bom) writeBin(BOM, con, endian="little")
                
                writeBin(charToRaw(x), con, endian="little")

                close(con)
              }

pre <- summary_section(APT)

big <- country_section(APT)

template <- "---
title: Dashboard - Airport
layout: default
---

<div class='index-links'>

{summary}

</div>

{block}
"

str_glue(template, summary = pre, block = big) %>% 
  writeUTF8(file = "./docs/_index.md")
