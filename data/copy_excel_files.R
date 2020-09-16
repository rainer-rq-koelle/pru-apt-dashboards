# copy over the Excel files 

library(fs)

# AIRPORT from AIU Portal
basedir <- "C:/Users/spi/repos/pru-portal/static/download/xls"
filenames <- c(
  "Airport_Traffic.xlsx",
  "Airport_Arrival_ATFM_Delay.xlsx",
  "ATFM_Slot_Adherence.xlsx",
  "ASMA_Additional_Time.xlsx",
  "Taxi-Out_Additional_Time.xlsx"
)

# for Sharepoint mapped drive
# basedir <- "https://coll.eurocontrol.int/sites/pru/dashboard/Data"
# files_in <- paste(basedir, filenames, sep = "/")
# purrr::walk2(
#   files_in,
#   files_out,
#   ~ download.file(.x, .y, method = "wininet", quiet = TRUE))

files_in <- fs::path_abs(filenames, start = basedir)
files_out <- fs::path_abs(filenames, start = here::here("data"))

fs::file_copy(files_in, files_out, overwrite = TRUE)


# AIRPORT from COVID
basedir <- 'G:/HQ/dgof-pru/Data/DataProcessing/Covid19'
file_in <- fs::path_abs("1_Top_100_Airport_dep+arr_traffic_LTFM+LTBA (Synthesis).xlsx", start = basedir)
# file_in <- fs::path_abs("1_Top_100_Airport_dep+arr_traffic(Synthesis).xlsx", start = basedir)
file_out <- fs::path_abs("COVID-AIRPORT.xlsx", start = here::here("data"))

fs::file_copy(file_in, file_out, overwrite = TRUE)

