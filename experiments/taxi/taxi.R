library(stringr)
library(arrow)

BASE_URL <- "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_"
YEARS <- c("2023", "2024")
MONTHS <- str_pad(1:12, width = 2, pad = "0")

FILES <- c()

for(year in YEARS) {
  for(month in MONTHS) {
    file_name <- paste0(year, "-", month, ".parquet")
    FILES <- c(FILES, file_name)
  }
}

destination_dir <- tempdir()
download.file(paste0(BASE_URL, ))