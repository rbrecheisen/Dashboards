source("dependencies.R")

library(stringr)
library(dplyr)
library(httr)
library(arrow)

# BASE_URL <- "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_"
# DATA_DICT_URL <- "https://www.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf"
# 
# download_tlc_data <- function(year, month, save_path = "/Users/ralph/dev/tools/parquet") {
#   if(!dir.exists(save_path)) {
#     dir.create(save_path)
#   }
#   month <- sprintf("%02d", month)
#   file_url <- paste0(BASE_URL, year, "-", month, ".parquet")
#   file_name <- paste0(save_path, "/yellow_tripdata_", year, "_", month, ".parquet")
#   response <- GET(file_url, write_disk(file_name, overwrite = TRUE))
#   if (http_status(response)$category == "Success") {
#     message("Downloaded: ", file_name)
#   } else {
#     message("Failed to download: ", file_url)
#   }
# }
# 
# years <- c(2023, 2024)
# months <- 1:12
# 
# for(year in years) {
#   for(month in months) {
#     download_tlc_data(year, month)
#     message(paste0("downloaded year ", year, " and month ", month))
#   }
# }

df <- open_dataset("parquet", format = "parquet")
df %>% glimpse()

explanations <- list(
  "tpep_pickup_datetime"="Date and time when meter was started",
  "tpep_dropoff_datetime"="Date and time when meter was stopped",
  "passenger_count"="Number of passengers in the vehicle at the time the meter was started",
  "trip_distance"="Distance in miles driven between pickup and dropoff",
  "RatecodeID"="Final rate code at the end of the trip. Options: 1=Standard rate, 2=JFK, 3=Newardk, 4=Nassua or Westchester, 5=Negotiated fare, 6=Group ride",
  "store_and_fwd_flag"="Whether trip record was stored temporarily before being sent to the vendor. Options: Y=Yes, N=No",
  "PULocationID"="Taxi zone when meter was started",
  "DOLocationID"="Taxi zone when meter was stopped",
  "payment_type"="Method of payment. Options: 1=Credit card, 2=Cash, 3=No charge, 4=Dispute, 5=Unknown, 6=Voided trip",
  "fare_amount"="Time-and-distance far calculated",
  "extra"="Miscellenous extras and surcharges. Currently only 0.50 and 1.00 dollar extras for rush hour and overnight trips",
  "mta_tax"="0.50 dollar MTA tax automatically added based on metered rate in use",
  "tip_amount"="Tip amount. Automatically added for credit cards. Cash tips not included",
  "tolls_amount"="Total amount of all tolls paid in this trip",
  "improvement_surcharge"="0.30 dollar surcharge at the flag drop",
  "total_amount"="Total amount charged to passengers excluding cash tips",
  "congestion_surcharge"="Total amount collected in trip for congestion surcharge",
  "airport_fee"="1.25 dollars for pick up at LaGuardia and JF Kennedy airports"
)
explanations
