input_data <- read.csv(glue("{GHT_input_dir}/processed_formatted_data_w_dalys_ght.csv"))
input_data$start_date <- as.Date(input_data$start_date)
input_data$end_date <- as.Date(input_data$end_date)

input_data$Start <- input_data$start_date
input_data$End <- input_data$end_date
input_data$DALYs <- input_data$dalys_per_100k

