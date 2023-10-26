input_data <- read.csv(glue("{GHT_input_dir}/forecast_modeling_inputs.csv"))
input_data$start_date <- as.Date(input_data$start_date)
input_data$end_date <- as.Date(input_data$end_date)
input_data <- input_data[order(input_data$start_date),]

toss_based_on_old_data <- which(input_data$start_date < as.Date("2018-01-01"))
input_data <- input_data[-toss_based_on_old_data,]

toss_based_on_measure <- which(input_data$measure != "cases")
input_data <- input_data[-toss_based_on_measure,]

### Generally drop these places are they are all 0 or very unmodelable (and mostly 0)
toss_special <- which(input_data$cause_name == "Meningitis")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Measles" & input_data$location_name == "Greater London")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Covid-19" & input_data$location_name == "Hong Kong")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Measles" & input_data$location_name == "Hong Kong")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Dengue" & input_data$location_name == "Hong Kong")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Measles" & input_data$location_name == "Israel")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Covid-19" & input_data$location_name == "Shanghai")
input_data <- input_data[-toss_special,]

toss_special <- which(input_data$cause_name == "Measles" & input_data$location_name == "Singapore")
input_data <- input_data[-toss_special,]




### Notes for 2023_09_28

drop_last_covid <- c("Ireland", "Israel")
for (tmp_loc_num in 1:length(drop_last_covid)){
  tmplocs <- which(input_data$cause_name == "Covid-19" & input_data$location_name == drop_last_covid[tmp_loc_num])
  input_data <- input_data[-tail(tmplocs,1),]
}

drop_last_tb <- c("New York City")
for (tmp_loc_num in 1:length(drop_last_tb)){
  tmplocs <- which(input_data$cause_name == "Tuberculosis" & input_data$location_name == drop_last_tb[tmp_loc_num])
  input_data <- input_data[-tail(tmplocs,1),]
}

drop_last_4_tb <- c("Sao Paulo")
for (tmp_loc_num in 1:length(drop_last_4_tb)){
  tmplocs <- which(input_data$cause_name == "Tuberculosis" & input_data$location_name == drop_last_4_tb[tmp_loc_num])
  input_data <- input_data[-tail(tmplocs,4),]
}

# toss_NA <- which(is.na(input_data$value))
# input_data <- input_data[-toss_NA,]

# toss_based_on_measurement_window <- which(as.numeric(input_data$end_date - input_data$start_date) > 10)
# input_data <- input_data[-toss_based_on_measurement_window,]

UIDs <- unique(input_data$location_id)
UCauses <- unique(input_data$cause_name)




# input_data <- input_data[order(input_data$start_date),]
# X <- table(input_data$location_name, input_data$cause_name)
# Y <- matrix(NA, length(UIDs), length(UCauses))
# colnames(Y) <- UCauses
# row.names(Y) <- sapply(UIDs, function(x){
#   tmp_mat <- input_data[which(input_data$location_id == x),]
#   return(tmp_mat$location_name[1])
# })
# Z <- Y
# for (i in 1:length(UIDs)){
#   for (j in 1:length(UCauses)){
#     tmp_mat <- input_data[which(input_data$location_id == UIDs[i] & input_data$cause_name == UCauses[j]),]
#     if (dim(tmp_mat)[1] > 0){
#       Y[i,j] <- max(as.numeric(diff(tmp_mat$start_date)))
#       if (Y[i,j] > 7){
#         tmp_loc <- which.max(as.numeric(diff(tmp_mat$start_date)))
#         Z[i,j] <- as.character(tmp_mat$start_date[tmp_loc])
#       }
#     }
#   }
# }
#

# COVID_ifr <- fread(glue("{IHME_COVID_dir}/infection_fatality_ratio.csv"))
# COVID_idr <- fread(glue("{IHME_COVID_dir}/infection_detection_ratio.csv"))
# COVID_ifr$date <- as.Date(COVID_ifr$date)
# COVID_idr$date <- as.Date(COVID_idr$date)


# cause_num <- 1
# for (id_num in 1:length(UIDs)){
#   if (UIDs[id_num] == 796){
#     tmp_loc_id <- 527
#   } else if (UIDs[id_num] == 4624){
#     tmp_loc_id <- 95
#   } else {
#     tmp_loc_id <- UIDs[id_num]
#   }
#   
#   tmp_ifr <- COVID_ifr[which(COVID_ifr$location_id == tmp_loc_id), ]
#   tmp_idr <- COVID_idr[which(COVID_idr$location_id == tmp_loc_id), ]
#   toss <- which(tmp_ifr$mean == Inf | tmp_idr$mean == Inf)
#   tmp_ifr <- tmp_ifr[-toss,]
#   tmp_idr <- tmp_idr[-toss,]
#   
#   tmp_cfr <- tmp_idr
#   tmp_cfr$mean <- NA
#   
#   tmp_cfr$mean <- tmp_ifr$mean / (tmp_idr$mean + quantile(tmp_idr$mean[tmp_idr$mean > 0], prob = .001, na.rm = TRUE))
#   
#   plot(tmp_cfr$date, tmp_cfr$mean, type = 'l')
#   tmp_locs <- which(tmp_cfr$date <= as.Date("2021-12-31"))
#   tmp_past_mean <- mean(tmp_cfr$mean[tmp_locs], na.rm = TRUE)
#   tmp_cfr$adjustment <- tmp_cfr$mean / tmp_past_mean
#   plot(tmp_cfr$date, tmp_cfr$mean / tmp_past_mean, ylim = c(0,3), type = 'l')
#   abline(h = 1, lty = 2)
# 
#   tmp_full_data_locs <- which(input_data$location_id == UIDs[id_num] & input_data$cause_name == UCauses[cause_num])
#   tmp_mat <- input_data[tmp_full_data_locs,]
#   tmp_mat_old <- tmp_mat
#   for (ix in 1:length(tmp_full_data_locs)){
#     tmp_cfr_loc <- min(which(tmp_cfr$date > tmp_mat$start_date[ix] & tmp_cfr$adjustment > 0))
#     tmp_adjustment <- tmp_cfr$adjustment[tmp_cfr_loc]
#     if (!is.na(tmp_adjustment)){
#       tmp_mat$dalys_per_100k[ix] <- tmp_mat$dalys_per_100k[ix] * tmp_adjustment
#       tmp_mat$ght[ix] <- DALY_to_GHT(tmp_mat$dalys_per_100k[ix])
#     }
#   }
#   input_data[tmp_full_data_locs,] <- tmp_mat
# }  


 
input_data$Start <- input_data$start_date
input_data$End <- input_data$end_date
input_data$DALYs <- input_data$dalys_per_100k
