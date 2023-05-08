rm(list = ls())

#
# Libraries and Helpers
library(readxl)
require(glue)
require(RColorBrewer)
require(mgcv)
require(WaveletComp, lib = "~/packages")
require(stringr)
require(data.table)
require(boot)
library(tseries)
library(readr)
library(ggplot2)
library(forecast)
require(fma)
library(fpp2, lib = "~/packages")
library(TTR)
library(dplyr)



# invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
# '%nin%' = Negate(`%in%`)
# '%notlike%' <- Negate('%like%')

User <- Sys.getenv("USER")
repo_dir <- glue("/ihme/homes/{User}/repos/GHT")

data_date <- "2023_04_28_mvp"
today <- as.Date("2023-05-05")


source(glue("{repo_dir}/code/0_paths.r"))
source(glue("{repo_dir}/code/1_functions.r"))
source(glue("{repo_dir}/code/2_constants.r"))
source(glue("{repo_dir}/code/3_load_suite_of_models.r"))
source(glue("{repo_dir}/code/4_load_data.r"))




for (id_num in 1:length(UIDs)){
  for (cause_num in 1:length(UCauses)){
    if (UIDs[id_num] != 4849 | UCauses[cause_num] != "dengue"){
      tmp_locs <- which(input_data$location_id == UIDs[id_num] & input_data$cause_name == UCauses[cause_num])
      if (length(tmp_locs)){
        tmp_data <- input_data[tmp_locs,]
        #
        Data_Lag <- floor(as.numeric(today - max(input_data$End))/7)
        Weeks_Out_To_Model <- Forecast_Weeks + Data_Lag
        #
        message(glue("Modling {tmp_data$cause_name[1]} in {tmp_data$location_name[1]}"))
        message(glue("There are {length(tmp_data$dalys_per_100k)} weeks of data"))
        message(glue("The lag is {Data_Lag} week(s)"))
        #
        test_dates <- seq(tmp_data$Start[round(length(tmp_data$Start) * test_fraction)], 
                          max(tmp_data$Start) - Forecast_Weeks * 7, by = 7)
        #
        all_past_pred <- vector("list", length(suite_of_models))
        for (mod_num in 1:length(suite_of_models)){
          message(glue("Starting {suite_of_models[mod_num]}. Model {mod_num} out of {length(suite_of_models)}"))
          #
          tmp_model <- get(glue("{suite_of_models[mod_num]}_model"))
          all_past_pred[[mod_num]] <- vector("list", length(test_dates))
          #
          for (tmp_date_num in 1:length(test_dates)){
            all_past_pred[[mod_num]][[tmp_date_num]] <- tmp_model(tmp_data, test_dates[tmp_date_num], draws)
          }
        }
        #
        # make_plot()
        #
        past_pred_array <- generate_pred_array(all_past_pred)
        
        individual_model_RPS <- RPS_aggregator(all_past_pred)
        
        individual_skill_matrix <- 1- individual_model_RPS / t(replicate(length(suite_of_models), individual_model_RPS[1,]))
        
        optim_function <- function(vals){
          weights <- create_weights(vals)
          val <- -ensemble_RPS_by_week(weights,week_to_check,past_pred_array,individual_model_RPS[1,week_to_check])
          return(val)
        }
        
        create_weights <- function(vals){
          weights <- rep(0, length(suite_of_models))
          remaining <- 1
          for (mod_num in 1:(length(vals) - 1)){
            weights[mod_num] <- remaining * inv.logit(vals[mod_num])
            remaining <- 1 - sum(weights)
          }
          weights[length(vals)] <- remaining
          return(weights)
        }
        #
        #
        #
        
        create_vals <- function(weights){
          vals <- rep(0, length(suite_of_models))
          vals[1] <- logit(weights[1])
          for (mod_num in 2:(length(weights) - 1)){
            vals[mod_num] <- logit(weights[mod_num] / (1 - sum(weights[1:(mod_num-1)])))
          }
          return(vals)
        }
        
        best_fit <- vector("list", Weeks_Out_To_Model)
        for (forecast_week in 1:Weeks_Out_To_Model){
          week_to_check <- forecast_week
          message(glue("Checking week {forecast_week - Data_Lag}"))
          tmp_Monte <- vector("list", 100)
          for (i in 1:100){
            tmp_init_guess <- runif(length(suite_of_models))
            tmp_init_guess <- tmp_init_guess / sum(tmp_init_guess)
            tmp_Monte[[i]] <- optim(create_vals(tmp_init_guess), optim_function, method = "BFGS")
          }
          
          skills <- unlist(lapply(tmp_Monte, function(x) x$value))
          best_fit[[week_to_check]] <- tmp_Monte[[which.min(skills)]]
        }
        
        best_weights <- lapply(best_fit, function(x)create_weights(x$par))
        
        
        all_current_pred <- vector("list", length(suite_of_models))
        is_continuous <- rep(FALSE, length(suite_of_models))
        for (mod_num in 1:length(suite_of_models)){
          message(glue("Starting {suite_of_models[mod_num]}. Model {mod_num} out of {length(suite_of_models)}"))
          #
          tmp_model <- get(glue("{suite_of_models[mod_num]}_model"))
          #
          all_current_pred[[mod_num]] <- tmp_model(tmp_data, max(tmp_data$Start), draws)
          if (all_current_pred[[mod_num]]$out_type[1] == "continuous") is_continuous[mod_num] <- TRUE
        }
        
        continuous_mods <- which(is_continuous)
        best_continuous_weights <- lapply(best_weights, function(x) as.numeric(is_continuous) * x)
        best_continuous_weights <- lapply(best_continuous_weights, function(x) x / sum(x))
        
        final_pred <- all_current_pred[[1]]
        final_pred <- final_pred[,c("week", names(final_pred)[which(names(final_pred) %like% "pred_draw")], 
                                    names(final_pred)[which(names(final_pred) %like% "GHT_draw")])]
        pred_draw_rows <- which(names(final_pred) %like% "pred_draw")
        GHT_draw_rows <- which(names(final_pred) %like% "GHT_draw")
        final_pred[-1,names(final_pred)[which(names(final_pred) %like% "GHT_draw")]] <- 0
        final_pred[-1,names(final_pred)[which(names(final_pred) %like% "pred_draw")]] <- 0
        final_pred$week <- seq(final_pred$week[1], by = "1 week", length = length(final_pred$week))
        for (mod_num in 1:length(suite_of_models)){
          tmp_pred <- all_current_pred[[mod_num]]
          tmp_pred <- tmp_pred[,c("week", names(tmp_pred)[which(names(tmp_pred) %like% "pred_draw")], 
                                  names(tmp_pred)[which(names(tmp_pred) %like% "GHT_draw")])]
          tmp_pred$week <- seq(tmp_pred$week[1], by = "1 week", length = length(tmp_pred$week))
          for (forecast_week in 1:Weeks_Out_To_Model){
            final_pred[1 + forecast_week, GHT_draw_rows] <- final_pred[1 + forecast_week, GHT_draw_rows] + best_weights[[forecast_week]][mod_num] * tmp_pred[1 + forecast_week, GHT_draw_rows] 
            final_pred[1 + forecast_week, pred_draw_rows] <- final_pred[1 + forecast_week, pred_draw_rows] + best_continuous_weights[[forecast_week]][mod_num] * tmp_pred[1 + forecast_week, pred_draw_rows] 
          }
        }
        
        final_pred[,GHT_draw_rows] <- round(final_pred[,GHT_draw_rows])
        
        final_data <- combine_past_and_future(tmp_data, final_pred)
        final_data$skill <- NA
        final_data$skill[length(final_data$skill)+(-3:0)] <- -unlist(lapply(best_fit, function(x)x$value))
        tmp_text <- glue("{GHT_output_dir}/predictions_location_id_{final_data$location_id[1]}_{final_data$cause_name[1]}.csv")
        write.csv(final_data, tmp_text)
      }
    }
  }
}


