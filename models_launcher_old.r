setwd("C:/Users/bcreiner/Dropbox/IHME/GHT/ToyModel")
# setwd("D:/Dropbox/IHME/GHT/ToyModel")


# install.packages("readxl")
library(readxl)
require(glue)
require(RColorBrewer)
require(mgcv)
require(WaveletComp)
require(stringr)
require(data.table)
require(boot)
library(tseries)
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)

source("code/RPS_calculator.r")

GHT_bins <- c(0, 10, 100, 500, 2000, 6000, 12000, 100000, 1000000)
GHT_level_num <- length(GHT_bins) - 1

test_fraction <- 0.8
draws <- 100
Data_Lag <- 1
Forecast_Weeks <- 4
Weeks_Out_To_Model <- Forecast_Weeks + Data_Lag

location_name <- "Singapore"
location_id <- 69
cause_name <- "Dengue"
cause_id <- 357

pop <- 5454000

DALY_to_GHT <- function(x) return(9 - findInterval(100000 * x / pop, GHT_bins))

data <- read.csv(glue("processed_data/{location_name}.csv"))
data$Start <- as.Date(data$Start)
data$End <- as.Date(data$End)
data$YF <- as.factor(data$year)

test_dates <- seq(data$Start[round(length(data$Start) * test_fraction)], 
                  max(data$Start) - Forecast_Weeks * 7, by = 7)
# run_dates <- seq(as.Date("2020-01-01"), length = 10, by = 7)

source("code/DALY_converter.r")


data$DALYs <- data[,cause_name] * DALY_per_weekly_case
data$GHT <- DALY_to_GHT(data$DALYs)

suite_of_models <- unlist(lapply(strsplit(list.files("code/suite_of_models"), "[.]"), function(x)x[[1]]))
suite_of_models <- c("constant", setdiff(suite_of_models, "constant"))

sapply(glue("code/suite_of_models/{suite_of_models}.r"), source)


all_past_pred <- vector("list", length(suite_of_models))
for (mod_num in 1:length(suite_of_models)){
  message(glue("Starting {suite_of_models[mod_num]}. Model {mod_num} out of {length(suite_of_models)}"))
  #
  tmp_model <- get(glue("{suite_of_models[mod_num]}_model"))
  all_past_pred[[mod_num]] <- vector("list", length(test_dates))
  #
  pb = txtProgressBar(min = 0, max = length(test_dates), initial = 0) 
  for (tmp_date_num in 1:length(test_dates)){
    setTxtProgressBar(pb,tmp_date_num)
    all_past_pred[[mod_num]][[tmp_date_num]] <- tmp_model(data, test_dates[tmp_date_num], draws)
  }
  close(pb)
}



YMAX <- max(unlist(lapply(all_past_pred, function(x)lapply(x, function(y)max(y[which(names(y) %like% "pred_draw")])))))
par(mfrow = c(2,1))
plot(data$Start, data$DALYs, type = 'l', ylim = c(0, YMAX), xlim = range(test_dates))
for (i in 1:length(suite_of_models)){
  for (j in 1:length(test_dates)){
    tmp_mat <- all_past_pred[[i]][[j]]
    for (k in 1:draws){
      if (k < 11){
        tmp_y <- tmp_mat[,glue("pred_draw_0{k-1}")]
      } else {
        tmp_y <- tmp_mat[,glue("pred_draw_{k-1}")] 
      }
      lines(tmp_mat$week, tmp_y, col = i+1)
    }
    tmp_y <- apply(tmp_mat[,which(names(tmp_mat) %like% "pred_draw")], 1, mean)
    lines(tmp_mat$week, tmp_y, col = i+1, lwd = 2)
  }
}
lines(data$Start, data$DALYs, lwd = 3)
#
plot(data$Start, data$GHT, type = 'l', xlim = range(test_dates), ylim = c(0,length(GHT_bins) - 1))
for (i in 1:length(suite_of_models)){
  for (j in 1:length(test_dates)){
    tmp_mat <- all_past_pred[[i]][[j]]
    for (k in 1:draws){
      if (k < 11){
        tmp_y <- tmp_mat[,glue("GHT_draw_0{k-1}")]
      } else {
        tmp_y <- tmp_mat[,glue("GHT_draw_{k-1}")] 
      }
      lines(tmp_mat$week, tmp_y, col = i+1)
    }
    tmp_y <- apply(tmp_mat[,which(names(tmp_mat) %like% "GHT_draw")], 1, mean)
    lines(tmp_mat$week, tmp_y, col = i+1, lwd = 2)
  }
}
lines(data$Start, data$GHT, lwd = 3)

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
for (mod_num in 1:length(suite_of_models)){
  message(glue("Starting {suite_of_models[mod_num]}. Model {mod_num} out of {length(suite_of_models)}"))
  #
  tmp_model <- get(glue("{suite_of_models[mod_num]}_model"))
  #
  all_current_pred[[mod_num]] <- tmp_model(data, max(data$Start), draws)
}


final_pred <- all_current_pred[[1]]
final_pred <- final_pred[,c("week", names(final_pred)[which(names(final_pred) %like% "GHT_draw")])]
draw_rows <- which(names(final_pred) %like% "GHT_draw")
final_pred[-1,names(final_pred)[which(names(final_pred) %like% "GHT_draw")]] <- 0
final_pred$week <- seq(final_pred$week[1], by = "1 week", length = length(final_pred$week))
for (mod_num in 1:length(suite_of_models)){
  tmp_pred <- all_current_pred[[mod_num]]
  tmp_pred <- tmp_pred[,c("week", names(tmp_pred)[which(names(tmp_pred) %like% "GHT_draw")])]
  tmp_pred$week <- seq(tmp_pred$week[1], by = "1 week", length = length(tmp_pred$week))
  for (forecast_week in 1:Weeks_Out_To_Model){
    final_pred[1 + forecast_week, draw_rows] <- final_pred[1 + forecast_week, draw_rows] + best_weights[[forecast_week]][mod_num] * tmp_pred[1 + forecast_week, draw_rows] 
  }
}

final_pred[,draw_rows] <- round(final_pred[,draw_rows] )


