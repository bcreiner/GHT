## GAM
#
GAM_timeseries_model <- function(data_frame, time, draws){
  model_name <- "GAM_timeseries"
  
  Lagged_weeks <- 8
  
  tmp_locs <- which(data_frame$Start <= (time - 7*Data_Lag + 7))
  
  tmp_today <- data_frame$DALYs[max(tmp_locs)]
  pred_vals <- matrix(tmp_today, Weeks_Out_To_Model + 1, draws)
  
  tmp_data <- data_frame[tmp_locs,]
  tmp_data$time <- 1:length(tmp_locs)
  tmp_data$EW <- as.numeric(format(tmp_data$Start, "%U"))
  for (i in 1:Weeks_Out_To_Model){
    tmp_data[,glue("out_lag{i}")] <- c(rep(NA,i), head(tmp_data$DALYs, -i))
  }

  tmp_epi_wk <- tmp_data$EW[max(tmp_locs)]
  
  tmp_formula <- as.formula(glue("DALYs ~ s(EW, bs = 'cc', k = 52) + s(out_lag1) + s(time)"))
  
  mod <- gam(tmp_formula, data = tmp_data)
  
  pred_epi_wks <- (tmp_epi_wk - 1 + 1:Weeks_Out_To_Model) %% 52 + 1
  tmp_time <- tmp_data$time[max(tmp_locs)] + 1
  for (i in 1:Lagged_weeks){
    assign(glue("tmp_lag{i}"), tmp_data$DALYs[max(tmp_locs) - i + 1])
  }

  tmp_df <- data.frame(EW = pred_epi_wks[1], time = tmp_time, out_lag1 = tmp_lag1, out_lag2 = tmp_lag2,
                       out_lag3 = tmp_lag3, out_lag4 = tmp_lag4, out_lag5 = tmp_lag5, out_lag6 = tmp_lag6,
                       out_lag7 = tmp_lag7, out_lag8 = tmp_lag8)
  
  pred <- predict(mod, newdata = tmp_df, se = TRUE)
  pred_vals[2,] <- pmax(rnorm(draws, mean = pred$fit, sd = pred$se.fit), 0)
  #pred_vals[2,] <- max(pred$fit,0)
  
  for(d_num in 1:draws){
    for (i in 2:Lagged_weeks){
      assign(glue("t{i}"), get(glue("tmp_lag{i-1}")))
    }
    for (t_num in 2:Weeks_Out_To_Model){
      t1 <- max(pred_vals[t_num, d_num],0)
      
      tmp_df <- data.frame(EW = pred_epi_wks[t_num], time = tmp_time + t_num - 1, out_lag1 = t1,
                           out_lag2 = t2, out_lag3 = t3, out_lag4 = t4, out_lag5 = t5,
                           out_lag6 = t6, out_lag7 = t7, out_lag8 = t8)
      
      pred <- predict(mod, newdata = tmp_df, se = TRUE)
      pred_vals[t_num + 1,d_num] <- max(rnorm(1, mean = pred$fit, sd = pred$se.fit), 0)
      #
      for (i in 2:Lagged_weeks){
        assign(glue("t{i}"), get(glue("tmp_lag{i-1}")))
      }      
    }
  }
  
  pred_df <- data.frame(pred_vals)
  
  GHT_pred_df <- data.frame(mapply(DALY_to_GHT, pred_df))
  draw_num_w_pad <- str_pad(0:(draws-1), width = 1+floor(log10(draws-1)), pad = "0")
  names(pred_df) <- paste("pred_draw",draw_num_w_pad, sep = "_")
  names(GHT_pred_df) <- paste("GHT_draw",draw_num_w_pad, sep = "_")
  
  
  GHT_obs <- DALY_to_GHT(data_frame$DALYs[max(tmp_locs)+0:Weeks_Out_To_Model])
  
  if (Data_Lag == 0){
    tmp_future <- c(0, rep(1, Weeks_Out_To_Model))
  } else {
    tmp_future <- c(0, rep(0, Data_Lag), rep(1, Weeks_Out_To_Model - Data_Lag))
  }
  
  out_df <- data.frame(week = data_frame$Start[max(tmp_locs)+0:Weeks_Out_To_Model],
                       mod_name = rep(model_name, Weeks_Out_To_Model + 1),
                       future = tmp_future,
                       out_type = rep("continuous", Weeks_Out_To_Model + 1),
                       obs = data_frame$DALYs[max(tmp_locs)+0:Weeks_Out_To_Model],
                       pred_df,
                       GHT_obs = GHT_obs,
                       GHT_pred_df)
  
  return(out_df)
}
