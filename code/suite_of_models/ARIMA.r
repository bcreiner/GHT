## ARIMA
#
ARIMA_model <- function(data_frame, time, draws){
  model_name <- "ARIMA"
  
  tmp_locs <- which(data_frame$start_date <= (time - 7*Data_Lag + 7))
  
  dat_train = log(data_frame$dalys_per_100k[tmp_locs]+1e-8)

  dat_ts <- ts(dat_train)
  
  tmp_mod <- auto.arima(dat_ts)
  pred_df <- data.frame(rbind(rep(data_frame$dalys_per_100k[max(tmp_locs)], draws),
                   sapply(1:draws,function(x)exp(as.numeric(simulate(tmp_mod, future = TRUE, nsim = Weeks_Out_To_Model))))))

  
  GHT_pred_df <- data.frame(mapply(DALY_to_GHT, pred_df))
  draw_num_w_pad <- str_pad(0:(draws-1), width = 1+floor(log10(draws-1)), pad = "0")
  names(pred_df) <- paste("pred_draw",draw_num_w_pad, sep = "_")
  names(GHT_pred_df) <- paste("GHT_draw",draw_num_w_pad, sep = "_")
  
  
  GHT_obs <- DALY_to_GHT(data_frame$dalys_per_100k[max(tmp_locs)+0:Weeks_Out_To_Model])
  
  
  
  if (Data_Lag == 0){
    tmp_future <- c(0, rep(1, Weeks_Out_To_Model))
  } else {
    tmp_future <- c(0, rep(0, Data_Lag), rep(1, Weeks_Out_To_Model - Data_Lag))
  }
  
  out_df <- data.frame(week = data_frame$start_date[max(tmp_locs)]+7*(0:Weeks_Out_To_Model),
                       mod_name = rep(model_name, Weeks_Out_To_Model + 1),
                       future = tmp_future,
                       out_type = rep("continuous", Weeks_Out_To_Model + 1),
                       obs = data_frame$dalys_per_100k[max(tmp_locs)+0:Weeks_Out_To_Model],
                       pred_df,
                       GHT_obs = GHT_obs,
                       GHT_pred_df)
  
  
  return(out_df)
}
