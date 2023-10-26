## snaive
#
snaive_model <- function(data_frame, time, draws){
  model_name <- "snaive"
  
  tmp_locs <- which(data_frame$start_date <= (time - 7*Data_Lag + 7))
  
  dat_train = log(data_frame$dalys_per_100k[tmp_locs]+1e-8)

  dat_ts <- ts(dat_train)
  
  tmp_mod <- snaive(dat_train, h = Weeks_Out_To_Model, level = seq(51,99,by=1))
  tmp_out <- exp(cbind(data.frame(tmp_mod$lower), data.frame(tmp_mod$upper)))
  tmp_sample <- sample(1:length(tmp_out), draws, replace = TRUE)
  
  pred_df <- data.frame(rbind(rep(data_frame$dalys_per_100k[max(tmp_locs)], draws),
                   tmp_out[,tmp_sample]))

  
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