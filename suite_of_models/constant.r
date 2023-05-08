## constant model
#
constant_model <- function(data_frame, time, draws){
  model_name <- "constant"
  
  tmp_locs <- which(data_frame$Start <= (time - 7*Data_Lag + 7))
  
  tmp_today <- data_frame$DALYs[max(tmp_locs)]
  tmp_starts <- sample(head(tmp_locs, -Weeks_Out_To_Model), draws, replace = TRUE)
  
  tmp_error <- sapply(tmp_starts, function(x){
    data_frame$DALYs[x+0:Weeks_Out_To_Model] / (data_frame$DALYs[x] + 1e-8)
  })
  
  pred_vals <- matrix(tmp_today, Weeks_Out_To_Model + 1, draws) * tmp_error
  pred_vals <- tmp_today + pred_vals - apply(pred_vals, 1, mean)
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