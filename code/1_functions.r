DALY_to_GHT <- function(x) return(9 - findInterval(x, GHT_bins, all.inside = TRUE))




##

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
  weights[is.na(weights)] <- 0
  return(weights)
}

create_vals <- function(weights){
  vals <- rep(0, length(suite_of_models))
  vals[1] <- logit(weights[1])
  if (length(weights) > 2){
    for (mod_num in 2:(length(weights) - 1)){
      vals[mod_num] <- logit(weights[mod_num] / (1 - sum(weights[1:(mod_num-1)])))
    }
  }
  return(vals)
}


## RPS
#
pred_prob_calculator <- function(pred_vec){
  out <- sapply(1:GHT_level_num, function(x){
    length(which(pred_vec <= x))/length(pred_vec)
  })
  return(out)
}
#
RPS_calculator <- function(obs_pred_vec){
  obs <- obs_pred_vec[1]
  pred_prob_vec <- pred_prob_calculator(obs_pred_vec[-1])
  tmp_ej <- rep(0,GHT_level_num)
  tmp_ej[obs] <- 1
  tmp_ej <- cumsum(tmp_ej)
  return(sum((pred_prob_vec - tmp_ej)^2) / (GHT_level_num - 1))
}
#
RPS_aggregator <- function(all_pred_list){
  num_models <- length(all_pred_list)
  #
  RPS_vals <- matrix(NA, num_models, Weeks_Out_To_Model)
  #
  for (mod_num in 1:num_models){
    tmp_list <- all_pred_list[[mod_num]]
    tmp_RPS_mat <- matrix(NA, length(tmp_list), Weeks_Out_To_Model)
    for (pred_week_num in 1:length(tmp_list)){
      tmp_mat <- tmp_list[[pred_week_num]][-1,]
      for (fore_week_num in 1:Weeks_Out_To_Model){
        tmp_obs_pred_vec <- tmp_mat[fore_week_num, 
                                    c("GHT_obs", names(tmp_mat)[which(names(tmp_mat) %like% "GHT_draw")])]
        tmp_RPS_mat[pred_week_num, fore_week_num] <- RPS_calculator(unlist(tmp_obs_pred_vec))
      }
      RPS_vals[mod_num, ] <- apply(tmp_RPS_mat, 2, mean)
    }
  }
  return(RPS_vals)
}

generate_pred_array <- function(all_pred_list){
  val_array <- array(NA, dim = c(length(all_pred_list), length(all_pred_list[[1]]), Weeks_Out_To_Model, draws + 1))
  for (mod_num in 1:length(all_pred_list)){
    tmp_list <- all_pred_list[[mod_num]]
    for (pred_week_num in 1:length(all_pred_list[[1]])){
      tmp_mat <- tmp_list[[pred_week_num]][-1,]
      val_array[mod_num, pred_week_num,, ] <- unname(as.matrix(cbind(tmp_mat$GHT_obs, 
                                                                     tmp_mat[, which(names(tmp_mat) %like% "GHT_draw")])))
    }
  }
  return(val_array)
}

ensemble_RPS_by_week <- function(weights, week_num, all_pred_array, constant_rps){
  ensemble_pred <- weights[1] * all_pred_array[1,,week_num,]
  for (mod_num in 2:dim(all_pred_array)[1]){
    ensemble_pred <- ensemble_pred + weights[mod_num] * all_pred_array[mod_num,,week_num,]
  }
  ensemble_pred <- round(ensemble_pred)
  ensemble_RPS <- mean(apply(ensemble_pred,1,RPS_calculator))
  
  val <- 1 - ensemble_RPS / constant_rps
  
  return(val)
}


####
####
####

combine_past_and_future <- function(obs_df, pred_df){
  toss <- unlist(sapply(c("Start", "End", "DALYs"), function(x){
    which(names(obs_df) == x)
  }))
  if (length(toss)){
    obs_df <- obs_df[,-toss]
  }
  
  final_data <- obs_df
  final_data$observed <- 1
  pred_levels <- data.frame(matrix(NA, length(final_data[,1]), GHT_level_num))
  names(pred_levels) <- paste("Prob_level", 1:8, sep = "_")
  final_data <- cbind(final_data, pred_levels)
  
  new_row_nums <- rep(length(final_data[,1]), Weeks_Out_To_Model)
  new_rows <- final_data[new_row_nums,]
  new_rows$ght <- NA
  new_rows$observed <- 0

  for (pred_week_num in 1:Weeks_Out_To_Model){
    tmp_row <- pred_df[pred_week_num + 1,]
    new_rows$start_date[pred_week_num] <- new_rows$start_date[pred_week_num] + as.numeric(pred_week_num * 7)
    new_rows$end_date[pred_week_num] <- new_rows$end_date[pred_week_num] + pred_week_num * 7
    new_rows$epi_week[pred_week_num] <- new_rows$epi_week[pred_week_num] + pred_week_num
    tmp_pred_vec <- as.vector(unlist(tmp_row[which(names(tmp_row) %like% "pred_draw")]))
    tmp_GHT_vec <- as.vector(unlist(tmp_row[which(names(tmp_row) %like% "GHT_draw")]))
    new_rows$dalys_per_100k[pred_week_num] <- median(tmp_pred_vec)
    new_rows[pred_week_num, which(names(new_rows) %like% "Prob")] <- sapply(1:GHT_level_num, 
                                                                            function(x){
                                                                              length(which(tmp_GHT_vec == x)) / draws
                                                                              })
  }
  #
  final_data <- rbind(final_data, new_rows)
  #
  return(final_data)
}

####
####
####
# 
# make_plot()
# YMAX <- max(unlist(lapply(all_past_pred, function(x)lapply(x, function(y)max(y[which(names(y) %like% "pred_draw")])))))
# par(mfrow = c(2,1))
# plot(data$Start, data$DALYs, type = 'l', ylim = c(0, YMAX), xlim = range(test_dates))
# for (i in 1:length(suite_of_models)){
#   for (j in 1:length(test_dates)){
#     tmp_mat <- all_past_pred[[i]][[j]]
#     for (k in 1:draws){
#       if (k < 11){
#         tmp_y <- tmp_mat[,glue("pred_draw_0{k-1}")]
#       } else {
#         tmp_y <- tmp_mat[,glue("pred_draw_{k-1}")] 
#       }
#       lines(tmp_mat$week, tmp_y, col = i+1)
#     }
#     tmp_y <- apply(tmp_mat[,which(names(tmp_mat) %like% "pred_draw")], 1, mean)
#     lines(tmp_mat$week, tmp_y, col = i+1, lwd = 2)
#   }
# }
# lines(data$Start, data$DALYs, lwd = 3)
# #
# plot(data$Start, data$GHT, type = 'l', xlim = range(test_dates), ylim = c(0,length(GHT_bins) - 1))
# for (i in 1:length(suite_of_models)){
#   for (j in 1:length(test_dates)){
#     tmp_mat <- all_past_pred[[i]][[j]]
#     for (k in 1:draws){
#       if (k < 11){
#         tmp_y <- tmp_mat[,glue("GHT_draw_0{k-1}")]
#       } else {
#         tmp_y <- tmp_mat[,glue("GHT_draw_{k-1}")] 
#       }
#       lines(tmp_mat$week, tmp_y, col = i+1)
#     }
#     tmp_y <- apply(tmp_mat[,which(names(tmp_mat) %like% "GHT_draw")], 1, mean)
#     lines(tmp_mat$week, tmp_y, col = i+1, lwd = 2)
#   }
# }
# lines(data$Start, data$GHT, lwd = 3)


rescale <- function(vec){
  vals <- rep(NA, length(vec))
  for (i in 1:length(vec)){
    x <- vec[i]
    level <- findInterval(x, GHT_bins, all.inside = TRUE)
    if (level == 1){
      vals[i] <- x/10#max(0, log10(x+1))
    } else {
      log10_l <- log10(GHT_bins[level])
      log10_x <- log10(x)
      log10_u <- log10(GHT_bins[level + 1])
      vals[i] <- level + (log10_x - log10_l) / (log10_u - log10_l) - 1
    }
  }
  return(vals)
}


colscale <- function(x){
  x <- (x + .1) / 1.1
  x <- x / (1.1)
}
