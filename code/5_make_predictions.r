


for (id_num in 1:length(UIDs)){
  for (cause_num in 1:length(UCauses)){
    ###
    ###
    suite_of_models <- unlist(lapply(strsplit(list.files(glue("{repo_dir}/code/suite_of_models")), "[.]"), function(x)x[[1]]))
    suite_of_models <- c("constant", setdiff(suite_of_models, "constant"))
    
    suite_of_models <- setdiff(suite_of_models, "tbats")
    suite_of_models <- setdiff(suite_of_models, "ARIMA")
    if (UCauses[cause_num] == "Tuberculosis"){
      suite_of_models <- c("constant", "naive")
    }
    
    sapply(glue("{repo_dir}/code/suite_of_models/{suite_of_models}.r"), source)
    ###
    ###
    tmp_locs <- which(input_data$location_id == UIDs[id_num] & input_data$cause_name == UCauses[cause_num] & input_data$measure == "cases")
    if (length(tmp_locs)){
      tmp_data <- input_data[tmp_locs,]
      tmp_data <- tmp_data[order(tmp_data$start_date), ]
      last_non_zero <- max(which(tmp_data$value != 0))
      if (length(last_non_zero)){
        tmp_data <- tmp_data[1:last_non_zero,] 
        
        UStart <- unique(tmp_data$start_date)
        keep <- sapply(UStart, function(x) which(tmp_data$start_date == x)[1])
        tmp_data <- tmp_data[keep,]
        #
        Data_Lag <- floor(as.numeric(today - max(tmp_data$end_date))/7)
        Weeks_Out_To_Model <- Forecast_Weeks + Data_Lag
        #
        tst <- which(tmp_data$start_date > as.Date("2022-01-01"))
        tst_data <- tmp_data[tst,]
        if (max(as.numeric(diff(tmp_data$start_date))) > 7){
          message(glue("Modling {tmp_data$cause_name[1]} in {tmp_data$location_name[1]}"))
          message(glue("There are {length(tmp_data$dalys_per_100k)} weeks of data"))
          message(glue("The lag is {Data_Lag} week(s)"))
          message(glue("ERROR: max sequential data gap is {max(as.numeric(diff(tmp_data$start_date)))} days apart"))
        } else if (quantile(tst_data$DALYs, 0.75) > 5){
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
          
    
          #
          #
          #
          
    
          
          best_fit <- vector("list", Weeks_Out_To_Model)
          for (forecast_week in 1:Weeks_Out_To_Model){
            week_to_check <- forecast_week
            message(glue("Checking week {forecast_week - Data_Lag}"))
            if (individual_model_RPS[1, week_to_check] == 0){
              message("Constant model is perfect")
              best_fit[[week_to_check]]$par <- create_vals(c(1,rep(0, length(suite_of_models))))
            } else {
              tmp_Monte <- vector("list", 100)
              for (i in 1:100){
                tmp_init_guess <- runif(length(suite_of_models))
                tmp_init_guess <- tmp_init_guess / sum(tmp_init_guess)
                tmp_Monte[[i]] <- optim(create_vals(tmp_init_guess), optim_function, method = "BFGS")
              }
              
              neg_RPS <- unlist(lapply(tmp_Monte, function(x) x$value))
              best_fit[[week_to_check]] <- tmp_Monte[[which.min(neg_RPS)]]
            }
          }
          
          best_weights <- lapply(best_fit, function(x)create_weights(x$par))
          
          all_current_pred <- vector("list", length(suite_of_models))
          is_continuous <- rep(FALSE, length(suite_of_models))
          for (mod_num in 1:length(suite_of_models)){
            message(glue("Starting prediction for {suite_of_models[mod_num]}. Model {mod_num} out of {length(suite_of_models)}"))
            #
            tmp_model <- get(glue("{suite_of_models[mod_num]}_model"))
            #
            all_current_pred[[mod_num]] <- tmp_model(tmp_data, today, draws)
            if (all_current_pred[[mod_num]]$out_type[1] == "continuous") is_continuous[mod_num] <- TRUE
          }
          
          # Removing GOD AWFUL models
          for (mod_num in 1:length(suite_of_models)){
            tmp_pred <- all_current_pred[[mod_num]]
            tmp_pred <- tmp_pred[,c("week", names(tmp_pred)[which(names(tmp_pred) %like% "pred_draw")], 
                                    names(tmp_pred)[which(names(tmp_pred) %like% "GHT_draw")])]

            tmp_vec <- as.vector(as.matrix(tmp_pred[, which(names(tmp_pred) %like% "GHT_draw")]))
              
            if (diff(range(tmp_vec)) > 5){
              for (forecast_week in 1:Weeks_Out_To_Model){
                best_weights[[forecast_week]][mod_num] <- 0
              }
            }
          }
          #
          best_weights <- lapply(best_weights, function(x){
            x <- x/sum(x)
          })
          
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
          
          
          
          for (forecast_week in 1:Weeks_Out_To_Model){
            tmp_pred <- all_current_pred[[1]]
            tmp_pred <- tmp_pred[1 + forecast_week,c("week", names(tmp_pred)[which(names(tmp_pred) %like% "pred_draw")], 
                                    names(tmp_pred)[which(names(tmp_pred) %like% "GHT_draw")])]
            #
            for (mod_num in 2:length(suite_of_models)){
              tmp_mat <- all_current_pred[[mod_num]]
              tmp_mat <- tmp_mat[1 + forecast_week,c("week", names(tmp_mat)[which(names(tmp_mat) %like% "pred_draw")], 
                                      names(tmp_mat)[which(names(tmp_mat) %like% "GHT_draw")])]
              tmp_pred <- rbind(tmp_pred, tmp_mat)
            }
            tmp_weights <- replicate(100, best_weights[[forecast_week]])
            tmp_set_to_zero <- which(best_weights[[forecast_week]] == 0)
            if (length(tmp_set_to_zero)){
              tmp_pred[tmp_set_to_zero, GHT_draw_rows] <- 0
              tmp_pred[tmp_set_to_zero, pred_draw_rows] <- 0
            }
            final_pred[1 + forecast_week, GHT_draw_rows] <- colSums(tmp_pred[,GHT_draw_rows] * tmp_weights)
            final_pred[1 + forecast_week, pred_draw_rows] <- colSums(tmp_pred[,pred_draw_rows] * tmp_weights)
          }
          
          # for (mod_num in 1:length(suite_of_models)){
          #   tmp_pred <- all_current_pred[[mod_num]]
          #   tmp_pred <- tmp_pred[,c("week", names(tmp_pred)[which(names(tmp_pred) %like% "pred_draw")], 
          #                           names(tmp_pred)[which(names(tmp_pred) %like% "GHT_draw")])]
          #   tmp_pred$week <- seq(tmp_pred$week[1], by = "1 week", length = length(tmp_pred$week))
          #   for (forecast_week in 1:Weeks_Out_To_Model){
          #     final_pred[1 + forecast_week, GHT_draw_rows] <- final_pred[1 + forecast_week, GHT_draw_rows] + best_weights[[forecast_week]][mod_num] * tmp_pred[1 + forecast_week, GHT_draw_rows] 
          #     final_pred[1 + forecast_week, pred_draw_rows] <- final_pred[1 + forecast_week, pred_draw_rows] + best_continuous_weights[[forecast_week]][mod_num] * tmp_pred[1 + forecast_week, pred_draw_rows] 
          #   }
          # }
          
          final_pred[,GHT_draw_rows] <- round(final_pred[,GHT_draw_rows])
          
          final_data <- combine_past_and_future(tmp_data, final_pred)
          final_data$skill <- NA
          final_data$skill[length(final_data$skill)+(-(length(unlist(lapply(best_fit, function(x)x$value)))-1):0)] <- -unlist(lapply(best_fit, function(x)x$value))
          tmp_text <- glue("{GHT_output_dir}/predictions_location_id_{UIDs[id_num]}_{UCauses[cause_num]}.csv")
          write.csv(final_data, tmp_text)
          tmp_text <- glue("{CSU_output_dir}/predictions_location_id_{UIDs[id_num]}_{UCauses[cause_num]}.csv")
          write.csv(final_data, tmp_text)
          save(all_current_pred, best_weights, final_pred, file = glue("{GHT_output_dir}/intermediate_quantities_location_id_{UIDs[id_num]}_{UCauses[cause_num]}.RData"))
        }
      }
    }
  }
}


