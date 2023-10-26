xlab <- seq(-540, 0, by = 90) + today

df1 <- fread(paste0('/mnt/team/rapidresponse/priv/global-health-threats/deliverables/',data_date,'/model-inputs/forecast_modeling_inputs.csv'))

dt <- copy(df1)
dt <- dt[measure=='cases']
dt[, log_dalys := log10(dalys_per_100k + 1)]
# Get unique location and cause names
unique_locations <- unique(dt$location_name)
unique_causes <- unique(dt$cause_name)

for (internal in c(TRUE, FALSE)){
  if (internal){
    tmp_text <- glue("{GHT_output_dir}/{today}_plots_internal.pdf")
  } else{
    tmp_text <- glue("{CSU_output_dir}/{today}_plots.pdf")    
  }
  
  pdf(file = tmp_text, width = 16, height = 12)
  for (id_num in 1:length(UIDs)){
    for (cause_num in 1:length(UCauses)){
      
      tmp_text_local <- glue("predictions_location_id_{UIDs[id_num]}_{UCauses[cause_num]}.csv")
      if (is.element(tmp_text_local, list.files(GHT_output_dir))){
        
        # Filter data for the current location and cause
        subset_df <- input_data[which(input_data$location_id == UIDs[id_num] & input_data$cause_name == UCauses[cause_num]),]
        
        
        
        location <- subset_df$location_name[1]
        cause <- UCauses[cause_num]
        # Check if the subset is not empty
        # if (!is_empty(subset_dt)) {
        if (dim(subset_df)[1]>0) {
          par(las=0, mar = c(7.1,12.1,3.1,8.1))
          plot(subset_df$end_date, subset_df$dalys_per_100k, type = 'l',
               ylim = c(0, max(subset_df$dalys_per_100k)),
               ann = FALSE, axes = FALSE, yaxs = "i")
          box()
          mtext(glue("{subset_df$cause_name[1]} in {subset_df$location_name[1]} (loc id: {subset_df$location_id [1]})"), 
                line = 1.5, 3,cex = 1)
          mtext(glue("Last week of data: {format(max(subset_df$start_date), '%b %d')} - {format(max(subset_df$end_date), '%b %d')}, {as.numeric(today - max(subset_df$end_date))} days in the past"), 
                3, cex = 0.9)
          par(las = 2)
          
          axis(1, pretty(subset_df$end_date), format(pretty(subset_df$end_date), "%b %d, %Y"), -1, 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          
          axis(2, pretty( c(0, max(subset_df$dalys_per_100k))))
          par(las = 0)
          mtext("DALYs per 100,000", 2,line = 3.1, cex = 1)

          # Create the log plots
          #
          #
          # All the data
          #
          #
          tmp_multiplier_vec <- subset_df$value / subset_df$dalys_per_100k
          tmp_multiplier_vec <- tmp_multiplier_vec[!is.na(tmp_multiplier_vec)]
          tmp_multiplier <- mean(tmp_multiplier_vec) 
          
          subset_recent_df <- subset_df
          x_start <- max(subset_recent_df$start_date)

          xlab_all <- seq(-90*((as.numeric(today - min(subset_recent_df$start_date))) %/% 90), 0, by = 90) + today
          
          par(las=0, mar = c(7.1,12.1,3.1,8.1))
          plot(subset_recent_df$end_date, rescale(subset_recent_df$dalys_per_100k), type = 'b', 
               pch = 19, cex = 0.75,
               ylim = c(0, rescale(max(GHT_bins))), xlim = range(xlab_all),
               ann = FALSE, axes = FALSE, yaxs = "i")
          # box()
          
          mtext(glue("{subset_df$cause_name[1]} in {subset_df$location_name[1]} (loc id: {subset_df$location_id [1]})"), 
                line = 1.5, 3,cex = 1)
          mtext(glue("Last week of data: {format(max(subset_df$start_date), '%b %d')} - {format(max(subset_df$end_date), '%b %d')}, {as.numeric(today - max(subset_df$end_date))} days in the past"), 
                3, cex = 0.9)
          par(las = 2)
          
          axis(1, xlab_all, format(xlab_all, "%b %d, %Y"), -1, 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          
          axis(2, rescale(head(GHT_bins,-1)), signif(tmp_multiplier * head(GHT_bins,-1),2), 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          for (i in 1:length(head(GHT_bins,-1))){
            abline(h = rescale(GHT_bins[i]), col = rgb(0,0,0,.1))
          }
          axis(2, rescale(head(GHT_bins,-1)), head(GHT_bins,-1), line = 6, 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          axis(4, 0.5 + 0 : 7, paste("GHT Level ", 8:1), cex.axis = 0.8, lwd = 0)
          par(las = 0)
          mtext("DALYs per 100,000", 2,line = 9.6, cex = 1)
          mtext("Cases", 2,line = 3.6, cex = 1)
          #
          #
          # Zoomed into the present
          #
          #
          tmp_keep <- which(subset_df$end_date > today - 540)
          subset_recent_df <- subset_df[tmp_keep,]
          x_start <- max(subset_recent_df$start_date)

          par(las=0, mar = c(7.1,12.1,3.1,8.1))
          plot(subset_recent_df$end_date, rescale(subset_recent_df$dalys_per_100k), type = 'b', 
               pch = 19, cex = 0.75,
               ylim = c(0, rescale(max(GHT_bins))), xlim = range(xlab),
               ann = FALSE, axes = FALSE, yaxs = "i")
          # box()
          
          mtext(glue("{subset_df$cause_name[1]} in {subset_df$location_name[1]} (loc id: {subset_df$location_id [1]})"), 
                line = 1.5, 3,cex = 1)
          mtext(glue("Last week of data: {format(max(subset_df$start_date), '%b %d')} - {format(max(subset_df$end_date), '%b %d')}, {as.numeric(today - max(subset_df$end_date))} days in the past"), 
                3, cex = 0.9)
          par(las = 2)
          
          axis(1, xlab, format(xlab, "%b %d, %Y"), -1, 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          
          axis(2, rescale(head(GHT_bins,-1)), signif(tmp_multiplier * head(GHT_bins,-1),2), 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          for (i in 1:length(head(GHT_bins,-1))){
            abline(h = rescale(GHT_bins[i]), col = rgb(0,0,0,.1))
          }
          axis(2, rescale(head(GHT_bins,-1)), head(GHT_bins,-1), line = 6, 
               cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
          axis(4, 0.5 + 0 : 7, paste("GHT Level ", 8:1), cex.axis = 0.8, lwd = 0)
          par(las = 0)
          mtext("DALYs per 100,000", 2,line = 9.6, cex = 1)
          mtext("Cases", 2,line = 3.6, cex = 1)
        }
        
        
        options(scipen=999)
        tmp_data <- read.csv(glue("{GHT_output_dir}/{tmp_text_local}"))[,-1]
        tmp_data$start_date <- as.Date(tmp_data$start_date)
        tmp_data$end_date <- as.Date(tmp_data$end_date)
        n_rows <- length(tmp_data$start_date)
        
        # if (cause_num == 1){
        #   tmp_data$dalys_per_100k <- tmp_data$dalys_per_100k * .2
        # }
        
        tmp <- tmp_data$value / tmp_data$dalys_per_100k
        tmp <- tmp[!is.na(tmp)]
        tmp <- tail(tmp, 50)
        DALY_to_cases <- mean(tmp) 
        
        keep <- which(tmp_data$end_date < today & tmp_data$end_date > today - 120)
        las_data <- tmp_data[keep,]
        
        par(las=0, mar = c(5.1,12.1,2.1,8.1), oma = c(0,0,3.1,0))
        x_start <- max(las_data$start_date)
        plot(las_data$start_date, rescale( las_data$dalys_per_100k), type = 'l', 
             ylim = c(0, rescale(max(GHT_bins))), xlim = c(today - 120,x_start + 4 * 20),
             ann = FALSE, axes = FALSE, yaxs = "i", xaxs = "i")
        mtext(glue("{tmp_data$cause_name[1]} in {tmp_data$location_name[1]}"), 3, outer = TRUE, cex = 1.25)
        box()
        axis(1, head(pretty(las_data$start_date), -1), head(format(pretty(las_data$start_date), "%b %d"), -1))
        axis(2, rescale(head(GHT_bins,-1)), signif(DALY_to_cases * head(GHT_bins,-1),2))
        for (i in 1:length(head(GHT_bins,-1))){
          abline(h = rescale(GHT_bins[i]), col = rgb(0,0,0,.1))
        }
        axis(2, rescale(head(GHT_bins,-1)), head(GHT_bins,-1), line = 6)
        mtext("DALYs per 100,000", 2,line = 6+2.6, cex = 1.2)
        mtext("Cases", 2,line = 2.6, cex = 1.2)
        par(las=1)
        for (i in 1:4){
          if (internal){
            axis(3,x_start + (i - 1) * 20 + 10, round(100*tmp_data$skill[n_rows - 4 + i], 2))
          }
          axis(1, x_start + (i-1) * 20 + 10, format(today + (i-1) * 7, "%b %d"))
          for (j in 1:GHT_level_num){
            if (i == 1) axis(4,  mean(rescale(GHT_bins[j:(j+1)])), glue("GHT Level {9-j}"))
            if (j == 1){
              val <- tmp_data[n_rows - 4 + i, glue("Prob_level_{9-j}")]          
              # val <- val + tmp_data[n_rows - 4 + i, glue("Prob_level_{8-j}")]
            } else if (j == GHT_level_num){
              val <- 0
            } else {
              val <- tmp_data[n_rows - 4 + i, glue("Prob_level_{9-j}")]
            }
            
            tmp_txt <- glue("{round(100*val, 2)}%")
            val <- min(1,(val + mincol) / (0.9 + mincol))
            tmp_col <- adjustcolor(COLS[j], alpha = val)
            
            if (j == 1){
              rect(x_start + (i-1) * 20, 0, x_start + i * 20, rescale(GHT_bins[j+1]), col = tmp_col)
              text(x_start + (i-1) * 20 + 10, rescale(GHT_bins[j+1]) * 0.5, tmp_txt)
            } else {
              rect(x_start + (i-1) * 20, rescale(GHT_bins[j]), x_start + i * 20, rescale(GHT_bins[j+1]), col = tmp_col)
              text(x_start + (i-1) * 20 + 10, mean(rescale(GHT_bins[j:(j+1)])), tmp_txt)
            }
          }
        }
        
        
        
      }
    }
  }
  dev.off()
}