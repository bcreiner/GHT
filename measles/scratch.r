rm(list = ls())

#
# Libraries and Helpers
library(readxl)
require(glue)
require(sf)
require(RColorBrewer)
require(mgcv)
require(WaveletComp, lib = "~/packages")
# install.packages("paletteer", lib = "~/packages")
require(paletteer, lib = "~/packages")
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

User <- Sys.getenv("USER")
repo_dir <- glue("/ihme/homes/{User}/repos/GHT")

data_date <- "2023_07_24"
today <- as.Date("2023-07-27")


source(glue("{repo_dir}/code/0_paths.r"))
source(glue("{repo_dir}/code/1_functions.r"))
source(glue("{repo_dir}/code/4a_load_data.r"))

map_A0_data <- read_sf('/snfs1/WORK/11_geospatial/admin_shapefiles/2023_02_08/lbd_standard_admin_0_simplified.shp')
toss <- which(map_A0_data$ADM0_NAME == "Antarctica")
map_A0_data <- map_A0_data[-toss,]
map_A0 <- st_geometry(map_A0_data)
map_disp_data <- read_sf('/snfs1/WORK/11_geospatial/admin_shapefiles/2023_02_08/lbd_disputed_mask_simplified.shp')
map_disp <- st_geometry(map_disp_data)


measles_data <- input_data[which(input_data$cause_name == "Measles"),]

keep <- which(as.numeric(measles_data$end_date - as.numeric(measles_data$start_date)) > 10 & is.na(measles_data$country_id))
# keep <- which(as.numeric(measles_data$end_date - as.numeric(measles_data$start_date)) > 10 & which(measles_data$location_id == measles_data$country_id))

df <- measles_data[keep,]
df <- df[order(df$start_date),]
UIDs <- unique(df$location_id)

df$cum_tot <- 0
df$cum_tot_per <- 0
for (a0_num in 1:length(UIDs)){
  tmp_a0_locs <- which(df$location_id == UIDs[a0_num])
  tmp_mat <- df[tmp_a0_locs,]
  for (m_num in 1:length(tmp_mat[,1])){
    tmp_date <- tmp_mat$start_date[m_num]
    tmp_locs <- which(tmp_mat$start_date > tmp_date - 360 & tmp_mat$start_date <= tmp_date)
    df$cum_tot[tmp_a0_locs[m_num]] <- sum(tmp_mat$value[tmp_locs], na.rm = T)
    df$cum_tot_per[tmp_a0_locs[m_num]] <- 1000000 * sum(tmp_mat$value[tmp_locs], na.rm = T) / tmp_mat$population[m_num]
  }
}



df$over_20 <- ifelse(df$cum_tot_per > 20, 1, 0)
df$loc_af <- as.factor(df$location_name)
full_df <- df
df <- df[which(df$start_date < as.Date("2023-01-01") & df$start_date >= as.Date("2012-01-01")),]

ULocName <- unique(df$location_name)
UYears <- unique(df$year)
Range_Options <- c(as.character(UYears), "Ever", "NumYears", "NumMonths")
sum_df <- data.frame(location_name = rep(ULocName, each = length(Range_Options)),
                     location_id = rep(UIDs, each = length(Range_Options)),
                     time_range = rep(Range_Options, length = length(ULocName) * length(Range_Options)),
                     value = 0, value_20 = 0)

for (l_num in 1:length(ULocName)){
  tmp_locs <- which(df$location_name == ULocName[l_num])
  tmp_response <- which(sum_df$location_name == ULocName[l_num])
  #
  tmp_mat <- df[tmp_locs,]
  sum_df$value[tmp_response[length(UYears) + 1]] <- ifelse(sum(tmp_mat$value, na.rm = TRUE), 1, 0)
  sum_df$value_20[tmp_response[length(UYears) + 1]] <- ifelse(sum(tmp_mat$over_20), 1, 0)
  tmp_count_20 <- 0
  tmp_count <- 0
  for (y_num in 1:length(UYears)){
    dub_tmp_locs <- which(tmp_mat$year == UYears[y_num])
    tmp_sum <- sum(tmp_mat$over_20[dub_tmp_locs], na.rm = TRUE)
    if (tmp_sum){
      tmp_count_20 <- tmp_count_20 + 1
      sum_df$value_20[tmp_response[y_num]] <- tmp_sum
      sum_df$value_20[tmp_response[length(UYears) + 3]] <- sum_df$value_20[tmp_response[length(UYears) + 3]] + tmp_sum
    } else {
      sum_df$value_20[tmp_response[y_num]] <- 0
    }
    tmp_sum <- sum((tmp_mat$value > 0)[dub_tmp_locs], na.rm = TRUE)
    if (tmp_sum){
      tmp_count <- tmp_count + 1
      sum_df$value[tmp_response[y_num]] <- tmp_sum
      sum_df$value[tmp_response[length(UYears) + 3]] <- sum_df$value[tmp_response[length(UYears) + 3]] + tmp_sum
    } else {
      sum_df$value[tmp_response[y_num]] <- 0
    }
  }
  sum_df$value_20[tmp_response[length(UYears) + 2]] <- tmp_count_20
  sum_df$value[tmp_response[length(UYears) + 2]] <- tmp_count
}

write.csv(df, "df.csv")
write.csv(sum_df, "sum_df.csv")
sum_df[1:14,]


x <- sum_df$value[which(sum_df$time_range == "NumYears")]
y <- sum_df$value_20[which(sum_df$time_range == "NumYears")]
plot(jitter(sum_df$value[which(sum_df$time_range == "NumYears")]), sum_df$value[which(sum_df$time_range == "NumMonths")])

# Ever
# Num Years
# General Frequency

sub <- df[which(df$start_date == as.Date("2022-11-01")),]
a0_map_locs <- sapply(sub$location_id, function(x){
  tmp <- which(map_A0_data$loc_id == x)
  ifelse(length(tmp), tmp, NA)
})

COLS <- c("grey80", 'dodgerblue')
col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  if (sub$cum_tot_per[i] > 20){
    col_locs[a0_map_locs[i]] <- 2
  }
}

plot(map_A0, col = COLS[col_locs])

COLS <- c("grey90", rev(brewer.pal(11,"Spectral")))

sub <- sum_df[which(sum_df$time_range == "NumYears"),]
a0_map_locs <- sapply(sub$location_id, function(x){
  tmp <- which(map_A0_data$loc_id == x)
  ifelse(length(tmp), tmp, NA)
})

col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  col_locs[a0_map_locs[i]] <- sub$value_20[i] + 1
}

plot(map_A0, col = COLS[col_locs])

### Can I ID model this?
BINS <- c(0, 1, 12*(1:11))
COLS <- c("grey90", rev(brewer.pal(11,"Spectral")))
sub <- sum_df[which(sum_df$time_range == "NumMonths"),]
a0_map_locs <- sapply(sub$location_id, function(x){
  tmp <- which(map_A0_data$loc_id == x)
  ifelse(length(tmp), tmp, NA)
})

col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  col_locs[a0_map_locs[i]] <- findInterval(sub$value_20[i], BINS, all.inside = TRUE)
}

pdf(file="Weeks.pdf", width = 16, height = 8)
layout(matrix(1:2,2,1), height = c(2,7))
par(mar=c(3.1,0,2.1,0), oma = c(0,0,4,0), xpd = TRUE)
X_0 <- table(col_locs)
X <- log(table(col_locs))
big_gap <- 2
small_gap <- 0.1
plot(1, type = 'n', ann = FALSE, axes = FALSE, ylim = c(0, max(X)), xlim = c(0 - big_gap, length(COLS) + big_gap))
i_num <- 1
rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
axis(1, i_num - 0.5, 0, tick = FALSE)
for (i_num in 2:length(X)){
  rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
  text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
  axis(1, i_num - 0.5, glue("{BINS[i_num]} - {BINS[i_num + 1]}"), tick = FALSE)
}
par(mar=rep(0,4))
col_locs[which(map_A0_data$ADM0_NAME == "Taiwan (Province of China)")] <- col_locs[which(map_A0_data$ADM0_NAME == "China")]
plot(map_A0, col = COLS[col_locs])
plot(map_disp, add = TRUE)
mtext("Weeks classified with LoDO from 2012 - 2022", outer = TRUE, 3, cex = 1.25)
dev.off()

### Can I ID model this?
BINS <- 0:12
COLS <- c("grey90", rev(brewer.pal(length(BINS) - 1,"Spectral")))
sub <- sum_df[which(sum_df$time_range == "NumYears"),]
a0_map_locs <- sapply(sub$location_id, function(x){
  tmp <- which(map_A0_data$loc_id == x)
  ifelse(length(tmp), tmp, NA)
})

col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  col_locs[a0_map_locs[i]] <- findInterval(sub$value_20[i], BINS, all.inside = TRUE)
}
# pdf(file="Years.pdf", width = 12, height = 6)
png(file="Years.png", width = 12, height = 6, units = "in", res = 1080)
layout(matrix(1:2,2,1), height = c(2,7))
par(mar=c(2.1,0,1.1,0), oma = c(0,0,2,0), xpd = TRUE)
X_0 <- table(col_locs)
X <- log(table(col_locs))
big_gap <- 2
small_gap <- 0.1
plot(1, type = 'n', ann = FALSE, axes = FALSE, ylim = c(0, max(X)), xlim = c(0 - big_gap, length(COLS) + big_gap))
i_num <- 1
rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
axis(1, i_num - 0.5, 0, tick = FALSE)
for (i_num in 2:length(X)){
  rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
  text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
  axis(1, i_num - 0.5, BINS[i_num], tick = FALSE)
}
par(mar=rep(0,4))
col_locs[which(map_A0_data$ADM0_NAME == "Taiwan (Province of China)")] <- col_locs[which(map_A0_data$ADM0_NAME == "China")]
plot(map_A0, col = COLS[col_locs])
plot(map_disp, add = TRUE)
mtext("Years with at least one LoDO classification from 2012 - 2022", outer = TRUE, 3, cex = 1.25)
dev.off()










### Can I ID model this?
BINS <- c(0, 1, 12*(1:11))
COLS <- c("grey90", rev(brewer.pal(11,"Spectral")))
sub <- sum_df[which(sum_df$time_range == "NumMonths"),]
a0_map_locs <- sapply(sub$location_id, function(x){
  tmp <- which(map_A0_data$loc_id == x)
  ifelse(length(tmp), tmp, NA)
})

col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  col_locs[a0_map_locs[i]] <- findInterval(sub$value[i], BINS, all.inside = TRUE)
}

layout(matrix(1:2,2,1), height = c(2,7))
par(mar=c(3.1,0,2.1,0), oma = c(0,0,4,0), xpd = TRUE)
X_0 <- table(col_locs)
X <- log(table(col_locs))
big_gap <- 4
small_gap <- 0.1
plot(1, type = 'n', ann = FALSE, axes = FALSE, ylim = c(0, max(X)), xlim = c(0 - big_gap, length(COLS) + big_gap))
i_num <- 1
rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
axis(1, i_num - 0.5, 0, tick = FALSE)
for (i_num in 2:length(X)){
  rect(i_num - 1 + small_gap, 0, i_num - small_gap, X[i_num], col = COLS[i_num])
  text(i_num - 0.5, X[i_num], glue("n: {X_0[i_num]}"), pos = 3)
  axis(1, i_num - 0.5, glue("{BINS[i_num]} - {BINS[i_num + 1]}"), tick = FALSE)
}
par(mar=rep(0,4))
col_locs[which(map_A0_data$ADM0_NAME == "Taiwan (Province of China)")] <- col_locs[which(map_A0_data$ADM0_NAME == "China")]
plot(map_A0, col = COLS[col_locs])
plot(map_disp, add = TRUE)
mtext("Weeks with 'some' reported measles", outer = TRUE, 3, cex = 1.25)









hm <- sum_df[which(sum_df$time_range == "NumMonths"),]
hist(hm$value / (length(UYears) * 12))
length(UYears) * 12
require(lme4)
require(mgcv)

mod1 <- glmer(over_20 ~ 1 + (1|loc_af), data = df, family = binomial)

out <- data.frame(ranef(mod1))

out <- data.frame(location_name = out$grp,
                  value = out$condval)

out <- out[order(out$value), ]

tmp_COLS <- brewer.pal(3,"Set1")
HM <- colorRampPalette(c(tmp_COLS[1], tmp_COLS[2]))( 12 )
plot(1:length(HM), pch = 19, cex = 4, col = HM)
COLS <- paletteer_c("RColorBrewer::Set1", 100)


COLS <- c("grey80", 'dodgerblue')
col_locs <- rep(1, length(map_A0))
for (i in 1:length(sub[,1])){
  if (sub$cum_tot_per[i] > 20){
    col_locs[a0_map_locs[i]] <- 2
  }
}

plot(map_A0, col = COLS[col_locs])

sub[which(sub$cum_tot > 20),c("location_name", "cum_tot", "cum_tot_per")]

tmp_df <- df[which(df$location_name == "Angola" & df$start_date >= as.Date("2021-12-01")),]



# 2347
