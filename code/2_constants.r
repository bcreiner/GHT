GHT_bins <- c(0, 10, 100, 500, 2000, 6000, 12000, 100000, 1000000)
GHT_level_num <- length(GHT_bins) - 1

test_fraction <- 0.8
draws <- 200
Forecast_Weeks <- 5

mincol <- 0.1

COLS <- rev(brewer.pal(8, "Spectral"))