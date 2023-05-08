

GHT_bins <- c(0, 10, 100, 500, 2000, 6000, 12000, 100000, 1000000)
GHT_level_num <- length(GHT_bins) - 1

test_fraction <- 0.8
draws <- 100
Forecast_Weeks <- 4

UIDs <- c(796, # San Mateo County
          4849, # Delhi
          4624, # Greater London
          69) # Singapore

UCauses <- c("COVID-19",
             "dengue",
             "measles")