rm(list = ls())

#
# Libraries and Helpers
library(readxl)
require(glue)
require(RColorBrewer)
require(mgcv)
require(WaveletComp, lib = "~/packages")
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



# invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
# '%nin%' = Negate(`%in%`)
# '%notlike%' <- Negate('%like%')

User <- Sys.getenv("USER")
repo_dir <- glue("/ihme/homes/{User}/repos/GHT")

data_date <- "2023_10_20"
today <- Sys.Date()

source(glue("{repo_dir}/code/0_paths.r"))
source(glue("{repo_dir}/code/1_functions.r"))
source(glue("{repo_dir}/code/2_constants.r"))
source(glue("{repo_dir}/code/3_load_suite_of_models.r"))

source(glue("{repo_dir}/code/4_load_data.r"))
source(glue("{repo_dir}/code/5_make_predictions.r"))

source(glue("{repo_dir}/code/6_make_plots.r"))