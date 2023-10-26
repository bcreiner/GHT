suite_of_models <- unlist(lapply(strsplit(list.files(glue("{repo_dir}/code/suite_of_models")), "[.]"), function(x)x[[1]]))
suite_of_models <- c("constant", setdiff(suite_of_models, "constant"))

suite_of_models <- setdiff(suite_of_models, "tbats")
suite_of_models <- setdiff(suite_of_models, "ARIMA")

sapply(glue("{repo_dir}/code/suite_of_models/{suite_of_models}.r"), source)
