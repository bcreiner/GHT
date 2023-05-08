GHT_input_dir <- glue("/mnt/team/rapidresponse/priv/global-health-threats/deliverables/{data_date}/model-inputs")
GHT_output_dir <- glue("/mnt/team/rapidresponse/priv/global-health-threats/deliverables/{data_date}/model-outputs")


PROJECT_dir <- glue("/ihme/scratch/users/{User}/GHT")
suppressWarnings(dir.create(PROJECT_dir, recursive = TRUE))
suppressWarnings(dir.create(glue("{PROJECT_dir}/processed_data")))
suppressWarnings(dir.create(glue("{PROJECT_dir}/manuscript")))
suppressWarnings(dir.create(glue("{PROJECT_dir}/figures")))
