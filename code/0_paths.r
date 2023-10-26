GHT_input_dir <- glue("/mnt/team/rapidresponse/priv/global-health-threats/deliverables/{data_date}/model-inputs")
GHT_output_dir <- glue("/mnt/team/rapidresponse/priv/global-health-threats/deliverables/{data_date}/model-outputs")
suppressWarnings(dir.create(GHT_output_dir, recursive = TRUE))

IHME_COVID_dir <- "/mnt/team/covid_19/priv/modeling/seir-outputs/2022_12_15.05/reference/output_summaries"

CSU_output_dir <- glue("/snfs1/Project/CSU/meta_ght/outputs/{data_date}")
suppressWarnings(dir.create(CSU_output_dir, recursive = TRUE))

PROJECT_dir <- glue("/ihme/scratch/users/{User}/GHT")
suppressWarnings(dir.create(PROJECT_dir, recursive = TRUE))
suppressWarnings(dir.create(glue("{PROJECT_dir}/processed_data")))
suppressWarnings(dir.create(glue("{PROJECT_dir}/manuscript")))
suppressWarnings(dir.create(glue("{PROJECT_dir}/figures")))

