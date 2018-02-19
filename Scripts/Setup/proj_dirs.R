#directories at activity level
# do not remove the leading ".", it will cause problems with latex/knitr)

scripts_dir <- file.path(".", "Scripts")
derived_data_dir <- file.path(".", "DerivedData")
model_dir <- file.path(".", "Models")
report_dir <- file.path(".", "Report")
results_dir <- file.path(".", "Results")
sim_dir <- file.path(".", "Simulations")
source_data_dir <- file.path(".", "SourceData")

##sub-directories
#script_dir
setup_dir <- file.path(scripts_dir, "Setup")
functions_dir <- file.path(scripts_dir, "Functions")

#model_dir
base_model_dir <- file.path(model_dir, "BaseModel")
covariate_model_dir <- file.path(model_dir, "CovariateModel")

#result_dir
res_other_dir <- file.path(results_dir, "Other")
res_eda_dir <- file.path(results_dir, "ExploratoryDataAnalysis")
res_base_model_dir <- file.path(results_dir, "BaseModel")
res_cov_model_dir <- file.path(results_dir, "CovariateModel")

#report_dir
rep_setup_dir <- file.path(report_dir, "Setup")
rep_sections_dir <- file.path(report_dir, "Sections")
rep_appendicies_dir <- file.path(report_dir, "Appendices")
rep_images_dir <- file.path(report_dir, "Images")

##list_all_directories
all_dir <-
  list(
    scripts_dir = scripts_dir,
    derived_data_dir = derived_data_dir,
    model_dir = model_dir,
    report_dir = report_dir,
    results_dir = results_dir,
    sim_dir = sim_dir,
    source_data_dir = source_data_dir,
    setup_dir = setup_dir,
    functions_dir = functions_dir,
    base_model_dir = base_model_dir,
    covariate_model_dir = covariate_model_dir,
    res_other_dir = res_other_dir,
    res_eda_dir = res_eda_dir,
    res_base_model_dir = res_base_model_dir,
    res_cov_model_dir = res_cov_model_dir,
    rep_setup_dir = rep_setup_dir,
    rep_sections_dir = rep_sections_dir,
    rep_appendicies_dir = rep_appendicies_dir,
    rep_images_dir = rep_images_dir
  )


lapply(all_dir, xpmworkbench::mkdirp)

# return all directories as the value 
all_dir
