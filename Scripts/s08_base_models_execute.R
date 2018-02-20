###################################################
# s08_baseModelExecute.R
# Author: 
# Created: 
#    
# Description: Generate and execute NONMEM runs for base model development
# Dependencies: s07_NMDatasets.Rmd
###################################################

# ------------------------------------------------------------------
#  Prepare environment
# ------------------------------------------------------------------
# load generated NM datasets
load(file.path("Scripts", "s07.RData"))

# ------------------------------------------------------------------
#  Load blueprint and templates for nonmem 
# ------------------------------------------------------------------
blueprint <- Blueprint$new("nonmem")
templates <- load_templates("nonmem")
models <- available_models("nonmem")

# ------------------------------------------------------------------
#  Define path to data relative the base model dir
# ------------------------------------------------------------------
derived_data_dir_rel_base <- paste0("../.", derived_data_dir)

# ------------------------------------------------------------------
#  Settings for plots
# ------------------------------------------------------------------
theme_set(theme_bw())
update_geom_defaults("point", list(shape = 1))

# ------------------------------------------------------------------
#  Units
# ------------------------------------------------------------------
# Drug concentrations are in in mg/L, doses in mg and time(=TAFD) in hours. 
# Hence, Ka in h-1, CL & Q in L/h, V1 & V2 in L, scaling=V
unit_dv <- "mg/L"
unit_time <- "h"
unit_KA <- "h-1"
unit_CL <- "L/h"
unit_V  <- "L"
# Used in comment convention ; Parameter name ; Unit ; Transformtion

# ------------------------------------------------------------------
#  Model development
# ------------------------------------------------------------------

# initiating templates with dataset
one_cmt <- blueprint %>% 
  use_template(templates$compartmental) %>%
  model_type(models$one_cmt_oral) %>% 
  # Add data
  with_data(nm_data) %>%
  from_path(file.path(derived_data_dir_rel_base, nm_data_filename)) 

two_cmt <- blueprint %>% 
  use_template(templates$compartmental) %>%
  model_type(models$two_cmt_oral) %>% 
  # Add data
  with_data(nm_data) %>%
  from_path(file.path(derived_data_dir_rel_base, nm_data_filename)) 

two_cmt_rate <- blueprint %>% 
  use_template(templates$compartmental) %>%
  model_type(models$two_cmt_oral) %>% 
  # Add data
  with_data(nm_data_comb) %>%
  from_path(file.path(derived_data_dir_rel_base, nm_data_comb_filename)) 

# ------------------------------------------------------------------
#  1. Number of compartments
# ------------------------------------------------------------------

# ---------------
#  Run001
# ---------------
model_no <- "001"

if(model_no=="001"){
  
  # properties
  description <- "1 cmt model, 1st order abs, BSV on CL"
  based_on <- NA
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  # build nonmem control file
  run001 <- one_cmt %>% 
    # add parameters
    parameters(
      KA = parameter(0.8, lower_bound=0,
                     comment = paste0("KA ;", unit_KA, ";")),
      CL = parameter(200, lower_bound = 0.01,
                     comment=paste0("CL ;", unit_CL, ";")),
      V = parameter(400, lower_bound = 0, 
                    comment=paste0("V ;",unit_V, ";"))) %>%
    hierarchies(CL = 0.04) %>% 
    residual_error(PROP = 0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run001$get_params())
  run001rend <- render(run001)
  
  run001rend <- cltfile_update(run001rend)
  run001rend <- add_comments(run001rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run001rend, file=file.path(base_model_dir, filename))
  # Discuss with Tarj: 
  # build in as an option in nmproject that warns you about over-writing the file? 
  
  # Command line update
  psn_execute <- execute_update(psn_execute_temp, filename, dir_name) 
  # note: nmproject does not work with clean=3
  
  # --- Add to run using nmproject
  mod1 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod1, quiet=F)
}


# ---------------
#  Run002
# ---------------
model_no <- "002"

if(model_no=="002"){ 
  
  # properties
  description <- "1 cmt model, 1st order abs, BSV on CL and V"  
  based_on <- "001"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run002 <- run001 %>% 
    hierarchies(CL = 0.1, V = 0.1) 
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run002$get_params())
  run002rend <- render(run002)
  
  run002rend <- cltfile_update(run002rend)
  run002rend <- add_comments(run002rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run002rend, file=file.path(base_model_dir, filename)) 
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod2 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod2, quiet=F)
}

# ---------------
#  Run003
# ---------------
model_no <- "003"

if(model_no=="003"){ 
  
  # properties
  description <- "1 cmt model, 1st order abs, BSV on CL and V in block"
  based_on <- "002"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run003 <- one_cmt %>% 
    # add parameters
    parameters(
      KA = parameter(0.8, lower_bound=0,
                     comment = paste0("KA ;", unit_KA, ";")),
      CL = parameter(200, lower_bound = 0.01,
                     comment=paste0("CL ;", unit_CL, ";")),
      V = parameter(400, lower_bound = 0, 
                    comment=paste0("V ;",unit_V, ";"))) %>%
    hierarchies(b1 = block(0.1, 0.05, 0.1, 
                           param_names = c("CL", "V"))) %>% 
    residual_error(PROP = 0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run003$get_params())
  run003rend <- render(run003)
  
  # update tables
  run003rend <- cltfile_update(run003rend)
  run003rend <- add_comments(run003rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run003rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod3 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod3, quiet=F)
}

# ---------------
#  Run003+M3
# ---------------
# Not implemented in blueprint yet
model_no <- "003m3"

if(model_no=="003m3"){
  
  # properties
  # description <- "run 003 with M3 for BLQ"
  # based_on <- NA  
  #
  # output and dir to run
  #   filename   <- paste0("run", model_no)
  #   dir_name    <- paste0("dir_run", model_no)
  #   
  #   # Select seed to use for NPDEs
  #   seed <- format(Sys.time(), "%y%m%d%H%M")
  #   
  #   run003m3 <- one_cmt %>% 
  #     # add parameters
  #     parameters(
  #       KA = parameter(0.8, lower_bound=0,
  #                      comment = paste0("KA ;", unit_KA, ";")),
  #       CL = parameter(200, lower_bound = 0.01,
  #                      comment=paste0("CL ;", unit_CL, ";")),
  #       V = parameter(400, lower_bound = 0, 
  #                     comment=paste0("V ;",unit_V, ";"))) %>%
  #     hierarchies(b1 = block(0.1, 0.05, 0.1, 
  #                            param_names = c("CL", "V"))) # %>% 
  # #     residual_error(PROP = 0.1)
  #   
  #   # tweak for adding tables and comments (temporary)
  #   parms <- names(run003m3$get_params())
  #   run003m3rend <- render(run003m3)
  #   
  #   # update tables
  #   run003m3rend <- cltfile_update(run003m3rend)
  #   run003m3rend <- add_comments(run003m3rend, ref_run = based_on,
  #                              description = description, author = analystName)
  #   
  #   # --- write file to cluster
  #   write(run003m3rend, file=file.path(base_model_dir, filename))
  #   
  #   # command line update
  #   psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  #   
  #   # --- Add to run using nmproject
  #   mod3m3 <- nm(cmd = psnExecute, 
  #              run_in = base_model_dir)
  #   run(mod3m3, quiet=F)
}

# ---------------
#  Run004
# ---------------
model_no <- "004"

if(model_no=="004"){
  
  # properties
  description <- "2 cmt model with 1st order abs, BSV on CL only"
  based_on <- "001"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  # build nonmem control file
  run004 <- two_cmt %>% 
    # add parameters
    parameters(
      KA = parameter(5, lower_bound=0,
                     comment = paste0("KA ;", unit_KA, ";")),
      CL = parameter(140, lower_bound = 0.01,
                     comment=paste0("CL ;", unit_CL, ";")),
      V2 = parameter(200, lower_bound = 0, 
                     comment=paste0("V2 ;",unit_V, ";")),
      Q = parameter(15, lower_bound = 0, 
                    comment=paste0("Q ;",unit_CL, ";")),
      V3 = parameter(250, lower_bound = 0, 
                     comment=paste0("V3 ;",unit_V, ";"))) %>%
    hierarchies(CL = 0.04) %>% 
    residual_error(PROP = 0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run004$get_params())
  run004rend <- render(run004)
  
  # update tables
  run004rend <- cltfile_update(run004rend)
  run004rend <- add_comments(run004rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run004rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod4 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod4, quiet=F)
}

# ---------------
#  Run005
# ---------------
model_no <- "005"

if(model_no=="005"){ 
  
  # properties
  description <- "2 cmt model with 1st order abs, BSV on CL and V"
  based_on <- "004" # and 002 (1 cmt)
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run005 <- run004 %>% 
    hierarchies(CL = 0.1, V = 0.1) 
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run005$get_params())
  run005rend <- render(run005)
  
  # update tables
  run005rend <- cltfile_update(run005rend)
  run005rend <- add_comments(run005rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run005rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod5 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod5, quiet=F)
}

# ---------------
#  Run006
# ---------------
model_no <- "006"

if(model_no=="006"){ 
  
  # properties
  description <- "2 cmt model with 1st order abs, BSV on CL and V in block"
  based_on <- "005" # and 003 (1 cmt)
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  # build nonmem control file
  run006 <- two_cmt %>% 
    # add parameters
    parameters(
      KA = parameter(5, lower_bound=0,
                     comment = paste0("KA ;", unit_KA, ";")),
      CL = parameter(140, lower_bound = 0.01,
                     comment=paste0("CL ;", unit_CL, ";")),
      V2 = parameter(200, lower_bound = 0, 
                     comment=paste0("V2 ;",unit_V, ";")),
      Q = parameter(15, lower_bound = 0, 
                    comment=paste0("Q ;",unit_CL, ";")),
      V3 = parameter(250, lower_bound = 0, 
                     comment=paste0("V3 ;",unit_V, ";"))) %>%
    hierarchies(b1 = block(0.1, 0.05, 0.1, 
                           param_names = c("CL", "V2"))) %>% 
    residual_error(PROP = 0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run006$get_params())
  run006rend <- render(run006)
  
  # update tables
  run006rend <- cltfile_update(run006rend)
  run006rend <- add_comments(run006rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run006rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod6 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod6, quiet=F)
}

# ---------------
#  Run006+M3
# ---------------
# Not implemented yet


# ------------------------------------------------------------------
#  2. Investigate different absorption models
# ------------------------------------------------------------------
## Notes: Continue with a 2-compartment model, and BSV on CL and V in block(2)

# ---------------
#  Run007
# ---------------
model_no <- "007"

if(model_no=="007"){
  
  # properties
  description <- "Run 006 with lag time"
  based_on <- "006"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run007 <- run006 %>% 
    parameters(ALAG1 = parameter(0.05, lower_bound=0,
                                 comment = paste0("ALAG;", unitTime, ";")))
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run007$get_params())
  run007rend <- render(run007)
  
  # update tables
  run007rend <- cltfile_update(run007rend)
  run007rend <- add_comments(run007rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run007rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod7 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod7, quiet=F)
} 

# ---------------
#  Run007+M3
# ---------------
# Not implemented yet


# ---------------
#  Run008
# ---------------
model_no <- "008"

if(model_no=="008"){ 
  
  # properties
  description <- "2 cmt with zero and first order abs, BSV on CL and V in block"
  based_on <- "006" 
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  # build nonmem control file
  run008 <- two_cmt %>% 
    # add parameters
    parameters(
      D1 = parameter(0.15, lower_bound=0,
                     comment = paste0("D1 ;", unitTime, ";")),
      KA = parameter(5, lower_bound=0,
                     comment = paste0("KA ;", unit_KA, ";")),
      CL = parameter(140, lower_bound = 0.01,
                     comment=paste0("CL ;", unit_CL, ";")),
      V2 = parameter(200, lower_bound = 0, 
                     comment=paste0("V2 ;",unit_V, ";")),
      Q = parameter(15, lower_bound = 0, 
                    comment=paste0("Q ;",unit_CL, ";")),
      V3 = parameter(250, lower_bound = 0, 
                     comment=paste0("V3 ;",unit_V, ";"))) %>%
    hierarchies(b1 = block(0.1, 0.05, 0.1, 
                           param_names = c("CL", "V2"))) %>% 
    residual_error(PROP = 0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run008$get_params())
  run008rend <- render(run008)
  
  # update tables
  run008rend <- cltfile_update(run008rend)
  run008rend <- add_comments(run008rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run008rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod8 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod8, quiet=F)
} 

# ---------------
#  Run008+M3
# ---------------


# ---------------
#  Run009
# ---------------
model_no <- "009"

if(model_no=="009"){ 
  
  # properties
  description <- "2 cmt with lag time, zero and first order abs, BSV on CL and V in block"
  based_on <- "008"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run009 <- run008 %>% 
    parameters(ALAG1 = parameter(0.10, lower_bound=0,
                                 comment = paste0("ALAG;", unitTime, ";")))
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run009$get_params())
  run009rend <- render(run009)
  
  # update tables
  run009rend <- cltfile_update(run009rend)
  run009rend <- add_comments(run009rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run009rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod9 <- nm(cmd = psnExecute, 
             run_in = base_model_dir)
  run(mod9, quiet=F)
} 

# ---------------
#  Run010
# ---------------
model_no <- "010"

if(model_no=="010"){ 
  
  # properties
  description <- "2 cmt with zero and first order abs, BSV on CL and V in block, BSV D1"
  based_on <- "008"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run010 <- run008 %>% 
    hierarchies(D1=0.1)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run010$get_params())
  run010rend <- render(run010)
  
  # update tables
  run010rend <- cltfile_update(run010rend)
  run010rend <- add_comments(run010rend, ref_run = based_on,
                             description = description, author = analystName)
  
  # --- write file to cluster
  write(run010rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod10 <- nm(cmd = psnExecute, 
              run_in = base_model_dir)
  run(mod10, quiet=F)
} 

# ------------------------------------------------------------------
#  3. Investigate different residual error models
# ------------------------------------------------------------------

# ---------------
#  Run011
# ---------------
model_no <- "011"

if(model_no=="011"){ 
  
  # properties
  description <- "2 cmt with zero and first order abs, BSV on CL and V in block and D1, prop and add RSV"
  based_on <- "010"
  
  # output and dir to run
  filename   <- paste0("run", model_no)
  dir_name    <- paste0("dir_run", model_no)
  
  # Select seed to use for NPDEs
  seed <- format(Sys.time(), "%y%m%d%H%M")
  
  run011 <- run010 %>% 
    residual_error(ADD = 0.5)
  
  # tweak for adding tables and comments (temporary)
  parms <- names(run010$get_params())
  run011rend <- render(run011)
  
  # update tables
  run011rend <- cltfile_update(run011rend)
  
  # --- write file to cluster
  write(run011rend, file=file.path(base_model_dir, filename))
  
  # command line update
  psnExecute <- execute_update(psnExecuteTemp, filename, dir_name)  
  
  # --- Add to run using nmproject
  mod11 <- nm(cmd = psnExecute, 
              run_in = base_model_dir)
  run(mod11, quiet=F)
} 

