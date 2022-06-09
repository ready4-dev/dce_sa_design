path_to_pilot_dir_1L_chr <- "WRITE PATH HERE"
choices_tb <- readRDS(paste0(path_to_pilot_dir_1L_chr,"/choices_tb.rds"))
no_app_optout_ls <- readRDS(paste0(path_to_pilot_dir_1L_chr,"/no_app_optout_ls.rds"))
survey_ls <- readRDS(paste0(path_to_pilot_dir_1L_chr,"/survey_ls.rds"))
mu <- c(-0.15, # opt out constant
        0.5,1, # Outcomes
        0.2,0.4, # Information sharing
        0.1,0.2,0.3,0.4, # Social
        0.1,0.2,# Endorsers
        -0.05) # Cost
v <- diag(length(mu))
X <- Ready4useRepos(dv_nm_1L_chr = "springtolife", # Replace with values for a dataverse & dataset for which
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/VGPIPS", #  you have write permissions.
                    dv_server_1L_chr = "dataverse.harvard.edu")
Y <- share(X,
           obj_to_share_xx = list(choices_tb = choices_tb,
                                  mu = mu,
                                  pilot_design_ls = no_app_optout_ls,
                                  survey_ls = survey_ls,
                                  v = v),
           fl_nm_1L_chr = "AAA_pilot_design_ls",
           description_1L_chr = "List object to help reproduce pilot survey (output of dce_sa_design reproduction program)")