library(ready4)
params_ls = list(X = list(paths_ls = list(input_data_dir_1L_chr = "ENTER_PATH_HERE/Pilot_Survey",
                                          output_data_dir_1L_chr = "ENTER_PATH_HERE/Outputs",
                                          pilot_ds_fl_nm_1L_chr = "ENTER_FILE_NAME_HERE.csv",
                                          Models = "ENTER_PATH_HERE/Outputs/mychoice/Private/Models",
                                          Records = "ENTER_PATH_HERE/Outputs/mychoice/Private/Records",
                                          Replication = "ENTER_PATH_HERE/Outputs/mychoice/Public/Replication",
                                          Results = "ENTER_PATH_HERE/Confidential_Data/Outputs/mychoice/Public/Results"),
                          reproduce_1L_lgl = T),
                 eval_1L_lgl = T)
params_ls$eval_1L_lgl <- F
rmarkdown::render("Parent_PDF/CSDP.Rmd",
                  output_format = NULL,
                  params = params_ls,
                  output_file = "CSDP.pdf",
                  output_dir = "PDFs")
rmarkdown::render("Parent_PDF/Pilot_Design_Replication.Rmd",
                  output_format = NULL,
                  params = params_ls,
                  output_file = "Pilot_Design.pdf",
                  output_dir = "PDFs")
rmarkdown::render("Parent_PDF/Composite_2_3.Rmd",
                  output_format = NULL,
                  params = params_ls,
                  output_file = "Final_Design.pdf",
                  output_dir = "PDFs")
