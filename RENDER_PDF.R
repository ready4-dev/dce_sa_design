rmarkdown::render("Parent_PDF/Pilot_Design_Reproduction.RMD",
                  output_format = NULL,
                  params = list(eval_1L_lgl = F), # Do not set to True
                  output_file = "Pilot_Design_Reproduction.pdf",
                  output_dir = "PDFs")
rmarkdown::render("Parent_PDF/Pilot_Design_Replication.RMD",
                  output_format = NULL,
                  params = list(eval_1L_lgl = F), # Do not set to True
                  output_file = "Pilot_Design_Replication.pdf",
                  output_dir = "PDFs")
rmarkdown::render("Parent_PDF/Pilot_Analysis_Replication.RMD",
                  output_format = NULL,
                  params = list(eval_1L_lgl = F), # Do not set to True
                  output_file = "Pilot_Analysis_Replication.pdf",
                  output_dir = "PDFs")
rmarkdown::render("Parent_PDF/Final_Design_Replication.RMD",
                  output_format = NULL,
                  params = list(eval_1L_lgl = F), # Do not set to True
                  output_file = "Final_Design_Replication.pdf",
                  output_dir = "PDFs")