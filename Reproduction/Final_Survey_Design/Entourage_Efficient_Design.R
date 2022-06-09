## 1. If you have not got the following packages installed, uncomment and run the required install scripts below:
# install.packages("idefix")
# install.packages("magrittr")
# install.packages("stringr")
# install.packages("shiny")
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("magick")
# install.packages("webshot")
## 2. Load magrittr so that we can pipe functions
library(magrittr)
## 3. Read in functions that we will use later
source("functions.R")
## 4. Define the features that are specific to our survey.
source("Entourage_Survey_Features.R")
## 5. Create and save efficient design for pilot study
output_dir <- "pilot_survey"
## Note this step can take a long time (about an hour)
pilot_survey_ls <- export_eff_des(survey_features_ls,
                                   parallel = FALSE, 
                                   output_dir = output_dir)
## 6. Create and save pilot study choice cards
choice_cards_pilot_ls <- export_choice_cards(no_app_optout_ls = pilot_survey_ls,
                                             survey_features_ls = survey_features_ls,
                                             output_dir = output_dir)
# 7. Preview survey in Shiny App
preview_survey(no_app_optout_ls = pilot_survey_ls,
               survey_features_ls = survey_features_ls,
               pilot = T)
## The following steps were undertaken after pilot data was collected and analysed.
## 8. Create New Efficient Design With Updated Priors
source("Entourage_Pilot_Analysis.R")
output_dir <- "final_survey"
final_survey_ls <- export_eff_des(survey_features_ls,
                                   parallel = FALSE,
                                   pilot_analysis = pilot_analysis,
                                   start_des = list(pilot_survey_ls$design),
                                   output_dir = output_dir)
## 9. Create and save final study choice cards
choice_cards_final_ls <- export_choice_cards(no_app_optout_ls = final_survey_ls,
                                             survey_features_ls = survey_features_ls,
                                             output_dir = output_dir)
# 10. Run sample survey
preview_survey(no_app_optout_ls = final_survey_ls,
               survey_features_ls = survey_features_ls,
               pilot = F)
