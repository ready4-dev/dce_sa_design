## 1. If you have not got the following packages installed, uncomment and run the required install scripts below:
# install.packages("idefix")
# install.packages("magrittr")
# install.packages("stringr")
# install.packages("shiny")
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()
## 2. Load magrittr so that we can pipe functions
library(magrittr)
## 3. Create functions that we will use later
make_block_choice_tbs_ls <- function(block_ind,
                                     choices_tb){
  purrr::map(block_ind,
             ~ dplyr::filter(choices_tb, startsWith(Choice, paste0("set",.x,"."))))
}
make_choice_card <- function(choice_card_sng_tb){
  formatted_tb <- t(choice_card_sng_tb) %>% 
    tibble::as_tibble(rownames = "Attribute") %>% 
    dplyr::filter(Attribute != "Choice") %>%
    dplyr::rename(`Social Anxiety App 1` = V1,
                  `Social Anxiety App 2` = V2)  
  row_names <- formatted_tb %>% dplyr::pull(Attribute)
  formatted_tb <- formatted_tb %>% dplyr::select(-Attribute)
  formatted_tb <- formatted_tb %>%
    as.data.frame() 
  rownames(formatted_tb) <- row_names
  
  formatted_tb %>%
    knitr::kable(escape = F) %>%  
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "left") %>%
    kableExtra::column_spec(1,bold = T, border_right = T) %>%
    kableExtra::column_spec(2:3, width = "40em", color = "black", border_right = T)
}
make_one_block_choice_cards_ls <- function(block_choice_tbs_ls){
  purrr::map(1:length(block_choice_tbs_ls),
             ~ make_choice_card(block_choice_tbs_ls %>% purrr::pluck(.x)))
}
save_one_block_choice_cards <- function(block_choice_cards_ls,
                                        save_path_stub){
  purrr::walk2(block_choice_cards_ls,
               1:length(block_choice_cards_ls),
               ~ kableExtra::as_image(.x,
                                      file = paste0(save_path_stub,
                                                    "/choice_",
                                                    .y,
                                                    ".png")))
}
## 4. Define the features that are specific to our survey.
## 4.1 Survey attributes and levels
attribute_levels <- list(Outcomes = c("Provides knowledge and skills to  manage future situations",
                                      "Addresses current symptoms",
                                      "Addresses current symptoms and provides knowledge and skills  to manage future situations"),
                         Information_sharing = c("No information is shared with your treating clinician", 
                                                 "Information is shared with your treating clinician in accordance with app policy", 
                                                 "Information is shared with your treating clinician based on settings you control"),
                         Social = c("No discussions with other app users",
                                    "Unmoderated discussions with other app users",
                                    "Discussions with other app users moderated by trained peers", 
                                    "Discussions with other app users moderated by mental health clinicians", 
                                    "Discussions with other app users moderated by both trained peers and mental health clinicians"),
                         Endorsers = c("App has no endorsers",
                                       "App is endorsed by respected non experts",
                                       "App is endorsed by youth mental health experts"), 
                         Cost = c(0,
                                  5, 
                                  15,
                                  30, 
                                  60))
## 4.2 Number of levels for each attribute
at_lvls <- purrr::map_int(attribute_levels,~length(.x)) %>% unname()
## 4.3 Specifiy coefficient type for each attribute (C - Continuous, D - Dummy)
c_type <- c("D","D","D","D","C")
## 4.4 Identify the levels for each continous attribute
con_lvls <- purrr::keep(attribute_levels, is.numeric) %>% unname()
## 4.5 Create a design matrix of the full factorial of all attribute / level combinations
candidate_des_mat <- idefix::Profiles(lvls = at_lvls, 
                                      coding = c_type, 
                                      c.lvls = con_lvls)
## 5. Specify coefficient priors
## Note: Currently this is based on subjective interpretation of focus group discussion.
## These priors will be updated following pilot survey to generate a new efficient design.
## 5.1 Prior parameter vector
mu <- c(-0.15, # opt out constant
        0.5,1, # Outcomes
        0.2,0.4, # Information sharing
        0.1,0.2,0.3,0.4, # Social
        0.1,0.2,# Endorsers
        -0.05) # Cost
## 5.2 Prior variance matrix
v <- diag(length(mu))
## 6. Create matrices of parameter value draws
## 6.1 Set seed
set.seed(1987)
## 6.2 Create matrix comprised of ten draws for each parameter
pd <- MASS::mvrnorm(n = 10, mu = mu, Sigma = v)
## 6.3 Split matrix into two: one for the alternative specific constant, the other for the coefficients
p.d <- list(matrix(pd[,1], ncol = 1), pd[,2:12])
## 7 Specifiy additional features about our survey
## 7.1 Number of choice sets
## Note: As we are aiming for two blocks of 15, the total number of choice sets is 30.
n_sets <- 30
## 7.2 Numer of alternatives (including choosing no social anxiety app opt-out)
n_alts <- 3
## 7.3 Index number of opt-out choice
no_choice_idx <- 3
## 7.4 Confirm that we are using an opt-out option
no_choice_lgl <- TRUE
## 7.5 Apply the alternative specific constant to the opt-out option
alt_cte <- c(0,0,1)
## 8. Create efficient design
## Note this step can take a long time (about an hour)
no_app_optout_ls <- idefix::Modfed(cand.set = candidate_des_mat, 
                                   n.sets = n_sets, 
                                   n.alts = n_alts, 
                                   no.choice = no_choice_lgl,
                                   alt.cte = alt_cte, 
                                   parallel = FALSE, 
                                   par.draws = p.d)
## 9. Create directories for saving output
dir.create("data")
dir.create("data/block_1")
dir.create("data/block_2")
## 10. Save efficient design 
saveRDS(no_app_optout_ls,"data/no_app_optout_ls.rds")
## 11. Format level names so that cost items have additional explanatory text.
lvl_names <- attribute_levels %>%
  purrr::discard(is.numeric) %>% 
  append(list(paste("$",
                    as.character(attribute_levels[[5]]),
                    " per month")) %>%
           stats::setNames("Cost"))
## 12. Translate design matrix into survey choices
survey_ls <- idefix::Decode(des = no_app_optout_ls$design,
                            lvl.names = lvl_names,
                            coding = c_type,
                            c.lvls = con_lvls,
                            alt.cte = alt_cte,
                            n.alts = n_alts,
                            no.choice = no_choice_idx)
saveRDS(survey_ls,"data/survey_ls.rds")
## 13. Reformat table of survey choice sets, dropping opt-out rows
choices_tb <- tibble::as_tibble(survey_ls$design, rownames = "Choice") %>%
  dplyr::rename(Outcomes = V1,
                Information_sharing = V2,
                Social = V3,
                Endorsers = V4,
                Cost = V5) %>%
  dplyr::filter(!startsWith(Choice, "no"))
saveRDS(choices_tb,"data/choices_tb.rds")
## 14. Randomly sample from choice set table to create index numbers for two blocks
block_1_ind <- sample(1:30,15) %>% sort()
block_2_ind <- setdiff(1:30,block_1_ind)
## 15. Create list of choice sets for each block
blocks_choice_tbs_ls_ls <- purrr::map(list(block_1_ind,
                                           block_2_ind),
                                      ~ make_block_choice_tbs_ls(.x,choices_tb))
## 16. Create choice cards for each block
choice_cards_by_block_ls <- purrr::map(blocks_choice_tbs_ls_ls,
                                       ~ make_one_block_choice_cards_ls(.x))
## 17. Save choice cards
purrr::walk2(choice_cards_by_block_ls,
             paste0("data/block_",c(1:length(choice_cards_by_block_ls))),
             ~ save_one_block_choice_cards(.x,
                                           .y))
