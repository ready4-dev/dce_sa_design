## Survey features
## 1. Define Attributes And Levels
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
## 2. Format level names so that cost items have additional explanatory text.
cost_attr_indx <- 5
lvl_names <- attribute_levels %>%
  purrr::discard(is.numeric) %>% 
  append(list(paste("$",
                    as.character(attribute_levels[[cost_attr_indx]]),
                    " per month")) %>%
           stats::setNames("Cost"))
## 3. Create vector specifying number of levels for each attribute
at_lvls <- purrr::map_int(attribute_levels,~length(.x)) %>% unname()
## 4. Create a vector that specifies the coefficient type for each attribute (C - Continuous, D - Dummy)
c_type <- c("D","D","D","D","C")
## 5. Create a list containing vectors with the levels for each continous attribute
con_lvls <- purrr::keep(attribute_levels, is.numeric) %>% unname()
## 6. Create a design matrix of the full factorial of all attribute / level combinations
candidate_des_mat <- idefix::Profiles(lvls = at_lvls, 
                                      coding = c_type, 
                                      c.lvls = con_lvls)
## 7 Specifiy additional features about our survey
## 7.1 Numer of alternatives (including choosing no social anxiety app opt-out)
n_alts <- 3
## 7.2 Confirm that we are using an opt-out option
no_choice_lgl <- TRUE
## 7.3 Index number of opt-out choice
no_choice_idx <- 3
## 7.4 Apply the alternative specific constant to the opt-out option
alt_cte <- c(0,0,1)
## 7.5 Alternatives text for survey
alternatives <- c("Social Anxiety App A", "Social Anxiety App B", "Do not use a social anxiety app")
## 7.7 Number of choice sets and blocks
## Note: As we are aiming for two blocks of 15, the total number of choice sets is 30.
n_sets <- 30
n_blocks <- 2
## 8. Specify coefficient priors
## Note: Priors have been based on subjective interpretation of focus group discussion.
## 8.1 Prior parameter vector
mu <- c(-0.15, # Opt out constant
        0.5,1, # Outcomes
        0.2,0.4, # Information sharing
        0.1,0.2,0.3,0.4, # Social
        0.1,0.2,# Endorsers
        -0.05) # Cost
## 8.2 Prior variance matrix
v <- diag(length(mu))
## 9. Create matrices of parameter value draws
## 9.1 Set seed
set.seed(1987)
## 9.2 Create matrix comprised of ten draws for each parameter
pd <- MASS::mvrnorm(n = 10, mu = mu, Sigma = v)
## 9.3 Split matrix into two: one for the alternative specific constant, the other for the coefficients
p.d <- list(matrix(pd[,1], ncol = 1), pd[,2:12])
## 10 Make list object containing survey features.
survey_features_ls <- list(candidate_des_mat = candidate_des_mat,
                           lvl_names = lvl_names,
                           c_type = c_type,
                           con_lvls = con_lvls,
                           n_alts = n_alts,
                           alternatives = alternatives,
                           no_choice_lgl = no_choice_lgl,
                           no_choice_idx = no_choice_idx,
                           alt_cte = alt_cte,
                           n_sets = n_sets,
                           n_blocks = n_blocks,
                           p.d = p.d)
