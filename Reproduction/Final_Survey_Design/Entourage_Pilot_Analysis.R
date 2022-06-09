responses <- read.csv("data-raw/pilot_survey.csv")
n_responses <- nrow(responses)
choice_vars <- names(responses)[names(responses) %>% startsWith("DCE_B")] # Manually check order is correct
b1_vars <- choice_vars[choice_vars %>% startsWith("DCE_B1")]
b2_vars <- choice_vars[choice_vars %>% startsWith("DCE_B2")]
choice_responses <- responses %>% 
  dplyr::select(choice_vars) 
y <- purrr::map(1:n_responses, 
                ~ choice_responses %>% 
                  dplyr::slice(.x) %>% 
                  as.numeric() %>%
                  purrr::map(
                    ~ switch(.x,c(1,0,0),c(0,1,0),c(0,0,1))) %>% 
                  unlist()) %>%
  unlist()
blocks_vec <-purrr::map(1:n_responses, 
                        ~ choice_responses %>% 
                          dplyr::slice(.x) %>% 
                          as.numeric()) %>%
  purrr::map_chr(~ ifelse(all(is.na(c(.x)[1:15])),"Block_2","Block_1"))
des <- purrr::map(blocks_vec,
                   ~ no_app_optout_ls$design[switch("Block_1","Block_1" = 1:45, "Block_2" = 46:90),]) %>%
  stats::setNames(letters[1:7]) %>%
  purrr::reduce(~rbind(.x,.y))
## Constraints
low = c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0.005, 0.006, -Inf)
up = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, -0.0499)
pilot_analysis <- idefix::ImpsampMNL(n.draws = 100, 
                                     prior.mean = mu, 
                                     prior.covar = v, 
                                     des = des, 
                                     n.alts = 3, 
                                     y = y, 
                                     lower = low, 
                                     upper = up, 
                                     alt.cte = c(0,0,1))
###

# mlogit.data <- idefix::Datatrans(pkg = "mlogit", des = des, y = y, n.alts = 3, n.sets = 15, n.resp = n_responses, bin = TRUE)
# Rchoice.data <- idefix::Datatrans(pkg = "Rchoice", des = des, y = y,n.alts = 3, n.sets = 15, n.resp = n_responses, bin = TRUE)
# ChoiceModelR.data <- idefix::Datatrans(pkg = "ChoiceModelR", des = des, y = y,n.alts = 3, n.sets = 15, n.resp = n_responses, bin = TRUE)
# 
# ChoiceModelR::choicemodelr(data = ChoiceModelR.data,
#                            xcoding = c(0,0,0,0,0,0,0,0,0,0,0,1),
#                            mcmc = list(R=1000, use=100),
#                            options = list(none=F, save=T, keep=1))
# 
# 
# idefix.data <- idefix::aggregate_design
# des <- as.matrix(idefix.data[, 3:8], ncol = 6)
# y <- idefix.data[, 9]
# bayesm.data <- idefix::Datatrans(pkg = "bayesm", des = des, y = y,n.alts = 2, n.sets = 8, n.resp = 7, bin = TRUE)
# Mix.pro.data <- idefix::Datatrans(pkg = "Mixed.Probit", des = des, y = y,n.alts = 2, n.sets = 8, n.resp = 7, bin = TRUE)
# mlogit.data <- idefix::Datatrans(pkg = "mlogit", des = des, y = y,n.alts = 2, n.sets = 8, n.resp = 7, bin = TRUE)
# 
# des <- purrr::map(1:7,~ idefix::nochoice_design) %>%
#   stats::setNames(letters[1:7]) %>%
#   purrr::reduce(~rbind(.x,.y))
# y <- purrr::map(1:7, ~ purrr::map(1:8,~sample(c(1,0,0))) %>% unlist()) %>% unlist()
# bayesm.data <- idefix::Datatrans(pkg = "bayesm", des = des, y = y,n.alts = 3, n.sets = 8, n.resp = 7, bin = TRUE)
# Mix.pro.data <- idefix::Datatrans(pkg = "Mixed.Probit", des = des, y = y,n.alts = 3, n.sets = 8, n.resp = 7, bin = TRUE)
# mlogit.data <- idefix::Datatrans(pkg = "mlogit", des = des, y = y,n.alts = 3, n.sets = 8, n.resp = 7, bin = TRUE)
# Rchoice.data <- idefix::Datatrans(pkg = "Rchoice", des = des, y = y,n.alts = 3, n.sets = 8, n.resp = 7, bin = TRUE)
# ChoiceModelR.data <- idefix::Datatrans(pkg = "ChoiceModelR", des = des, y = y,n.alts = 3, n.sets = 8, n.resp = 7, bin = TRUE)
# #
