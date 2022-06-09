## Sample Size Function
# This implements as a function the code described here: 
# https://www.erim.eur.nl/choice-modelling/resources/software-tools/sample-size-requirements/
compute.sample.size.reqs<-function(design,
                                   test_alpha,
                                   test_beta,
                                   parameters,
                                   ncoefficients,
                                   nalts,
                                   nchoices){
  z_one_minus_alpha<-qnorm(1-test_alpha)
  z_one_minus_beta<-qnorm(1-test_beta)
  #compute the information matrix
  # initialize a matrix of size ncoefficients by ncoefficients filled with zeros.
  info_mat=matrix(rep(0,ncoefficients*ncoefficients), ncoefficients, ncoefficients) 
  # compute exp(design matrix times initial parameter values) 
  exputilities=exp(design%*%parameters)
  # loop over all choice sets
  for (k_set in 1:nchoices) {
    # select alternatives in the choice set
    alternatives=((k_set-1)*nalts+1) : (k_set*nalts)
    # obtain vector of choice shares within the choice set
    p_set=exputilities[alternatives]/sum(exputilities[alternatives])
    # also put these probabilities on the diagonal of a matrix that only contains zeros
    p_diag=diag(p_set)
    # compute middle term P-pp’
    middle_term<-p_diag-p_set%o%p_set
    # pre- and postmultiply with the Xs from the design matrix for the alternatives in this choice set
    full_term<-t(design[alternatives,])%*%middle_term%*%design[alternatives,]
    # Add contribution of this choice set to the information matrix
    info_mat<-info_mat+full_term 
  } # end of loop over choice sets
  #get the inverse of the information matrix (i.e., gets the variance-covariance matrix)
  sigma_beta<-solve(info_mat,diag(ncoefficients)) 
  # Use the parameter values as effect size. Other values can be used here.
  effectsize<-parameters
  # formula for sample size calculation is n>[(z_(beta)+z_(1-alpha))*sqrt(Σγκ)/delta]^2 
  N<-((z_one_minus_beta + z_one_minus_alpha)*sqrt(diag(sigma_beta))/abs(effectsize))^2 
  return(N) # Return results (required sample size for each coefficient)
}
# Create a function to return coefficients for the contstant, categorical attributes and continuous attribute
one.parameter.set<-function(constant.coeff,
                            first.coeff,
                            coeff.multiplier,
                            continuous.coeff){
  second.coeff<-first.coeff*coeff.multiplier
  partial.list<-purrr::pmap(list(a=first.coeff,
                                 b=second.coeff),
                            ~ c(..1,..2))
  partial.vect<-unlist(partial.list)
  # Sunmmarise the coefficients here
  parameters<-c(constant.coeff,partial.vect,continuous.coeff)
  return(parameters)
}
# Create a function to estimate what proportion of parameters have required sample sizes less than our envisioned sample
prop.params.ok<-function(sample.size.results,
                         samplesize, 
                         nbr.parameters){
  countokwithsamplesize<-length(which(sample.size.results<samplesize))
  propokwithsamplesize<-countokwithsamplesize/length(nbr.parameters)
  return(propokwithsamplesize)
}
## Efficient Design Function
export_eff_des <- function(survey_features_ls,
                           # candidate_des_mat, 
                           #                          n_sets, 
                           #                          n_alts, 
                           #                          no_choice_lgl,
                           #                          alt_cte, 
                           parallel = FALSE, 
                           # p.d,
                           output_dir,
                           pilot_analysis = NULL,
                           start_des = NULL){
  if(is.null(pilot_analysis))
    par.draws <- survey_features_ls$p.d
  else
    par.draws <- pilot_analysis$sample
  no_app_optout_ls <- idefix::Modfed(cand.set = survey_features_ls$candidate_des_mat, 
                                     n.sets = survey_features_ls$n_sets, 
                                     n.alts = survey_features_ls$n_alts, 
                                     no.choice = survey_features_ls$no_choice_lgl,
                                     alt.cte = survey_features_ls$alt_cte, 
                                     parallel = parallel, 
                                     par.draws = par.draws,
                                     start.des = start_des)
  ## This section needs abstracting before the function can be used for other survey designs.
  dir.create(output_dir)
  dir.create(paste0(output_dir,"/block_1"))
  dir.create(paste0(output_dir,"/block_2"))
  ## 
  saveRDS(no_app_optout_ls,paste0(output_dir,"/no_app_optout_ls.rds"))
  no_app_optout_ls
}
## Choice Card Functions
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
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left") %>%
    kableExtra::column_spec(1,bold = T, border_right = T) %>%
    kableExtra::column_spec(2:3, 
                            # width = "40em", 
                            color = "black", border_right = T)
}
make_one_block_choice_cards_ls <- function(block_choice_tbs_ls){
  purrr::map(1:length(block_choice_tbs_ls),
             ~ make_choice_card(block_choice_tbs_ls %>% purrr::pluck(.x)))
}
save_one_block_choice_cards <- function(block_choice_cards_ls,
                                        save_path_stub,
                                        output_type = ".png"){
  
  purrr::walk2(block_choice_cards_ls,
               1:length(block_choice_cards_ls),
               ~ save_choice_card_as(.x,
                                     .y,
                                     save_path_stub = save_path_stub,
                                     output_type = output_type)
  )
}
save_choice_card_as <- function(choice_kab,
                                choice_nbr,
                                save_path_stub,
                                output_type){
  file_path = paste0(save_path_stub,
                     "/choice_",
                     choice_nbr,
                     output_type)
  if(output_type==".png"){
    kableExtra::as_image(choice_kab,
                         file = file_path)
  }else{
    kableExtra::save_kable(choice_kab, file = file_path, self_contained = F)
  }
}
export_choice_cards <- function(survey_features_ls,
                                no_app_optout_ls,
                                # lvl_names,
                                # c_type,
                                # con_lvls,
                                # alt_cte,
                                # n_alts,
                                # no_choice_idx,
                                output_dir
                                # ,
                                # n_sets,
                                # n_blocks
                                ){
  ## 12. Translate design matrix into survey choices
  survey_ls <- idefix::Decode(des = no_app_optout_ls$design,
                              lvl.names = survey_features_ls$lvl_names,
                              coding = survey_features_ls$c_type,
                              c.lvls = survey_features_ls$con_lvls,
                              alt.cte = survey_features_ls$alt_cte,
                              n.alts = survey_features_ls$n_alts,
                              no.choice = survey_features_ls$no_choice_idx)
  saveRDS(survey_ls,paste0(output_dir,"/survey_ls.rds"))
  ## 13. Reformat table of survey choice sets, dropping opt-out rows
  ## Note: This needs to be abstracted before this function can be used for other survey designs.
  choices_tb <- tibble::as_tibble(survey_ls$design, 
                                  rownames = "Choice") %>%
    dplyr::rename(Outcomes = V1,
                  `Information sharing` = V2,
                  Social = V3,
                  Endorsers = V4,
                  Cost = V5) %>%
    dplyr::filter(!startsWith(Choice, "no"))
  saveRDS(choices_tb,paste0(output_dir,"/choices_tb.rds"))
  ## 14. Randomly sample from choice set table to create index numbers for two blocks
  block_1_ind <- sample(1:survey_features_ls$n_sets,survey_features_ls$n_sets/survey_features_ls$n_blocks) %>% sort()
  block_2_ind <- setdiff(1:survey_features_ls$n_sets,block_1_ind)
  ## 15. Create list of choice sets for each block
  blocks_choice_tbs_ls_ls <- purrr::map(list(block_1_ind,
                                             block_2_ind),
                                        ~ make_block_choice_tbs_ls(.x,choices_tb))
  ## 16. Create choice cards for each block
  choice_cards_by_block_ls <- purrr::map(blocks_choice_tbs_ls_ls,
                                         ~ make_one_block_choice_cards_ls(.x))
  ## 17. Save choice cards
  purrr::walk2(choice_cards_by_block_ls,
               paste0(output_dir,"/block_",c(1:length(choice_cards_by_block_ls))),
               ~ save_one_block_choice_cards(.x,
                                             .y,
                                             output_type = ".html"))
  list(survey_ls = survey_ls,
       choices_tb = choices_tb,
       blocks_choice_tbs_ls_ls = blocks_choice_tbs_ls_ls,
       choice_cards_by_block_ls = choice_cards_by_block_ls)
  
}
## Survey Preview Function
preview_survey <- function(no_app_optout_ls,
                           survey_features_ls,
                           pilot = T){
  # xdes <- no_app_optout_ls$design
  ## Create xdes_1 and x_des_2
  attributes <- names(survey_features_ls$lvl_names) %>% stringr::str_replace_all("_"," ")
  labels <- survey_features_ls$lvl_names %>% unname()
  if(pilot)
    i.text <- "Pilot Survey Preview (All choice cards from all blocks)"
  else
    i.text <- "Final Survey Preview (All choice cards from all blocks)" 
  b.text <- "Please choose the alternative you prefer"
  e.text <- "Thanks for taking the survey"
  idefix::SurveyApp(des = no_app_optout_ls$design,
                     n.total = survey_features_ls$n_sets,
                     alts = survey_features_ls$alternatives,
                     atts = attributes,
                     lvl.names = labels,
                     c.lvls = survey_features_ls$con_lvls,
                     coding = survey_features_ls$c_typ,
                     buttons.text = b.text,
                     intro.text = i.text,
                     end.text = e.text,
                     no.choice = survey_features_ls$no_choice_idx,
                     alt.cte = survey_features_ls$alt_cte)
}