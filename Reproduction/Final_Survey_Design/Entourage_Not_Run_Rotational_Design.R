## Step 3: Generate a potential DCE design (four categorigal attributes with 3 levels and one continuous attribute with 3 levels)
library(magrittr)
seed_nbr<-1000 # Set seed for reproducibility
nalts<-3
sample_design<-support.CEs::rotation.design(attribute.names = list(Outcomes = c("Current symptoms and skills to manage future situations.", 
                                                                                "Current symptoms.", 
                                                                                "Skills to  manage future situations."),
                                                                   Information_sharing_with_face_to_face_care = c("No information is shared", 
                                                                                           "Information is shared in accordance app policy.", 
                                                                                           "Information is shared based on settings you control."),
                                                                   Social = c("No discussions",
                                                                              "Unmoderated discussions",
                                                                              "Discussions moderated by trained peers.", 
                                                                              "Discussions moderated by mental health clinicians.", 
                                                                              "Discussions moderated by both trained peer moderators and mental health clinicians."),
                                                                   Endorsers = c("Youth mental health experts.", 
                                                                                 "Respected non experts.", 
                                                                                 "No endorsers."), 
                                                                   Monthly_cost_in_dollars = c("0",
                                                                                               "10", 
                                                                                               "20",
                                                                                               "40"
                                                                                               ,
                                                                                               "80"
                                                                                               )),
                                            nalternatives = nalts-1, # Note: no need to specify the opt out option.
                                            nblocks=3,
                                            seed = seed_nbr)
# What the generated questionnaire would look like can be printed here  (can skip this step)
support.CEs::questionnaire(choice.experiment.design = sample_design, 
                           quote = FALSE)
# Convert the sample design into a matrix
sample_design_mat <- support.CEs::make.design.matrix(
  choice.experiment.design = sample_design,
  optout = TRUE,
  categorical.attributes = c("Outcomes",
                             "Information_sharing",
                             "Social",
                             "Endorsers"),
  continuous.attributes = c("Monthly_cost_in_dollars"),
  unlabeled = TRUE)
## Step 4: Subset the questionnaire for our number of choice cards and format design matrices
# Choose the first nchoices choice cards for our study questionnaire
nchoices <- 15
sample_design_tb<-tibble::as_tibble(sample_design_mat) %>%
  dplyr::filter(QES<=nchoices)
# Create matrices for each alterantives
choice_one_tib <- sample_design_tb %>%
  dplyr::filter(ALT==1) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT) %>%
  dplyr::rename(constant = ASC)
choice_two_tib <- sample_design_tb %>%
  dplyr::filter(ALT==2) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT) %>%
  dplyr::rename(constant = ASC)
choice_optout_tib <- sample_design_tb %>%
  dplyr::filter(ALT == 3) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT) %>%
  dplyr::rename(constant = ASC)
choice.one.mat<-as.matrix(choice.one.tib)[,1:10] 
dimnames(choice.one.mat) <- NULL
choice.two.mat<-as.matrix(choice.two.tib)[,c(1,11:19)] 
dimnames(choice.two.mat) <- NULL
choice.optout.mat<-as.matrix(choice.optout.tib)[,1:10] 
dimnames(choice.optout.mat) <- NULL
list.design=purrr::pmap(list(a=split(choice.one.mat,1:nrow(choice.one.mat)),
                             b=split(choice.two.mat,1:nrow(choice.two.mat)),
                             c=split(choice.optout.mat,1:nrow(choice.optout.mat))),
                        ~ matrix(c(..1,..2,..3),nrow=3,byrow=TRUE))
