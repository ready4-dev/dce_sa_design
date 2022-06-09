## Step 1: Load required packages
install.packages("support.CEs")
install.packages("magrittr")
install.packages("tidyverse")
require(support.CEs)
require(magrittr)
## Step 2: Create function to calculate sample size requirements
source("functions.R")
## Step 3: Generate a potential DCE design (four categorigal attributes with 3 levels and one continuous attribute with 3 levels)
seed.nbr<-1000 # Set seed for reproducibility
nalts<-3
nchoices<-12
sample.design<-support.CEs::Lma.design(attribute.names = list(Categorical.One = c("first.var.level.1.", "first.var.level.2.", "first.var.level.3."),
                                                 Categorical.Two = c("second.var.level.1.", "second.var.level.2.", "second.var.level.3."),
                                                 Categorical.Three = c("third.var.level.1.", "third.var.level.2.", "third.var.level.3."),
                                                 Categorical.Four = c("fourth.var.level.1.", "fourth.var.level.2.", "fourth.var.level.3."), 
                                                 TimeMoney = c("25", "50", "75")
                                  ),
           nalternatives=nalts-1, # Note: no need to specify the opt out option.
           nblocks=1,
           seed=seed.nbr
           )
# What the generated questionnaire would look like can be printed here  (can skip this step)
support.CEs::questionnaire(choice.experiment.design = sample.design, 
              quote = FALSE)
# Convert the sample design into a matrix
sample.design.mat <- support.CEs::make.design.matrix(
  choice.experiment.design = sample.design,
  optout = TRUE,
  categorical.attributes = c("Categorical.One",
                             "Categorical.Two",
                             "Categorical.Three",
                             "Categorical.Four"),
  continuous.attributes = c("TimeMoney"),
  unlabeled = FALSE)
## Step 4: Subset the questionnaire for our number of choice cards and format design matrices
# Choose the first nchoices choice cards for our study questionnaire
sample.design.tib<-tibble::as_tibble(sample.design.mat) %>%
  dplyr::filter(QES<=nchoices)
# Create matrices for each alterantives
choice.one.tib<-sample.design.tib %>%
  dplyr::filter(ASC1==1) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT,
                -ASC2) %>%
  dplyr::rename(constant=ASC1)
choice.two.tib<-sample.design.tib %>%
  dplyr::filter(ASC2==1) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT,
                -ASC1) %>%
  dplyr::rename(constant=ASC2)
choice.optout.tib<-sample.design.tib %>%
  dplyr::filter(ASC1==0 & ASC2==0) %>%
  dplyr::select(-BLOCK, 
                -QES, 
                -ALT,
                -ASC1) %>%
  dplyr::rename(constant=ASC2)
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
# Create the matrix which summarises the design we will use in our example
matrix.design<-Reduce(rbind,list.design)#[1:(12*3),]
# Step 5: Prepare functions and data to enable us to do sample size calculations
#
# Sample distributions of the hypothesised parameter values for each of the coefficients
parameter.inputs<-list(a=rnorm(500,1,0.2),
                       b=rnorm(500,2,0.6),
                       c=rnorm(500,-1.5,0.3),
                       d=rnorm(500,0.5,0.1),
                       e=rnorm(500,1.7,0.5),
                       f=rnorm(500,1.3,0.2),
                       g=rnorm(500,1.1,0.2),
                       h=rnorm(500,1.6,0.2),
                       i=rnorm(500,1.2,0.2),
                       j=rnorm(500,0.05,0.01)
                       )
parameter.list<-purrr::map(1:500,
                           ~ one.parameter.set(constant.coeff = parameter.inputs %>% 
                                                 purrr::pluck("a") %>%
                                                 as.vector() %>%
                                                 dplyr::nth(.x),
                                               first.coeff = c(parameter.inputs %>% 
                                                                 purrr::pluck("b") %>%
                                                                 as.vector() %>%
                                                                 dplyr::nth(.x),
                                                               parameter.inputs %>% 
                                                                 purrr::pluck("c") %>%
                                                                 as.vector() %>%
                                                                 dplyr::nth(.x),
                                                               parameter.inputs %>% 
                                                                 purrr::pluck("d") %>%
                                                                 as.vector() %>%
                                                                 dplyr::nth(.x),
                                                               parameter.inputs %>% 
                                                                 purrr::pluck("e") %>%
                                                                 as.vector() %>%
                                                                 dplyr::nth(.x)),
                                               coeff.multiplier = c(parameter.inputs %>% 
                                                                      purrr::pluck("f") %>%
                                                                      as.vector() %>%
                                                                      dplyr::nth(.x),
                                                                    parameter.inputs %>% 
                                                                      purrr::pluck("g") %>%
                                                                      as.vector() %>%
                                                                      dplyr::nth(.x),
                                                                    parameter.inputs %>% 
                                                                      purrr::pluck("h") %>%
                                                                      as.vector() %>%
                                                                      dplyr::nth(.x),
                                                                    parameter.inputs %>% 
                                                                      purrr::pluck("i") %>%
                                                                      as.vector() %>%
                                                                      dplyr::nth(.x)),
                                               continuous.coeff = parameter.inputs %>% 
                                                 purrr::pluck("j") %>%
                                                 as.vector() %>%
                                                 dplyr::nth(.x)
                                               )
)
#
# Step 6: Perform calculations
# Calculate what proportion of parameters are OK with envisioned sample size at p=0.01 and 80% power
raw.results.01<-purrr::map(parameter.list,
                           ~ compute.sample.size.reqs(design=matrix.design, # load the design information
                                                      test_alpha=0.01,
                                                      test_beta=0.2,
                                                      parameters=.,
                                                      ncoefficients=10,
                                                      nalts=nalts,
                                                      nchoices=nchoices))
prop.ok.01.list<-purrr::map(raw.results.01,
                        ~ prop.params.ok(.,
                                         200, 
                                         10)) 
prop.ok.01.mean<-mean(unlist(prop.ok.01.list %>% purrr::flatten()))
# Calculate what proportion of parameters are OK with envisioned sample size at p=0.05 and 80% power
raw.results.05<-purrr::map(parameter.list,
                           ~ compute.sample.size.reqs(design=matrix.design, # load the design information
                                                      test_alpha=0.05,
                                                      test_beta=0.2,
                                                      parameters=.,
                                                      ncoefficients=10,
                                                      nalts=nalts,
                                                      nchoices=nchoices))
prop.ok.05.list<-purrr::map(raw.results.05,
                            ~ prop.params.ok(.,
                                             200, 
                                             10)) 
prop.ok.05.mean<-mean(unlist(prop.ok.05.list %>% purrr::flatten()))
