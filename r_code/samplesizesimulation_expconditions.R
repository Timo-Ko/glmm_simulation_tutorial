#### PREPARATION ####

# Load required packages

require(packages) 
require(dplyr)
require(ggplot2)
set.seed(101)

# the whole experiment is described in detail in this google doc:
# https://docs.google.com/document/d/1-3MCjl_JsVmqOevUTN-LPg3bJJk1YDGn/edit

# We simulate the data for the diagnostic outcome here since it is the focal point of the study.
# Further, for other DVs, there are not enough prior assumptions available

#### INSERT SAMPLE SIZE ASSUMPTIONS ####

nSubjects <- 200 # number of participants in total 

nExperts <- 50 # we aim to recruit 50 experts (neuroradiologists and radiologists)
nNonExperts <- nSubjects - nExperts # the remaining participants are non-experts

nExp <- 3 # number of experiments
nObs <- 50 # number of observations per experiment (cases the participant has to look at)

N = nSubjects*nExp*nObs # number of total observations

#### SET UP EXPERIMENT DATA FRAME ####

## Define parameter/experimental design values:

subject <- factor(rep(seq(nSubjects)))
experiment <- factor(rep(seq(nExp)))
observation <- factor(rep(seq(nObs)))

# set up experimental conditions 

exp_conditions <- data.frame(group = rep(c(1,2,3,4,5,6), times = 1, each = 3),
                             experiment = rep(c(1,2,3), times = 6, each = 1),
                             advice = rep(c(1,1,0,1,0,1,0,1,1), times = 2, each =1),
                             interpretable = c(1,0,0,1,0,0,0,1,0,
                                               0,1,0,0,0,1,0,0,1))

# interpretability is only relevant when AI advice is given!

# randomly assign subjects to experimental conditions

subject_cond <- data.frame(subject = c(1:nSubjects), expert = sample(x = c(rep(0, nNonExperts), rep(1,nExperts)), size = nSubjects, replace = F), group = sample(1:6, size = nSubjects, replace = TRUE))

table(subject_cond$group) # check if experimental groups have roughly the number of participants in them

# create df for experiment data

experiment_data <- expand.grid(observation, experiment, subject)

colnames(experiment_data) <- c("observation", "experiment", "subject") # add colnames

# add exp conditions group column
experiment_data <- merge(experiment_data, subject_cond[, c("subject", "expert", "group")], by="subject")

# add AI advice and interpretablility columns
experiment_data <- merge(experiment_data, exp_conditions, by=c("group","experiment"))

# reorder columns
experiment_data <- experiment_data[, c("observation", "experiment", "subject", "group", "expert", "advice", "interpretable")]

# sort by subject id
experiment_data <- experiment_data[with(experiment_data, order(subject )),]

# in 20% of cases in each experiment (with 50 observations, i.e. 10 observations) the AI advice is incorrect, incorrect advice is coded with "2" ("1" is correct AI advice, "0" is no AI advice at all)

# group per subject and per experiment and randomize correct and incorrect cases

  for (subject in unique(experiment_data$subject)) {
    
    subjectrows <- experiment_data[experiment_data$subject == subject, ] # get rows from given subject
    
    for (experiment in unique(subjectrows$experiment)) {
    
            subjectrows[subjectrows$experiment == experiment & subjectrows$advice == 1, "advice" ] <- sample(x = c(rep(1, 40), rep(2,10)), size = nObs, replace = F)
            
    }
    experiment_data[experiment_data$subject == subject, ] <- subjectrows #insert altered rows
 }

# check distribution for AI advice
table(experiment_data$advice)

# convert independent variables to factors (relevant for regression models)
experiment_data$expert <- as.factor(experiment_data$expert)
experiment_data$advice <- as.factor(experiment_data$advice)
experiment_data$interpretable <- as.factor(experiment_data$interpretable)

#### REGRESSION MODEL ASSUMPTIONS ####

# here, for modelling, we only consider the experimental conditions where AI advice is given

experiment_data_nocontrol <- experiment_data[experiment_data$advice != 0, ]

# drop unneeded levels
experiment_data_nocontrol$advice <- droplevels(experiment_data_nocontrol$advice)

# advice1 is correct AI advice (baseline)
# advice2 is incorrect AI advice
# expert0 is baseline (no AI advice at all)

# interpretable0 is baseline

beta_diagnosis = c(
  "experiment1" = qlogis(0.75), 
  "experiment2" = qlogis(0.75), 
  "experiment3" = qlogis(0.75), #baseline prob for non experts with correct AI advice is 75%
  "advice2" = qlogis(0.40)-qlogis(0.75), 
  "expert1" = qlogis(0.95)-qlogis(0.75), 
  "interpretable1" = qlogis(0.90)-qlogis(0.75), 
  "advice2:expert1" = qlogis(0.75) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75))), 
  "advice2:interpretable1" = qlogis(0.40) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75))), # this interaction is zero
  "expert1:interpretable1" = qlogis(0.99) - (qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75))), 
  "advice2:expert1:interpretable1" = qlogis(0.85) - # double interaction
                                        (qlogis(0.75) + # main effect experiment
                                           (qlogis(0.40)-qlogis(0.75)) + #main effect advice2
                                           (qlogis(0.95)-qlogis(0.75)) + #main effect expert1
                                           (qlogis(0.90)-qlogis(0.75)) + #main effect interpretable1
                                           (qlogis(0.75) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75)))) + # interaction advice2:expert1
                                           (qlogis(0.99)- (qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75)))))  # interaction expert1:interpretable1
                                    )
 

## double check plogis

# main effects
# false AI advice for non-experts (40%)
qlogis(0.75) + (qlogis(0.40)-qlogis(0.75))

# correct AI advice for experts (95%)
qlogis(0.75) + (qlogis(0.95)-qlogis(0.75))

# interpretable correct AI advice for non-experts (90%)
qlogis(0.75) + (qlogis(0.90)-qlogis(0.75))

# single interaction effects
# for incorrect AI advice for experts (75%)
qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.75) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75))))

# for incorrect interpretable AI advice for non-experts (40%)
qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75)) + (qlogis(0.40) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75))))

# for correct AI advice that is interpretable for experts (99%)
qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75)) + (qlogis(0.99) - (qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75))))

# double interaction effect
# for incorrect interpretable AI advice for experts (85%)
qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75)) + #main effects
  (qlogis(0.75) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75)))) + # single interaction
  (qlogis(0.999)- (qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75)))) + # single interaction
  (qlogis(0.85) - # double interaction
  (qlogis(0.75) + # main effect experiment
     (qlogis(0.40)-qlogis(0.75)) + #main effect advice2
     (qlogis(0.95)-qlogis(0.75)) + #main effect expert1
     (qlogis(0.90)-qlogis(0.75)) + #main effect interpretable1
     (qlogis(0.75) - (qlogis(0.75) + (qlogis(0.40)-qlogis(0.75)) + (qlogis(0.95)-qlogis(0.75)))) + # interaction advice2:expert1
     (qlogis(0.99)- (qlogis(0.75) + (qlogis(0.95)-qlogis(0.75)) + (qlogis(0.90)-qlogis(0.75))))))

## theta parameters (test example cases, especially extreme cases!)
theta = c("subject.(Intercept)" = 0.5, "observation.(Intercept)" = 0.5)

#### REGRESSION MODEL: FORMULA ####

# random intercepts per subject (e.g., due to different skills), random intercept per observation (e.g., due due different difficulty of head scans)
# fixed intercept for experiment

f_diagnosis <- diagnosis ~ 0 + experiment + advice*expert*interpretable + (1|subject) + (1|observation)

### SIMULATE DATA ####

nsim <- 20 # set number of simulations to run

# simulate nsim response vectors
ss <- simulate(~ 0 + experiment + advice*expert*interpretable + (1|subject) + (1|observation), nsim = nsim, family = binomial, 
               newdata = experiment_data_nocontrol, newparams = list(theta = theta, beta = beta_diagnosis))

# add simulated data to df

experiment_data_nocontrol$diagnosis <- ss[, 1] # add response vector for diagnosis to df

### FIT REGRESSION MODELS ####

# fit model once to check if everything has worked

fit1 <- glmer(f_diagnosis, data = experiment_data_nocontrol, family = "binomial")
summary(fit1) #get summary

# fit second model to see if that works too
fit1B <- refit(fit1, ss[[2]])

# create function to fit multiple models (get coefficients for most complex hypothesis - the two way interaction - this will need most participants to detect!)

fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["advice2:expert1:interpretable1", ] # get coefficients from refitted model
}

# apply function and fit multiple models, store results
fitAll <- lapply(seq(nsim), function(i) fitsim(i)) #this is a list 

# save models
saveRDS(fitAll, "data/fitAll.rds")

# restructure output

fitAll_df <- data.frame(matrix(unlist(fitAll), nrow=length(fitAll), byrow=TRUE))

colnames(fitAll_df) <- c("est", "stderr", "zval", "pval")

# get overview of results across fitted models
head(fitAll_df)

# power calculation
with(fitAll_df, mean(pval < 0.05)) # all estimates are significant!

# plot estimates from all fitted models 
ggplot(fitAll_df, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -1.014692, 
                                                             colour = "red")

# plot estimates and CIs from all fitted models 
ggplot(arrange(fitAll_df, est), aes(x = seq(nsim), y = est, ymin = est - 1.96 * 
                                   stderr, ymax = est + 1.96 * stderr)) + geom_pointrange() + geom_hline(yintercept = -1.014692, 
                                                                                                         colour = "red")
## FINISH