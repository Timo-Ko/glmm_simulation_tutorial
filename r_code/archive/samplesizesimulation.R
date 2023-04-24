#### PREPARATION ####

# inspiration for this code came from..

# online ideas: https://datascienceplus.com/how-to-create-a-loop-to-run-multiple-regression-models/
# this looks really (!!) good: https://rstudio-pubs-static.s3.amazonaws.com/11703_21d1c073558845e1b56ec921c6e0931e.html
# source for theta parameters: https://stats.stackexchange.com/questions/155424/how-does-lme4-optimize-the-variance-parameters-of-random-effects-theta-vector

# Load packages

library(lme4) 
library(ggplot2)
set.seed(101)

#### INSERT SAMPLE SIZE ASSUMPTIONS ####

nSubjects <- 250 # number of participants (experts to non experts with 1:5 ratio)
nExp <- 3 # number of experiments
nObs <- 50 # number of observations per experiment

N = nSubjects*nExp*nObs # number of total observations

#### SET UM EXPERIMENT DATA FRAME ####

# Set up data frame

# generate experimental variables 
# interpretability is only relevant when AI advice is given!

## Define parameter/experimental design values:

subject <- factor(rep(seq(nSubjects)))
experiment <- factor(rep(seq(nExp)))
observation <- factor(rep(seq(nObs)))

# set up experimental conditions (see https://docs.google.com/document/d/1-3MCjl_JsVmqOevUTN-LPg3bJJk1YDGn/edit)

exp_conditions <- data.frame(group = rep(c(1,2,3,4,5,6), times = 1, each = 3),
                             experiment = rep(c(1,2,3), times = 6, each = 1),
                             advice = rep(c(1,1,0,1,0,1,0,1,1), times = 2, each =1),
                             interpretable = c(1,0,0,1,0,0,0,1,0,
                                               0,1,0,0,0,1,0,0,1))

# randomly assign subjects to experimental conditions

subject_cond <- data.frame(subject = c(1:250), expert = sample(x = 0:1, size = 250, replace = TRUE, prob = c(0.8, 0.2)), group = sample(1:6, size = 250, replace = TRUE))

# we aim to recruit 250 participant with a 5:1 ratio of non experts to experts

# create df for experiment data

experiment_data <- expand.grid(observation, experiment, subject)

colnames(experiment_data) <- c("observation", "experiment", "subject") # add colnames

# add exp cond group column
experiment_data <- merge(experiment_data, subject_cond[, c("subject", "expert", "group")], by="subject")

# add advice, interpretable columns
experiment_data <- merge(experiment_data, exp_conditions, by=c("group","experiment"))

# in 20% of cases the AI advice is incorrect, this is coded with "2" ("1" is correct AI advice)
experiment_data$advice[experiment_data$advice == 1] <- sample(x = 1:2, size = length(which(experiment_data$advice == 1)), replace = TRUE, prob = c(0.8, 0.2))

# check result distribution
table(experiment_data$advice)

# reorder columns
experiment_data <- experiment_data[, c("observation", "experiment", "subject", "expert", "group", "advice", "interpretable")]

# sort by subject id
experiment_data <- experiment_data[with(experiment_data, order(subject )),]

# convert independent variables to factors
experiment_data$expert <- as.factor(experiment_data$expert)
experiment_data$advice <- as.factor(experiment_data$advice)
experiment_data$interpretable <- as.factor(experiment_data$interpretable)



#### REGRESSION MODEL ASSUMPTIONS ####

# beta regression weights

# advice0 is no AI advice at all
# advice1 is correct AI advice
# advice2 is incorrect AI advice

beta_diagnosis = c(experiment1 = qlogis(0.7), experiment2 = qlogis(0.7), experiment3 = qlogis(0.7), #baseline prob for non experts without AI advice is 70%
  expert = qlogis(0.9)-qlogis(0.7), #baseline prob for experts without AI advice is 90% (70+20%)
  advice1 = -2.944439, #correct non-interpretable advice increases prob by 5% (i.e., qlogis(0.05)) for non experts and experts
  advice2 = -0.8472979, #incorrect non-interpretable advice reduces prob by 30% (i.e., qlogis(0.2)) for non experts
  "advice1:interpretable" = -2.944439, #correct interpretable advice increases prob by 5% (i.e., qlogis(0.05))for non experts
  "expert:advice1" = -2.944439, #correct non-interpretable advice increases prob by 5% (i.e., qlogis(0.05)) for experts
  "expert:advice2" = -1.734601, #incorrect non-interpretable advice reduces prob by 15% (i.e., qlogis(0.15)) for experts
  "expert:advice1:interpretable" = -2.944439, #correct interpretable advice increases prob by 5% (i.e., qlogis(0.1)) for experts
  "expert:advice2:interpretable" = -2.197225) #incorrect interpretable advice increases prob by 10%(i.e., qlogis(0.1)) for experts

qlogis(0.8)

# check likelihood for correct diagnosis using plogis (log reg), qlogis is the inverse of plogis


# # all others are not so important
# # we take to log of the expected time to only have positive values 
# beta_time <- c(experiment1 = log(30), experiment2 = log(30), experiment3 = log(30), #baseline time to diagnosis for non experts without AI advice is 30 seconds
# expert = -log(10), #baseline prob for experts without AI advice is 20 seconds (30 sec - 10 sec)
# advice2 = -qlogis(0.2), #incorrect advice reduces prob by 38% for non experts
# "advice1:interpretable" = 0.25, "advice2:interpretable" = -0.05,
# "expert:advice1" = 0.05, "expert:advice2" = -0.25,
# "expert:advice1:interpretable" = 0, "expert:advice2:interpretable" = 0.25)
# 
# # use gamma regression (nur pos. Werte!) ?, oder Zeiten logarithmieren und dann normale Gauss Regression
# # zB 30 sec dann modelliere ich 3.4!!
# log(30)
# log(-2)
# 
# beta_confidence <- c(experiment1 = log(30), experiment2 = log(30), experiment3 = log(30), #baseline confidence in diagnosis for non experts without AI advice is 5 (on 7 point Likert scale)
# expert = -log(10), #baseline prob for experts without AI advice is 20 seconds (30 sec - 10 sec)
# advice2 = -qlogis(0.2), #incorrect advice reduces prob by 38% for non experts
# "advice1:interpretable" = 0.25, 
# "advice2:interpretable" = -0.05,
# "expert:advice1" = 0.05, 
# "expert:advice2" = -0.25,
# "expert:advice1:interpretable" = 0, 
# "expert:advice2:interpretable" = 0.25)
# # hier entsprechend runden!! zb alles <1.5 wird zu 1 - checken! weil keine ungerade responses möglich!
# 
# beta_usefulness <- c(experiment1 = 6, experiment2 = 6, experiment3 = 6, #baseline AI advice usefulness rating for non experts is 6 (on 7 point Likert scale)
# advice2 = -qlogis(0.2), #incorrect advice reduces prob by 38% for non experts
# "advice1:interpretable" = 0.25, "advice2:interpretable" = -0.05,
# "expert:advice1" = 0.05, "expert:advice2" = -0.25,
# "expert:advice1:interpretable" = 0, "expert:advice2:interpretable" = 0.25)
# 
# # hier entsprechend in simulated data runden!! zb alles <1.5 wird zu 1 - checken! weil keine ungerade responses möglich!


## theta regression weights
theta <- c(subject = 0.1, observation = 0.1)  ## theta values for two random slope, these need to be adjusted?


### More comments from Flo on parameters

theta = c(subject.(Intercept) = 0.001, observation.(Intercept) = 0.001) # standardabweichungen für Diagnose (hier expert uncertainty einfließen lassen)
# andere thetas für andere Outcomes?!
# simulationen anschauen (ist realistisch?!) - wie sind Lösungswahrsch zwischen Observations?
# Expertise -> fixed effect, dann random slope für observation!
# bei random slopes: https://stats.stackexchange.com/questions/155424/how-does-lme4-optimize-the-variance-parameters-of-random-effects-theta-vector
# für theta berechnung, oder power analyse vereinfachen


#### REGRESSION MODEL: FORMULAE ####

# random intercepts per subject (e.g. due to different skills), random intercept per observation (e.g. due due different difficulty of head scan)
# fixed intercept for experiment

diagnosis ~ 0 + experiment + advice*expert + (1|subject) + (1|observation)
# f_time ~ 0 + experiment + advice*expert + (1|subject) + (1|observation)
# f_confidence ~ 0 + experiment + advice*expert + (1|subject) + (1|observation)
# f_usefulness ~ 0 + experiment + advice*expert + (1|subject) + (1|observation)

### SIMULATE DATA ####

nsim <- 20 # set number of simulations to run

# simulate nsim response vectors
ss <- simulate(0 + experiment + advice:expert + (1|subject) + (1|observation), nsim = nsim, family = binomial, 
               newdata = experiment_data, newparams = list(theta = theta, beta = beta_diagnosis))

# add simulated data to df

experiment_data$diagnosis <- ss[, 1] # add response vector for diagnosis to df

### FIT REGRESSION MODELS ####

# fit model once to check if everything has worked

# diagnosis
mod_diagnosis <- glmer(f_diagnosis, data = experiment_data, family = "binomial")
summary(mod_diagnosis)

# time to diagnosis
# mod_time <- glmer(f_time, data = experiment_data, family = "gaussian")
# summary(mod_time)

# test diagnosis
experiment_data$diagnosis <- rbinom(n = nrow(experiment_data), size = 1, prob = 0.5)


str(experiment_data)

# eine neue faktorvariable erstellen mit 5 werten (intertpretable*advice)
experiment_data$dummy <- as.factor(sapply(1:nrow(experiment_data), function (x) paste0(experiment_data[x, "advice"], experiment_data[x, "interpretable"])))
#test <- sapply(1:nrow(experiment_data), function (x) paste(experiment_data[x, "advice"], experiment_data[x, "interpretable"]))
table(test)

# fit model
fit1 <- glmer(diagnosis ~ 0 + experiment + advice*expert*interpretable + (1|subject) + (1|observation), family = binomial, data = experiment_data)

fit1 <- glmer(diagnosis ~ 0 + experiment + expert*dummy + (1|subject) + (1|observation), family = binomial, data = experiment_data)


fit1 <- glmer(diagnosis ~ 0 + experiment + advice*expert + (1|subject) + (1|observation), family = binomial, data = experiment_data)
fit1 <- glmer(diagnosis ~ 0 + experiment + advice*expert + (1|subject), family = binomial, data = experiment_data)
fit1_glm <- glm(diagnosis ~  0 + experiment + advice*expert*interpretable , family = binomial, data = experiment_data)


summary(fit1)
# dummy 00 ist intercept 


# fit second model (just testing)
fit1B <- refit(fit1, ss[[2]])

# create function to fit multiple models (get coefficients for most complex hypotheses)

fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["expert:advice2:interpretable", ] # get coefficients from refitted model
}

# apply function and fit multiple models, store results
fitAll <- lapply(seq(nsim), function(i) fitsim(i)) #this is a list 

# set names in overview table
fitAll <- t(as.data.frame(fitAll))

rownames(fitAll) <- c(1:nrow(fitAll))

colnames(fitAll) <- c("est", "stderr", "zval", "pval")

# get overview of results across fitted models
head(fitAll)

# power calculation
with(fitAll, mean(pval < 0.05))

# plot estimates from all fitted models 
ggplot(fitAll, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -0.2, 
                                                             colour = "red")

str(fitAll)



# simulate large df once everything is figured out

# dimnames(a) <- list(nsamp.per.block=[vector of values],
#                     nblock=[vector of values],
#                     effect.size=[vector of values])



beta = c(
  experiment1 = 1, experiment2 = 1, experiment3 = 1,
  advice:expert = 0, advice:interpretable = 0,
  advice:expert:interpretable = 0.5
)
theta = c(subject.(Intercept) = 0.001, observation.(Intercept) = 0.001)

# simulate data
dat <- simulate(~ 0 + experiment + advice*expert + (1|subject) + (1|observation),
                nsim = 100,
                newdata = experiment_data,
                newparams = list(beta = beta, theta = theta),
                family = binomial,
                allow.new.levels = TRUE
)


### FINISH 