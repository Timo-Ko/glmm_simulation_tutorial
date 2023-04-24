#### PREPARATION ####

# Load packages

library(lme4)      
set.seed(101)

#### INSERT SAMPLE SIZE ASSUMPTIONS ####

nSubjects <- 50 # number of clinicians
nExp <- 3 # number of experiments
nObs <- 48 # number of observations per experiment

N = nSubjects*nExp*nObs # number of total observations

# 150 rows per single subject
# 48*3 per observation
# 7200/3 per experiment

#### SET UM EXPERIMENT DATA FRAME ####

#Set up data frame

## generate advice, timing, and interpretable variables (these are all binary) - get the google slide in numbers
# timing and interpretability are only relevant when advice is given!

## Define parameter/experimental design values:

subject <- factor(rep(seq(nSubjects)))
experiment <- factor(rep(seq(nExp)))
observation <- factor(rep(seq(nObs)))

# set up experimental conditions (see https://docs.google.com/document/d/1-3MCjl_JsVmqOevUTN-LPg3bJJk1YDGn/edit)

exp_conditions <- data.frame(group = rep(c(1,2,3,4,5,6), times = 1, each = 3),
                             experiment = rep(c(1,2,3), times = 6, each = 1),
                             advice = rep(c(1,1,0,1,0,1,0,1,1), times = 2, each =1),
                             interpretable = c(1,2,0,1,0,2,0,1,2,
                                               2,1,0,2,0,1,0,2,1))

# 3 conditions

# old condition with timing
# exp_conditions <- data.frame(group = rep(c(1,2,3,4,5,6), times = 1, each = 3), 
#                              experiment = rep(c(1,2,3), times = 6, each = 1), 
#                              advice = rep(c(1,1,0,1,0,1,0,1,1), times = 2, each =1),
#                              timing = c(1,2,0,1,0,2,0,1,2,
#                                         2,1,0,2,0,1,0,2,1),
#                              interpretable = c(1,2,0,1,0,2,0,1,2,
#                                                2,1,0,2,0,1,0,2,1))


# expand grid does not work bc advice = 0 can only work with int & timing = 0
# exp_conditions3 <- expand.grid(experiment = c(1,2,3), 
#                               advice = c(c(1,1,0,1,0,1,0,1,1)),
#                               timing = c(c(1,2,0,1,0,2,0,1,2)),
#                               interpretable = rep(c(1,2,0,1,0,2,0,1,2))

# randomly assign subjects to exp conditions

subject_cond <- data.frame(subject = c(1:50), group = sample(1:6, size = 50, replace = TRUE))

# create df for experiment data

experiment_data <- expand.grid(observation, experiment, subject)

colnames(experiment_data) <- c("observation", "experiment", "subject")

# add exp cond group column
experiment_data <- merge(experiment_data, subject_cond[, c("subject", "group")], by="subject")

# add advice, interpretable and columns

experiment_data <- merge(experiment_data, exp_conditions, by=c("group","experiment"))

# reorder columns
experiment_data <- experiment_data[, c("observation", "experiment", "subject", "group", "advice", "interpretable")]

# sort by subject id
experiment_data <- experiment_data[with(experiment_data, order(subject )),]


# code missing by design NAs as 0 (we deal with the missing conditions by not estimating main effect!)
# experiment_data[is.na(experiment_data)] <- 0


#### INSERT MODEL ASSUMPTIONS ####

sdvec <- c( advice=1, timing=1, interpretable=1) # vector with standard deviations
rho <- 0.5  ## correlation
betavec <- c(advice=1,timing=2, interpretable=3) # vector with beta parameters
beta_sc <- betavec*sdvec  ## scale beta parameter values by sd
theta <- c(0.4, 0.4, 0.4)  ## theta values random slope
sigma <- 1 # random error

### SIMULATE REGRESSION MODELS ####

# online ideas: https://datascienceplus.com/how-to-create-a-loop-to-run-multiple-regression-models/
# this looks really (!!) good: https://rstudio-pubs-static.s3.amazonaws.com/11703_21d1c073558845e1b56ec921c6e0931e.html
# source for theta parameters: https://stats.stackexchange.com/questions/155424/how-does-lme4-optimize-the-variance-parameters-of-random-effects-theta-vector


# set up formula

# model
#f <- formula(y ~ 0 + experiment + advice:(timing*interpretable) + (1|subject) + (1|observation))
# random intercepts per subject (e.g. due to different skills), random intercept per observation (e.g. due due different difficulty of head scan)
# fixed intercept for experiment

f_diagnosis <- formula(diagnosis ~ 0 + experiment + advice:(timing*interpretable) + (1|subject) + (1|observation))
f_time <- formula(time ~ 0 + experiment + advice:(timing*interpretable) + (1|subject) + (1|observation))
f_confidence

# fit model: makes no sense and has convergence issues because of random y variable
mod_diagnosis <- glmer(f_diagnosis, data = experiment_data, family = "binomial")
summary(mod_diagnosis)

mod_time <- glmer(f_time, data = experiment_data, family = "gaussian")
summary(mod_time)

##

nsim <- 20

# old!!

betavec <- c(subject = 1, observation =1, advice=1,interpretable=3) # vector with beta parameters
betavec <- c(1,1,1,2,3) # vector with beta parameters
theta <- c(0.4, 0.4)  ## theta values for random slopes for subject and observation

# simulate nsim response vectors
ss <- simulate(~experiment + advice:(timing*interpretable) + (1|subject) + (1|observation), nsim = nsim, family = binomial, 
               newdata = experiment_data, newparams = list(theta = theta, beta = betavec))


# FINALE FORMEL:
# formula: diagnosis ~ 0 + experiment + advice*interpretable*expert + (1|subject) + (1|observation)


experiment_data$diagnosis <- ss[, 1] # add response vector for diagnosis
# fit model
fit1 <- glmer(diagnosis ~ experiment + advice:(timing*interpretable) + (1|subject) + (1|observation), family = binomial, data = experiment_data)
# fit second model (just testing)
fit1B <- refit(fit1, ss[[2]])

# function to fit multiple models (get coefficients for most complex hypotheses)
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

library(ggplot2)

# plot estimates 
ggplot(fitAll, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -0.2, 
                                                             colour = "red")

str(fitAll)


# simulate large df

dimnames(a) <- list(nsamp.per.block=[vector of values],
                    nblock=[vector of values],
                    effect.size=[vector of values])



# correct/ incorrect diagnosis
experiment_data$diagnosis <- sample(0:1, size = nrow(experiment_data), replace = TRUE)

#abc <- rbinom(x = c(0,1), n = nrow(experiment_data), prob = 0.8)

# diagnosis time
#experiment_data$time <- sample(1:100, size = nrow(experiment_data), replace = TRUE)
experiment_data$time <- rnorm(n = nrow(experiment_data), 20, 5)


### 

# flo beta theta snippet

# hier alle Kombinationen "durchspielen"

beta_old = c(
  experiment1 = 1, experiment2 = 1, experiment3 = 1,
  advice:timing = 0, advice:interpretable = 0,
  advice:timing:interpretable = 0.5
)


beta_diagnosis = c(
  experiment1 = 1, experiment2 = 1, experiment3 = 1,
  advice2 = -0.5, 
  advice1:interpretable = 0.25, advice2:interpretable = -0.05,
  expert = 0.75, expert:advice1 = 0.05, expert:advice2 = -0.25,
  expert:advice1:interpretable = 0, expert:advice2:interpretable = 0.25
)

# log reg
plogis(1.5)
# Lösungswahrscheinlichkeit für KEIN advice, bei 1 ist es ca. 73%


# beta_time
# wie sehen die time Werte aus?
# gamma regression (nur pos. Werte!), oder Zeiten logarithmieren und dann Gauss Regression
# zB 30 sec dann modelliere ich 3.4!!
log(30)

# beta_confidence <- Normalverteilung
# hier entsprechend runden!! zb alles <1.5 wird zu 1 - checken!


theta = c(subject.(Intercept) = 0.001, observation.(Intercept) = 0.001) # standardabweichungen für Diagnose (hier expert uncertainty einfließen lassen)
# andere thetas für andere Outcomes?!
# simulationen anschauen (ist realistisch?!) - wie sind Lösungswahrsch zwischen Observations?
# Expertise -> fixed effect, dann random slope für observation!
# bei random slopes: https://stats.stackexchange.com/questions/155424/how-does-lme4-optimize-the-variance-parameters-of-random-effects-theta-vector
# für theta berechnung, oder power analyse vereinfachen

# simulate data
dat <- simulate(f,
                nsim = 100,
                newdata = experiment_data,
                newparams = list(beta = beta, theta = theta),
                family = binomial,
                allow.new.levels = TRUE
)



