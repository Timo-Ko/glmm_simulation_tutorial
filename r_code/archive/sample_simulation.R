## sample size stimation

install.packages("simr")
library(simr)

## simulating without data





## online example 
# 
# # creating covariates
# 
# subj <- factor(1:10)
# class_id <- letters[1:5]
# time <- 0:2
# group <- c("control", "intervention")
# 
# subj_full <- rep(subj, 15)
# class_full <- rep(rep(class_id, each=10), 3)
# time_full <- rep(time, each=50)
# group_full <- rep(rep(group, each=5), 15)
# 
# covars <- data.frame(id=subj_full, class=class_full, treat=group_full, time=factor(time_full))
# 
# covars
# 
# 
# # create model
# 
# model <- makeLmer(y ~ treat*time + (1|class/id), fixef=fixed, VarCorr=rand, sigma=res, data=covars)
# model
# 
# ## run monte carlo simulations
# 
# SimPower1<-powerSim(SimModel,fixed("Condition1", "lr"),
#                     nsim=100, alpha=.045, progress=FALSE)
# SimPower1

# power curves by sample size


#### lme4 only solution (https://stackoverflow.com/questions/51937986/simulate-data-for-mixed-effects-model-with-predefined-parameter)

## Define parameter/experimental design values:
  
nSubjects <- 50 # number of clinicians
nExp <- 3 # number of experiments
nObs <- 48 # number of observations per experiment

## means of a,b are 0 without loss of generality
## we have a 3-level model (trial - test - person)
## we have three experimental variables 
# (AI advice shown yes/no
# Interpretability shown yes/no)
# Timing of advice (sim/ post)
# 
sdvec <- c( advice=1, timing=1, interpretable=1) # vector with standard deviations
rho <- 0.5  ## correlation
betavec <- c(advice=1,timing=2, interpretable=3) # vector with beta parameters
beta_sc <- betavec*sdvec  ## scale beta parameter values by sd
theta <- c(0.4, 0.4, 0.4)  ## theta values random slope
sigma <- 1 # random error

#Set up data frame:
  
library(lme4)      
set.seed(101)

## generate advice, timing, and interpretable variables (these are all binary) - get the google slide in numbers
# timing and interpretability are only relevant when advice is given!
# these will look different in the final data set since order is randomized

N = nSubjects*nExp*nObs # number of total observations

advice <- factor(rep(c(0,1,1),each = nObs, times=nSubjects)) # no advice in first experiment
timing <- factor(rep(c(0,1,2),each = nObs, times=nSubjects)) # no timing in first experiment, simultaneous in the second exp, post in the third experiment
interpretable <- factor(rep(c(0,0,1),each = nObs, times=nSubjects)) # no interpretability in first and second experiment, interpretability in third experiment

# also simulate other factors here! This is done now!
subject <- factor(rep(seq(nSubjects),each=nExp*nObs))
experiment <- factor(rep(seq(nExp),each=nObs, times = nSubjects))
observation <- factor(rep(seq(nObs), times = nSubjects*nExp))

# 150 rows per single subject
# 48*3 per observation
# 7200/3 per experiment

## online code
## sample every nObs'th value of a

# avec <- mm[seq(1,nObs*nSubjects,by=nObs),"a"]
# avec <- rep(avec,each=nObs)  ## replicate
# bvec <- mm[,"b"]
# dd <- data.frame(a=avec,b=bvec,Subject=subj)

experiment_data <- data.frame(advice,timing, interpretable, subject, experiment, observation)

# Simulate participant responses (correct/ incorrect diagnose):

## example
# dd$y <- simulate(~a+b+(1|Subject),
#                    newdata=dd,
#                    newparams=list(beta=beta_sc,theta=theta,sigma=1),
#                    family=gaussian)[[1]]

## my code
experiment_data$y <- simulate(~advice+timing+interpretable+(1|subject)+(1|experiment)+(1|observation),
                 newdata=experiment_data,
                 newparams=list(beta=beta_sc,theta=theta),
                 family=binomial)[[1]]



