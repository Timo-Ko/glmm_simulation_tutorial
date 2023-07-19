library(tidyverse)
library(faux)
library(lme4)
library(multcomp)
library(binom)
library(furrr)

# simulate a dataset based on population parameters
simulate <- function(n_subjects = 100, n_items = 50,
  b_0 = 0, b_e = 0.2, b_a = 0.2, b_ea = 0.1, sd_u0s = 1, sd_u0i = 1, ...){
  # simulate design
  dat <- add_random(subject = n_subjects, item = n_items) %>%
    add_between("subject", expert = c(1, 0), .prob = 0.8) %>%
    add_between("subject", advice = c(1, 0)) %>%
    
    # add_between("subject", interpretable = c(1, 0)) %>%
    # mutate(interpretable = replace(interpretable, advice == 1, NA)) %>%
    
    # add random effects
    add_ranef("subject", u0s = sd_u0s) %>%
    add_ranef("item", u0i = sd_u0s) %>%
    # compute dependent variable
    mutate(linpred = b_0 + b_e * expert + b_a * advice + b_ea * expert * advice +
        u0i + u0s) %>%
    mutate(y_prob = plogis(linpred)) %>%
    mutate(y = rbinom(n = n(), size = 1, prob = y_prob))
  dat
}

# fit a model to a data set and compute a pvalue
analyse <- function(data, 
  formula_chr = "y ~ 1 + expert * advice + (1|subject) + (1|item)", 
  null_hypothesis = "expert <= 0", ...){
  # fit model
  model <- glmer(as.formula(formula_chr), data = data, family = "binomial")
  # compute p-value for user defined hypothesis test
  glht <- glht(model, linfct = null_hypothesis)
  pvalue <- summary(glht)$test$pvalues
  as.numeric(pvalue)
}

# helper function for power simulation
sim_helper <- function(...){
  dat <- simulate(...)
  analyse(dat, ...)
}

# enable parallelization
plan("multisession", workers = 4)
# make simulation reproducible
set.seed(2)

# perform simulation study
sim_design <- crossing(
  rep = 1:100,
  n_subjects = c(100, 200),
  n_items = c(10, 100)
) %>%
  mutate(pvalue = future_pmap(., sim_helper, 
    .options = furrr_options(seed = TRUE))) %>%
  unnest(pvalue)

# calculate power for alpha = 0.05 and plot results
power <- sim_design %>%
  group_by(n_subjects, n_items) %>% 
  summarise(power = mean(pvalue < .05), n_sig = sum(pvalue < 0.05), n = n(),
    ci.lwr = binom.confint(n_sig, n, method = "wilson")$lower,
    ci.upr = binom.confint(n_sig, n, method = "wilson")$upper,
    .groups = "drop")
power %>%
  ggplot(aes(n_subjects, n_items, fill = power)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f \n [%.2f; %.2f]", power, ci.lwr, ci.upr)), 
    color = "white", size = 10) +
  scale_fill_viridis_c(limits = c(0, 1))
