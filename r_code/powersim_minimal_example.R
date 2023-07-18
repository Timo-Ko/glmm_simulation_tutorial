library(tidyverse)
library(faux)
library(lme4)
library(multcomp)
library(furrr)

# code adapted from the vignette of the faux package:
# https://debruine.github.io/faux/articles/sim_mixed.html

# simulation function that simulates data, fits a model and computes a p-value
sim <- function(n_subjects = 100, n_items = 50,
  b_0 = 0, b_e = 0.2, b_a = 0.2, b_ea = 0.1, sd_u0s = 1, sd_u0i = 1, ...){
  # simulate design
  dat <- add_random(subject = n_subjects, item = n_items) %>%
    add_between("subject", expert = c(1, 0), .prob = 0.2) %>%
    add_between("subject", advice = c(1, 0)) %>%
    
    # add_between("subject", interpretable = c(1, 0)) %>%
    # mutate(interpretable = replace(interpretable, advice == 1, NA)) %>%
    
    # add random effects
    add_ranef("subject", u0s = sd_u0s) %>%
    add_ranef("item", u0i = sd_u0s) %>%
    # compute dependent variable
    mutate(linpred = b_0 + b_e * expert + b_a * advice + 
        b_ea * expert * advice +
        u0i + u0s) %>%
    mutate(y_prob = plogis(linpred)) %>%
    mutate(y = rbinom(n = n(), size = 1, prob = y_prob))
  # fit model
  model <- glmer(y ~ expert * advice + (1|subject) + (1|item),
    data = dat, family = "binomial")
  # compute p-value for user defined hypothesis test
  null_hypothesis <- "expert <= 0"
  glht <- glht(model, linfct = null_hypothesis)
  data.frame(pvalue = summary(glht)$test$pvalues)
}

# enable parallelization
plan("multisession", workers = 4)
# make simulation reproducible
set.seed(2)

# perform simulation study
sim_design <- crossing(
  rep = 1:200,
  n_subjects = c(100, 200),
  n_items = c(50, 100)
) %>%
  mutate(analysis = future_pmap(., sim, 
    .options = furrr_options(seed = TRUE))) %>%
  unnest(analysis)

# calculate power for alpha = 0.05 and plot results
sim_design %>%
  group_by(n_subjects, n_items) %>% 
  summarise(power = mean(pvalue < .05), 
    .groups = "drop") %>%
  ggplot(aes(n_subjects, n_items, fill = power)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", power)), color = "white", size = 10) +
  scale_fill_viridis_c(limits = c(0, 1))
