# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)

# Load data
data(cherry_blossom_sample)
running <- cherry_blossom_sample

running <- running %>%
  select(runner, age, net) %>%
  na.omit()

running_model_1_prior <- stan_glmer(
  net ~ age + (1 | runner),
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735,
  prior_PD = TRUE)

set.seed(84735)
running %>%
  add_fitted_draws(running_model_1_prior, n = 4) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = paste(runner, .draw))) +
  facet_wrap(~ .draw)
running %>%
  add_predicted_draws(running_model_1_prior, n = 100) %>%
  ggplot(aes(x = net)) +
  geom_density(aes(x = .prediction, group = .draw)) +
  xlim(-100,300)

ggplot(running, aes(x = age, y = net)) +
  geom_point() +
  facet_wrap(~ runner)

# Simulate the posterior
running_model_1 <- update(running_model_1_prior, prior_PD = FALSE)
# Check the prior specifications
prior_summary(running_model_1)
# Markov chain diagnostics
mcmc_trace(running_model_1)
mcmc_dens_overlay(running_model_1)
mcmc_acf(running_model_1)
neff_ratio(running_model_1)
rhat(running_model_1)

tidy_summary_1 <- tidy(running_model_1, effects = "fixed",
                       conf.int = TRUE, conf.level = 0.80)
tidy_summary_1

B0 <- tidy_summary_1$estimate[1]
B1 <- tidy_summary_1$estimate[2]
running %>%
  add_fitted_draws(running_model_1, n = 200, re_formula = NA) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(aes(y = .value, group = .draw), alpha = 0.1) +
  geom_abline(intercept = B0, slope = B1, color = "blue") +
  lims(y = c(75, 110))

runner_summaries_1 <- running_model_1 %>%
  spread_draws(`(Intercept)`, b[,runner]) %>%
  mutate(runner_intercept = `(Intercept)` + b) %>%
  select(-`(Intercept)`, -b) %>%
  median_qi(.width = 0.80) %>%
  select(runner, runner_intercept, .lower, .upper)

runner_summaries_1 %>%
  filter(runner %in% c("runner:4", "runner:5"))

running %>%
  filter(runner %in% c("4", "5")) %>%
  add_fitted_draws(running_model_1, n = 100) %>%
  ggplot(aes(x = age, y = net)) +
  geom_line(
    aes(y = .value, group = paste(runner, .draw), color = runner),
    alpha = 0.1) +
  geom_point(aes(color = runner))

ggplot(running, aes(y = net, x = age, group = runner)) +
  geom_abline(data = runner_summaries_1, color = "gray",
              aes(intercept = runner_intercept, slope = B1)) +
  geom_abline(intercept = B0, slope = B1, color = "blue") +
  lims(x = c(50, 61), y = c(50, 135))

tidy_sigma <- tidy(running_model_1, effects = "ran_pars")
tidy_sigma

sigma_0 <- tidy_sigma[1,3]
sigma_y <- tidy_sigma[2,3]
sigma_0^2 / (sigma_0^2 + sigma_y^2)
sigma_y^2 / (sigma_0^2 + sigma_y^2)

running %>%
  filter(runner %in% c("4", "5", "20", "29")) %>%
  ggplot(., aes(x = age, y = net)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~ runner)

ggplot(running, aes(x = age, y = net, group = runner)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)

running_model_2 <- stan_glmer(
  net ~ age + (age | runner),
  data = running, family = gaussian,
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735, adapt_delta = 0.99999
)

prior_summary(running_model_2)

tidy(running_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)

runner_chains_2 <- running_model_2 %>%
  spread_draws(`(Intercept)`, b[term, runner], `age`) %>%
  pivot_wider(names_from = term, names_glue = "b_{term}",
              values_from = b) %>%
  mutate(runner_intercept = `(Intercept)` + `b_(Intercept)`,
         runner_age = age + b_age)

runner_summaries_2 <- runner_chains_2 %>%
  group_by(runner) %>%
  summarize(runner_intercept = median(runner_intercept),
            runner_age = median(runner_age))

head(runner_summaries_2, 3)

ggplot(running, aes(y = net, x = age, group = runner)) +
  geom_abline(data = runner_summaries_2, color = "gray",
              aes(intercept = runner_intercept, slope = runner_age)) +
  lims(x = c(50, 61), y = c(50, 135))

runner_summaries_2 %>%
  filter(runner %in% c("runner:1", "runner:10"))

tidy(running_model_2, effects = "ran_pars")

pp_check(running_model_1) +
  labs(x = "net", title = "running model 1")
pp_check(running_model_2) +
  labs(x = "net", title = "running model 2")

set.seed(84735)
prediction_summary(model = running_model_1, data = running)
prediction_summary(model = running_model_2, data = running)

prediction_summary_cv(model = running_model_1, data = running,
                      k = 10, group = "runner")

elpd_hierarchical_1 <- loo(running_model_1)
elpd_hierarchical_2 <- loo(running_model_2)

loo_compare(elpd_hierarchical_1, elpd_hierarchical_2)

running %>%
  filter(runner %in% c("1", "10")) %>%
  ggplot(., aes(x = age, y = net)) +
  geom_point() +
  facet_grid(~ runner) +
  lims(x = c(54, 61))

set.seed(84735)
predict_next_race <- posterior_predict(
  running_model_1,
  newdata = data.frame(runner = c("1", "Miles", "10"),
                       age = c(61, 61, 61)))

B0 + B1 * 61
mcmc_areas(predict_next_race, prob = 0.8) +
  ggplot2::scale_y_discrete(labels = c("runner 1", "Miles", "runner 10"))
\