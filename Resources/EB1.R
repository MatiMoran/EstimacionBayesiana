require(brms)
require(palmerpenguins)
require(tidyverse)
require(broom)
require(broom.mixed)
require(tidybayes)

# datos
data(penguins, package = "palmerpenguins")
chinstrap <- penguins %>% 
  filter(species == "Chinstrap")

# exploratorio
chinstrap %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se = FALSE)

# fit F
fit1.f <- lm(
  data = chinstrap,
  bill_length_mm ~ 1 + body_mass_g
)

# resumen
summary(fit1.f)
coef(fit1.f)

# predicciones
nd <- tibble(body_mass_g = seq(from = min(chinstrap$body_mass_g),
                               to = max(chinstrap$body_mass_g),
                               length.out = 50))

predict(fit1.f,
        interval = "confidence",
        newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  ggplot(aes(x = body_mass_g)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 1/3) +
  geom_line(aes(y = fit)) +
  geom_point(data = chinstrap,
             aes(y = bill_length_mm))


# fit B

fit1.b <- brm(
  data = chinstrap,
  bill_length_mm ~ 1 + body_mass_g
)

summary(fit1.b)

conditional_effects(fit1.b)

# comparacion f y b
bind_rows(
  tidy(fit1.f, conf.int = TRUE) %>% select(term, estimate, contains("conf")),
  tidy(fit1.b) %>% select(term, estimate, contains("conf")) %>% filter(term != "sd__Observation")
  ) %>% 
  mutate(method = rep(c("lm()", "brm()"), each = 2)) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = method)) +
  geom_pointrange() +
  scale_x_continuous("parameter space", expand = expansion(mult = 0.2)) +
  scale_y_discrete(expand = expansion(mult = 5)) +
  facet_wrap(~ term, scales = "free_x")



# visualizar las posteriors
as_draws_df(fit1.b) %>% 
  pivot_longer(starts_with("b_")) %>% 
  # plot!
  ggplot(aes(x = value)) + 
  # geom_density(fill = "grey20") +
  geom_histogram(bins = 40) +
  facet_wrap(~ name, scales = "free")


as_draws_df(fit1.b) %>% 
  pivot_longer(starts_with("b_")) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            ll = quantile(value, probs = 0.025),
            ul = quantile(value, probs = 0.975))

fixef(fit1.b)

plot(fit1.b)
pairs(fit1.b)

pairs(fit1.b, off_diag_args = list(size = 1/4, alpha = 1/4))
vcov(fit1.b) 
vcov(fit1.b, correlation = TRUE)  # correlacion



vcov(fit1.f)
cov_xy <- vcov(fit1.f)[2, 1]  # covarianza ente intercept y pendiente
var_x  <- vcov(fit1.f)[1, 1]  # varianza para intercept
var_y  <- vcov(fit1.f)[2, 2]  # varianza para pendiente

# correlacion
cov_xy / (sqrt(var_x) * sqrt(var_y))


draws <- as_draws_df(fit1.b)

draws <- draws %>% 
  mutate(beta0 = b_Intercept,
         beta1 = b_body_mass_g)

draws %>% 
  select(.draw, beta0, beta1) %>% 
  mutate(body_mass_g = mean(chinstrap$body_mass_g)) %>% 
  mutate(y_hat = beta0 + beta1 * body_mass_g) %>% 
  ggplot(aes(x = y_hat)) +
  geom_histogram(bins = 40) +
  labs(x = expression(hat(italic(y))*'|'*italic(x)==3733.1)) +
  coord_cartesian(xlim = c(47, 51))


# F
predict(fit1.f,
        newdata = tibble(body_mass_g = mean(chinstrap$body_mass_g)),
        interval = "confidence") %>% 
  data.frame() %>% 
  
  ggplot(aes(x = fit, xmin = lwr, xmax = upr, y = 0)) +
  geom_pointrange() +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(hat(italic(y))*'|'*italic(x)==3733.1)) +
  coord_cartesian(xlim = c(47, 51))


draws %>% 
  mutate(body_mass_g = mean(chinstrap$body_mass_g)) %>% 
  mutate(y_hat = beta0 + beta1 * body_mass_g) %>% 
  ggplot(aes(x = y_hat)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  # scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Bayesians have posterior distributions",
       x = expression(hat(italic(y))*'|'*italic(x)==3733.1)) +
  coord_cartesian(xlim = c(47, 51))



draws %>% 
  mutate(body_mass_g = mean(chinstrap$body_mass_g)) %>% 
  mutate(y_hat = beta0 + beta1 * body_mass_g) %>% 
  ggplot(aes(x = y_hat)) +
  # note the changes to this line
  stat_halfeye(point_interval = median_qi, .width = c(.5, .99)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(hat(italic(y))*'|'*italic(x)==3733.1)) +
  coord_cartesian(xlim = c(47, 51))



points <- draws %>% 
  rename(`beta[0]` = beta0,
         `beta[1]` = beta1) %>% 
  pivot_longer(cols = c(`beta[0]`, `beta[1]`, sigma), 
               names_to = "parameter") %>% 
  group_by(parameter) %>% 
  summarise(mean = mean(value),
            median = median(value),
            mode = Mode(value)) %>% 
  pivot_longer(starts_with("m"), names_to = "statistic")



draws %>% 
  rename(`beta[0]` = beta0,
         `beta[1]` = beta1) %>% 
  pivot_longer(cols = c(`beta[0]`, `beta[1]`, sigma), 
               names_to = "parameter") %>% 
  
  ggplot(aes(x = value)) +
  geom_density() +
  geom_vline(data = points,
             aes(xintercept = value, color = statistic),
             size = 3/4) +
  scale_color_viridis_d(option = "A", end = .8) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("parameter space") +
  facet_wrap(~ parameter, labeller = label_parsed, scales = "free", ncol = 1) +
  theme(strip.text = element_text(size = 14))


fixef(fit1.b)          
fixef(fit1.b, robust = TRUE)  # medians


summary(fit1.b, prob = .80)




draws %>% mean_qi(beta0)                      
draws %>% median_qi(beta0, .width = .80)      
draws %>% mode_hdi(beta0, .width = c(.5, .95))


draws %>% 
  slice_sample(n = 100) %>% 
  select(.draw, beta0, beta1) %>% 
  expand_grid(body_mass_g = range(chinstrap$body_mass_g)) %>% 
  mutate(y_hat = beta0 + beta1 * body_mass_g) %>% 
  ggplot(aes(x = body_mass_g, y = y_hat, group = .draw)) +
  geom_line(linewidth = 1/2, alpha = 1/2)


nd <- tibble(body_mass_g = seq(
  from = min(chinstrap$body_mass_g),
  to = max(chinstrap$body_mass_g),
  length.out = 100))


fitted(fit1.b, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  ggplot(aes(x = body_mass_g)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/3) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = chinstrap,
             aes(y = bill_length_mm))

fitted(fit1.b, 
       newdata = nd,
       summary = F) %>% 
  data.frame() %>% 
  set_names(pull(nd, body_mass_g)) %>% 
  mutate(draw = 1:n()) %>% 
  pivot_longer(-draw) %>% 
  mutate(body_mass_g = as.double(name)) %>%
  
  ggplot(aes(x = body_mass_g, y = value)) +
  stat_lineribbon() +
  scale_fill_brewer() +
  coord_cartesian(ylim = range(chinstrap$bill_length_mm)) +
  theme_classic()


fitted(fit1.b, 
       newdata = nd,
       summary = F) %>% 
  data.frame() %>% 
  set_names(pull(nd, body_mass_g)) %>% 
  mutate(draw = 1:n()) %>% 
  pivot_longer(-draw) %>% 
  mutate(body_mass_g = as.double(name)) %>%
  
  ggplot(aes(x = body_mass_g, y = value, fill = after_stat(.width))) +
  stat_lineribbon(.width = ppoints(50)) +
  scale_fill_distiller(limits = 0:1) +
  coord_cartesian(ylim = range(chinstrap$bill_length_mm)) +
  theme_classic()


data.frame() %>% 
  bind_cols(nd) %>% 
  ggplot(aes(x = body_mass_g)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/3) +
  geom_line(aes(y = Estimate)) +
  geom_point(data = chinstrap,
             aes(y = bill_length_mm)) +
  coord_cartesian(ylim = range(chinstrap$bill_length_mm))



predict(fit1.b, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  ggplot(aes(x = body_mass_g)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/3) +
  geom_line(aes(y = Estimate)) +
  # datos
  geom_point(data = chinstrap,
             aes(y = bill_length_mm)) +
  coord_cartesian(ylim = range(chinstrap$bill_length_mm))


f <- fitted(fit1.b, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) 

predict(fit1.b, newdata = nd) %>% 
  data.frame() %>% 
  bind_cols(nd) %>% 
  
  ggplot(aes(x = body_mass_g)) +
  # 95% posterior-predictive range
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4) +
  # 95% conditional mean range
  geom_ribbon(data = f,
              aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/4) +
  # posterior mean of the conditional mean
  geom_line(data = f,
            aes(y = Estimate)) +
  # original data
  geom_point(data = chinstrap,
             aes(y = bill_length_mm)) +
  coord_cartesian(ylim = range(chinstrap$bill_length_mm))




as_draws_df(fit1.b) %>% 
  rename(beta0 = b_Intercept,
         beta1 = b_body_mass_g) %>% 
  select(.draw, beta0, beta1, sigma) %>% 
  slice_sample(n = 50) %>%  
  expand_grid(chinstrap %>% select(body_mass_g)) %>% 
  mutate(bill_length_mm = rnorm(n = n(),
                                mean = beta0 + beta1 * body_mass_g,
                                sd = sigma)) %>% 
  
  ggplot(aes(x = bill_length_mm, group = .draw)) + 
  geom_density(size = 1/4, color = alpha("black", 1/2)) +
  coord_cartesian(xlim = range(chinstrap$bill_length_mm) + c(-2, 2))
