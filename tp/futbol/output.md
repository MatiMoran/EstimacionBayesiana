---
title: "TP Final Estimacion Bayesiana"
subtitle: "Matias Moran y Matias Gangui"
output: html_document
---

En este trabajo final vamos a utilizar tecnicas de estimacion bayesiana vistas en la materia para poder obtener y analizar informacion sobre como se comportan los precios de mercado de los jugadores de futbol a nivel internacional.

El dataset que usamos consiste del precio historico de mercado de **8572** jugadores de futbol que alguna vez jugaron algun partido en las competiciones mas importantes del mundo (UCL, Serie A, La Liga, Premier y Bundesliga).

El dataset esta basado en una [recopilacion](https://www.kaggle.com/datasets/davidcariboo/player-scores) de datos de la pagina **transfermarkt** la cual contiene informacion historica de ligas, equipos y jugadores de futbol de todo el mundo.

# Setup

Importamos las librerias


``` r
# Cargamos los paquetes
library(bayesrules)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ───────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(rstanarm)
```

```
## Loading required package: Rcpp
## This is rstanarm version 2.32.1
## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!
## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.
## - For execution on a local, multicore CPU with excess RAM we recommend calling
##   options(mc.cores = parallel::detectCores())
```

``` r
library(bayesplot)
```

```
## This is bayesplot version 1.11.1
## - Online documentation and vignettes at mc-stan.org/bayesplot
## - bayesplot theme set to bayesplot::theme_default()
##    * Does _not_ affect other ggplot2 plots
##    * See ?bayesplot_theme_set for details on theme setting
```

``` r
library(tidybayes)
library(broom.mixed)
library(RColorBrewer)
```

Cargamos el dataset y hacemos feature engineering y reescalamos


``` r
# Cargamos el dataset
football = read.csv("players_age_valuation.csv", header = TRUE)

football$player_id <- as.character(football$player_id)

football <- football %>%
  mutate(age_2 = age^2)
football <- football %>% 
  mutate(log_market_value_in_eur = log(market_value_in_eur))


head(football)
```

```
##   player_id               name age market_value_in_eur age_2 log_market_value_in_eur
## 1      6893      Gabriel Tamas  20              900000   400                13.71015
## 2        10     Miroslav Klose  26             7000000   676                15.76142
## 3        26 Roman Weidenfeller  24             1500000   576                14.22098
## 4        65   Dimitar Berbatov  23             8000000   529                15.89495
## 5        77              Lúcio  26            13000000   676                16.38046
## 6        80         Tom Starke  23              400000   529                12.89922
```

# Data Exploration

Primero que nada vamos a ver la data de los jugadores para poder entender mejor el problema y poder armar el modelo

## Distribucion de market value


``` r
players_stats <- football %>%
  group_by(player_id) %>%
  summarize(
    max_value = max(log_market_value_in_eur, na.rm = TRUE),
    median_value = median(log_market_value_in_eur, na.rm = TRUE),
    mean_value = mean(log_market_value_in_eur, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(max_value, median_value, mean_value), 
               names_to = "measure", 
               values_to = "log_market_value")

ggplot(data = players_stats, aes(x = log_market_value, fill = measure)) +
  geom_histogram(color = "black", alpha = 0.7, position = "dodge") +
  facet_wrap(~ measure, scales = "free", ncol = 1) +
  ggtitle("Distribución de Valor de Mercado por Jugador (Máximo, Mediana, Promedio)") +
  labs(x = "Log Valor de Mercado (en EUR)", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Esto nos da un buen insight de que el logaritmo del valor de cada jugador se distribuye de una forma normal.


``` r
set.seed(314159)

players_with_history = football %>%
  filter(age %in% c(20, 35)) %>%
  group_by(player_id) %>%
  filter(any(age == 20) & any(age == 35)) %>%
  pull(player_id) %>%
  unique()

cracks = football[football$player_id %in% head(players_with_history, 50),]

player_ids = c(28003, 8198, 68290, 342229)
cracks = rbind(cracks, football[football$player_id %in% player_ids,])

cracks_10 = football[football$player_id %in% head(players_with_history, 10),]


ggplot(data = cracks_10, aes(age, market_value_in_eur, color = name)) +
  ggtitle("Precio de mercado historico por jugador (N = 10)") +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position = "none") +
  labs(x="Edad", y="Valor (EUR)") +
  scale_y_continuous(labels = scales::label_dollar()) +
  lims(y = c(0,40000000))
```

```
## Scale for y is already present.
## Adding another scale for y, which will replace the existing scale.
## `geom_smooth()` using formula = 'y ~ x'
```

```
## Warning: Removed 9 rows containing missing values or values outside the scale range (`geom_smooth()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

``` r
ggplot(data = cracks_10, aes(age, log_market_value_in_eur, color = name)) +
  ggtitle("Log-Precio de mercado historico por jugador (N = 10)") +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position = "none") +
  labs(x="Edad", y="Log-Valor (EUR)") +
  scale_y_continuous(labels = scales::label_dollar()) +
  lims(y = c(10,18))
```

```
## Scale for y is already present.
## Adding another scale for y, which will replace the existing scale.
## `geom_smooth()` using formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

``` r
ggplot(cracks_10, aes(x = age, y = log_market_value_in_eur)) +
  geom_point() +
  facet_wrap(~ player_id) +
  ggtitle("Plot de 10 jugadores distintos")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png)

# Modelado

Dada la forma de los datos pensamos es poder predecir el logaritmo del precio de mercado de un jugador en base solamente a su edad, vamos a hacer un modelo jerarquico a nivel de jugador y vamos a tratar de modelar una relacion cuadratica y vamos a asignar a cada jugador su propio intercept y haremos que el intercept de cada jugador este normalmente distribuido como vimos en el grafico anterior.


``` r
set.seed(314159)

quadratic_fun <- function(x, a, b, c) {
  return(a*x^2 + b*x + c)
}

ggplot(data = cracks_10, aes(age, log_market_value_in_eur, color = name)) +
  ggtitle("Log-Precio de mercado historico por jugador (N = 10) vs Regresion") +
  geom_smooth(method = "loess", se = FALSE) +
  geom_line(data = data.frame(age = seq(min(cracks_10$age), max(cracks_10$age), length.out = 100)), 
            aes(x = age, y = quadratic_fun(age, -0.05, 2.7, -19)), color = "black", size = 1.5) +
  theme(legend.position = "none") +
  labs(x="Edad", y="Log-Valor (EUR)") +
  scale_y_continuous(labels = scales::label_dollar())
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Vemos la funcion cuadratica que vemos que puede llegar a aproximar bien a los datos $y = -19 + 2.7x -0.05 x^2$

## Definicion:

$$
\text{data:} \quad Y_{{ij}}|\beta_{{0j}}, \beta_{{1}} ,\sigma_{y} \sim \mathcal{{N}}(\mu_{{ij}}, \sigma_{y}^{2}) \quad  \text{with} \quad  \mu_{{ij}} = \beta_{{0j}} + \beta_{{1}} \cdot X_{{ij}} + \beta_{{2}} \cdot X_{ij}^{2}
$$

$$
\text{priors:} \quad \beta_{0j}|\beta_{0},\sigma_{0} \sim \mathcal{{N}}(\beta_{0}, \sigma_{0}^{{2}})
$$

$$
\quad \quad \quad \quad \beta_{0c} \sim \mathcal{{N}}(-19, 20^2)
$$

$$
\quad \quad \quad \sigma_{0} \sim Exp(1)
$$

$$
\quad \quad \quad \quad \beta_{{1}} \sim \mathcal{{N}}(2.7, 5^{{2}})
$$

$$
\quad \quad \quad \quad \beta_{{2}} \sim \mathcal{{N}}(-0.05, 0.5^{{2}})
$$

$$
\quad \quad \quad \sigma_{y} \sim Exp(1)
$$





































