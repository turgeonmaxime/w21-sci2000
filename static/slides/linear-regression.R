## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

knitr::opts_chunk$set(cache = FALSE, message = FALSE,
                      linewidth = 50)


## ----message = FALSE----------------------------------------------------------
library(tidyverse)

dataset <- read_csv("heart.csv")

# Use function lm
fit <- lm(age ~ 1, data = dataset)
fit


## -----------------------------------------------------------------------------
# This is what we ran last lecture
n <- nrow(dataset)
dataset %>% 
    summarise(avg_age = mean(age),
              sd_age = sd(age)) %>% 
    mutate(lo_bd = avg_age - 1.96*sd_age/sqrt(n),
           up_bd = avg_age + 1.96*sd_age/sqrt(n))


## -----------------------------------------------------------------------------
# To compute confidence interval
# use confint
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(age ~ sex, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
# What if we change the coding 0/1 to female/male?
dataset <- dataset %>% 
    mutate(sex = if_else(sex == 1, "male", "female"))


## -----------------------------------------------------------------------------
fit <- lm(age ~ sex, data = dataset)
fit


## -----------------------------------------------------------------------------
fit <- lm(chol ~ sex, data = dataset)
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(age ~ chol, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(chol ~ age, data = dataset)
fit
confint(fit)


## -----------------------------------------------------------------------------
fit <- lm(chol ~ age, data = dataset)
# Extract coefficient estimates with coef
coef(fit)

ggplot(dataset, aes(x = age, y = chol)) +
  geom_point() +
  geom_abline(intercept = coef(fit)[1],
              slope = coef(fit)[2])


## -----------------------------------------------------------------------------
library(tidyverse)
library(dslabs)

count(olive, region)


## ----linewidth = 60-----------------------------------------------------------
fit <- lm(oleic ~ region, data = olive)
fit


## -----------------------------------------------------------------------------
confint(fit)

