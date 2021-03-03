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


## ---- message = FALSE---------------------------------------------------------
library(DAAG)
library(tidyverse)

# Fit model
fit <- lm(magnetic ~ chemical, data = ironslag)
confint(fit)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
ggplot(ironslag, aes(x = chemical, 
                     y = magnetic)) +
  geom_point() + 
  geom_abline(intercept = coef(fit)[1],
              slope = coef(fit)[2])


## -----------------------------------------------------------------------------
library(broom)

# Augment adds fitted values and residuals
# to the original data
names(augment(fit))


## -----------------------------------------------------------------------------
# Fitted against residuals
augment(fit) %>% 
  ggplot(aes(x = .fitted,
             y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed")


## ----message = FALSE----------------------------------------------------------
library(MASS)
library(tidyverse)

dataset <- mutate(mammals,
                  log_body = log(body))

# Fit model
fit <- lm(brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
ggplot(dataset, aes(x = log_body, 
                    y = brain)) +
  geom_point() + 
  geom_abline(intercept = coef(fit)[1],
              slope = coef(fit)[2])


## -----------------------------------------------------------------------------
names(augment(fit))


## -----------------------------------------------------------------------------
# Fitted against residuals
augment(fit) %>% 
  ggplot(aes(x = .fitted,
             y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed")


## ----message = FALSE----------------------------------------------------------
dataset <- mutate(mammals,
                  log_body = log(body),
                  log_brain = log(brain))

# Fit model
fit2 <- lm(log_brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit2)


## -----------------------------------------------------------------------------
# Plot fitted linear trend
ggplot(dataset, aes(x = log_body, 
                    y = log_brain)) +
  geom_point() + 
  geom_abline(intercept = coef(fit2)[1],
              slope = coef(fit2)[2])


## -----------------------------------------------------------------------------
names(augment(fit2))


## -----------------------------------------------------------------------------
# Fitted against residuals
augment(fit2) %>% 
  ggplot(aes(x = .fitted,
             y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed")


## ---- message = FALSE---------------------------------------------------------
library(DAAG)
library(tidyverse)

# Fit model
fit <- lm(magnetic ~ chemical, data = ironslag)
confint(fit)


## ----message = FALSE----------------------------------------------------------
library(lmtest)
library(sandwich)
coefci(fit, vcov. = vcovHC(fit))


## ----message = FALSE----------------------------------------------------------
dataset <- mutate(mammals,
                  log_body = log(body),
                  log_brain = log(brain))

# Fit model
fit2 <- lm(log_brain ~ log_body, data = dataset)


## ----message = FALSE----------------------------------------------------------
confint(fit2)
coefci(fit2, vcov. = vcovHC(fit2))

