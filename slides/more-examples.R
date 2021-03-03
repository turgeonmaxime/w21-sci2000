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
library(tidyverse)
# Import dataset into R
data_fev <- read_csv("FEV.csv")
glimpse(data_fev, width = 50) # So it fits the slide

# Explore data
ggplot(data_fev, aes(x = sex, y = fev)) +
  geom_boxplot()
ggplot(data_fev, aes(x = smoke, y = fev)) +
  geom_boxplot()


## -----------------------------------------------------------------------------
# Smokers have higher FEV??
fit <- lm(fev ~ smoke, data = data_fev)
coef(fit)
confint(fit)


## ---- message = FALSE---------------------------------------------------------
# Look at FEV vs age and height
ggplot(data_fev, aes(x = age, y = fev)) +
  geom_point()
ggplot(data_fev, aes(x = height, y = fev)) +
  geom_point()


## -----------------------------------------------------------------------------
# Fit linear model
model <- lm(fev ~ smoke + sex + age + height, 
            data = data_fev)


## -----------------------------------------------------------------------------
coef(model)


## -----------------------------------------------------------------------------
confint(model)


## -----------------------------------------------------------------------------
library(broom)
# Plot outcome vs fitted values
augment(model) %>% 
  ggplot(aes(x = .fitted, y = fev)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1)


## ----eval = FALSE-------------------------------------------------------------
## # Can also colour points according to smoking status
## augment(model) %>%
##   ggplot(aes(x = .fitted, y = fev)) +
##   geom_point(aes(colour = smoke)) +
##   geom_abline(intercept = 0,
##               slope = 1)


## -----------------------------------------------------------------------------
# Plot residuals vs fitted values
augment(model) %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dotted")


## -----------------------------------------------------------------------------
# We will add a quadratic term for height
# The function I() protects height^2
# Try removing it from the code below and 
# see how it's different
model2 <- lm(fev ~ smoke + sex + age + 
               height + I(height^2), 
            data = data_fev)


## -----------------------------------------------------------------------------
coef(model2)


## ----linewidth = 60-----------------------------------------------------------
confint(model2)


## -----------------------------------------------------------------------------
augment(model2) %>% 
  ggplot(aes(x = .fitted, y = fev)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1)


## -----------------------------------------------------------------------------
augment(model2) %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dotted")


## ----linewidth = 60-----------------------------------------------------------
# Let's use robust standard errors
library(lmtest)
library(sandwich)
coefci(model2, vcov. = vcovHC(model2))

