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


## ----echo = FALSE-------------------------------------------------------------
library(tidyverse)

tibble(p = ppoints(1000)) %>% 
  mutate(logit = log(p/(1-p))) %>% 
  ggplot(aes(x = p, y = logit)) +
  geom_line() +
  theme_minimal() +
  xlab("Probability") +
  ylab("Log odds")


## ----echo = FALSE-------------------------------------------------------------
library(tidyverse)
library(purrr)

expit <- function(t) exp(t)/(1 + exp(t))
xvar <- seq(65, 85)

c(-0.5, 0, 0.5, 1) %>% 
    map_df(function(beta) {
        tibble(height = xvar) %>% 
            mutate(beta = beta,
                   prob = expit(beta*(xvar - 75)))
    }) %>% 
    ggplot(aes(x = height, y = prob)) +
    geom_line() +
    facet_wrap(~beta, labeller = label_both) +
    xlab("Height (in)") + ylab("P(Y = 1 | X)") +
    theme_minimal()


## -----------------------------------------------------------------------------
library(tidyverse)
# Create dataset
dataset <- bind_rows(
  data.frame(Y = rep("right", 43),
             X = rep("male", 43)),
  data.frame(Y = rep("right", 44),
             X = rep("female", 44)),
  data.frame(Y = rep("left", 9),
             X = rep("male", 9)),
  data.frame(Y = rep("left", 4),
             X = rep("female", 4)))


## -----------------------------------------------------------------------------
glimpse(dataset)


## -----------------------------------------------------------------------------
# Outcome must be 0 or 1
dataset <- mutate(dataset, Y = as.numeric(Y=="right"))

glm(Y ~ X, data = dataset,
    family = "binomial")


## -----------------------------------------------------------------------------
# Relationship with odds?
log(11)
log(4.78/11)


## -----------------------------------------------------------------------------
library(Sleuth3)
library(tidyverse)

# First transform outcome to 0/1
dataset <- mutate(case2001,
                  Y = as.numeric(Status == "Died"))

fit <- glm(Y ~ Age, data = dataset,
           family = "binomial")


## -----------------------------------------------------------------------------
coef(fit)


## -----------------------------------------------------------------------------
confint(fit)
exp(confint(fit))


## -----------------------------------------------------------------------------
library(tidyverse)
# Import dataset into R
data_dmd <- read_csv("dmd.csv")
# Remove rows with missing values
data_dmd <- na.omit(data_dmd)


## ----echo = FALSE, eval = FALSE-----------------------------------------------
## # Explore data
## par(mfrow = c(2, 2))
## boxplot(ck ~ carrier, data = data_dmd)
## boxplot(h ~ carrier, data = data_dmd)
## boxplot(pk ~ carrier, data = data_dmd)
## boxplot(ld ~ carrier, data = data_dmd)


## ---- warning = FALSE---------------------------------------------------------
model <- glm(carrier ~ ck + h, data = data_dmd,
             family = "binomial")
confint(model)


## -----------------------------------------------------------------------------
library(broom)
# Plot residuals and probabilities (no binning)
augment(model, type.predict = "response") %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed")


## ---- linewidth = 50----------------------------------------------------------
# We will use the performance package
library(performance)

# By default: residuals vs fitted probs
#             sqrt(n) bins (~14 bins)
binned_residuals(model)


## ---- linewidth = 50----------------------------------------------------------
# Use 'term' to plot against covariate
binned_residuals(model, term = "ck")
binned_residuals(model, term = "h")


## -----------------------------------------------------------------------------
performance_hosmer(model)


## ---- linewidth=50------------------------------------------------------------
performance_score(model) # Quadratic = Brier
performance_roc(model)


## ---- linewidth=50------------------------------------------------------------
performance_pcp(model)

