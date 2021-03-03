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

library(tidyverse)
glimpse <- purrr::partial(glimpse, width = 50)


## ----message = FALSE----------------------------------------------------------
library(tidyverse)

df_beers <- read_csv("beers.csv")
df_breweries <- read_csv("breweries.csv")

glimpse(df_beers)
glimpse(df_breweries)


## -----------------------------------------------------------------------------
library(tidyverse)

dataset <- inner_join(df_beers,
                      df_breweries,
                      by = "brewery_id")
glimpse(dataset)


## -----------------------------------------------------------------------------
# dataset and df_beers have the same # of rows
nrow(dataset) == nrow(df_beers)
# dataset has one less than the sum of # cols
c(ncol(dataset), ncol(df_beers), ncol(df_breweries))


## -----------------------------------------------------------------------------
dataset %>% 
  group_by(state) %>% 
  summarise(avg_abv = mean(abv)) %>% 
  filter(avg_abv == max(avg_abv))


## -----------------------------------------------------------------------------
# What went wrong?
# Let's look at the data
dataset %>% 
  filter(is.na(abv)) %>% 
  glimpse


## -----------------------------------------------------------------------------
# NA in abv trickles down to the average
dataset %>% 
  group_by(state) %>% 
  summarise(avg_abv = mean(abv))


## -----------------------------------------------------------------------------
# Solution: na.rm = TRUE in mean
dataset %>% 
  group_by(state) %>% 
  summarise(avg_abv = mean(abv, na.rm = TRUE)) %>% 
  filter(avg_abv == max(avg_abv))


## -----------------------------------------------------------------------------
library(nycflights13)

# Information about flights
glimpse(flights)
# Information about airplanes
glimpse(planes)


## -----------------------------------------------------------------------------
# How many flights? How many planes?
c(nrow(flights), nrow(planes))

# How many flights have matching plane?
inner_join(flights, planes, by = "tailnum") %>% 
  nrow


## -----------------------------------------------------------------------------
# With left_join, we keep all flights
left_join(flights, planes, by = "tailnum") %>% 
  glimpse


## -----------------------------------------------------------------------------
# First we combine the two datasets 
# and create tot_delay variable
# Note: Could also use inner_join
dataset <- left_join(flights,        
                     planes,
                     by = "tailnum") %>% 
  mutate(tot_delay = dep_delay + arr_delay)


## ----error = TRUE-------------------------------------------------------------
# Next group by year and summarise
data_avg <- dataset %>% 
  group_by(year) %>% 
  summarise(avg_delay = mean(tot_delay, na.rm = TRUE))


## -----------------------------------------------------------------------------
# What happened?
# Both flights and planes have a variable year
# year.y refers to the one from planes
names(dataset)


## -----------------------------------------------------------------------------
# Try again
data_avg <- dataset %>% 
  group_by(year.y) %>% 
  summarise(avg_delay = mean(tot_delay, na.rm = TRUE))


## ----message = FALSE, warning = FALSE-----------------------------------------
data_avg %>% 
  ggplot(aes(x = year.y,
             y = avg_delay)) +
  geom_point()

