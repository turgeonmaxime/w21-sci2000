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


## -----------------------------------------------------------------------------
library(DBI)
# duckdb is a type of RDBMS
library(duckdb)


## -----------------------------------------------------------------------------
# 1. Create a connection
con <- dbConnect(duckdb())

# This database is actually empty...
# We fill it in with the data from nycflights13
library(nycflights13)
duckdb_register(conn = con, name = "flights", 
                df = flights)


## -----------------------------------------------------------------------------
library(tidyverse)

tbl(con, "flights") %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time, na.rm = TRUE))


## -----------------------------------------------------------------------------
# To turn the output into a data frame
# use collect()
tbl(con, "flights") %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time, na.rm = TRUE)) %>% 
  collect()


## -----------------------------------------------------------------------------
# Let's revisit an example from last week
# where we looked at average delay

# First, add planes table
duckdb_register(conn = con, name = "planes", 
                df = planes)


## ----warning = FALSE----------------------------------------------------------
# Replace data.frames by tbl(con, name)
data_avg <- left_join(tbl(con, "flights"),
                      tbl(con, "planes"),
                      by = "tailnum") %>% 
  mutate(tot_delay = dep_delay + arr_delay) %>% 
  group_by(year.y) %>% 
  summarise(avg_delay = mean(tot_delay)) %>% 
  collect() # Collect at the end for efficiency


## ----message = FALSE, warning = FALSE-----------------------------------------
data_avg %>% 
  ggplot(aes(x = year.y,
             y = avg_delay)) +
  geom_point()

