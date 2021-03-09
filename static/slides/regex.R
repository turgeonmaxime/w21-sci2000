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
library(stringr)
# Detect a pattern
str_detect(c("apple", "orange", "pineapple"),
           pattern = "apple")
# Careful: This is case-sensitive
str_detect(c("Apple", "Orange", "Pineapple"),
           pattern = "apple")
# To ignore case
str_detect(c("Apple", "Orange", "Pineapple"),
           pattern = regex("apple", 
                           ignore_case = TRUE))


## -----------------------------------------------------------------------------
# Match one of the patterns
str_detect(c("color", "colour", "coulour"),
           pattern = "color|colour")
# Or more compact
str_detect(c("color", "colour", "coulour"),
           pattern = "col(o|ou)r")


## -----------------------------------------------------------------------------
# Replace patterns
str_replace_all(c("Male", "male", "MaLe"),
                pattern = regex("male", 
                                ignore_case = TRUE),
                replacement = "male")


## -----------------------------------------------------------------------------
# Split a string using a pattern
str_split(c("it is a sentence", "it is another one"),
          pattern = " ")


## -----------------------------------------------------------------------------
# This doesn't work...
str_detect(c("$15.99", "$3.75", "1.99$"), 
           pattern = "^$")
# But this does!
str_detect(c("$15.99", "$3.75", "1.99$"), 
           pattern = "^\\$")


## -----------------------------------------------------------------------------
# Matching on prices that start or end with dollar sign
# Use logical operator
str_detect(c("$15.99", "$3.75", "1.99$"), 
           pattern = "^\\$") |
  str_detect(c("$15.99", "$3.75", "1.99$"), 
           pattern = "\\$$")


## -----------------------------------------------------------------------------
# Or more compact
str_detect(c("$15.99", "$3.75", "1.99$"), 
           pattern = "^\\$|\\$$")


## -----------------------------------------------------------------------------
# Revisiting an earlier example
str_detect(c("color", "colour", "coulour"),
           pattern = "colou?r")

# Matching strings that end with a bunch of periods
str_detect(c("str", "str.", "str..", "str..."),
           pattern = "\\.+$")


## -----------------------------------------------------------------------------
str_detect(c("string.", "string..", "string..."),
           pattern = "\\.{3}$")
# Be careful: a string with 4 dots will also match
str_detect("string....", pattern = "\\.{3}$")


## -----------------------------------------------------------------------------
# Split a sentence into words
str_split("The fox ate a berry.", "\\b")
str_split("The fox ate a berry.", "\\s")


## -----------------------------------------------------------------------------
# Trim white space
str_replace_all("Is  this     enough?", 
                pattern = "\\s+", 
                replacement = " ")


## -----------------------------------------------------------------------------
str_replace_all(" Is this enough?   ",
                pattern = "(^\\s+|\\s+$)",
                replacement = "")


## -----------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
glimpse(movielens)


## -----------------------------------------------------------------------------
# How many horror movie reviews
movielens %>% 
  filter(str_detect(genres, "Horror")) %>% 
  nrow()


## -----------------------------------------------------------------------------
# What proportion are about thrillers?
movielens %>% 
  mutate(thriller = str_detect(genres,
                               "Thriller")) %>% 
  summarise(prop = mean(thriller))


## -----------------------------------------------------------------------------
# What genre is Forrest Gump?
movielens %>% 
  filter(str_detect(title, "Gump")) %>% 
  pull(genres) %>% 
  unique %>%
  str_split(pattern = "\\|")


## -----------------------------------------------------------------------------
library(tidyverse)
library(dslabs)
library(stringr)

reported_heights %>% 
  filter(str_detect(height, "^\\d$")) %>% 
  count(height)


## -----------------------------------------------------------------------------
data_clean1 <- reported_heights %>% 
  filter(!height %in% c(0, 1)) %>% # Remove mistakes
  filter(str_detect(height, "^\\d$")) %>% 
  mutate(height_in = if_else(height == "2",
                             78.75,
                             12 * as.numeric(height)))
nrow(data_clean1)


## -----------------------------------------------------------------------------
reported_heights %>% 
    filter(!str_detect(height, "^\\d{1,2}$")) %>%
  head()

