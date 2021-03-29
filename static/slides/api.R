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
library(httr)
# Base URL path
base_url <- paste0("https://data.colorado.gov/",
                   "resource/q5vp-adf3.json?")
full_url <- paste0(base_url, "county=Boulder")

data <- GET(full_url)
status_code(data) # 200 OK

# What did we receive?
data$headers$`content-type`


## -----------------------------------------------------------------------------
library(tidyverse)
library(jsonlite)

data <- fromJSON(content(data, as = "text"))
is.data.frame(data)
names(data)


## ----tidy = TRUE, tidy.opts = list(width.cutoff = I(60))----------------------
# We can filter the data via the URL
full_url <- paste0(base_url, "county=Boulder",
                   "&$where=age between 20 and 40")
# Need to encode to proper URL
full_url <- URLencode(full_url)
# Spaces are replaced by %20
str_sub(full_url, 66)


## -----------------------------------------------------------------------------
data <- GET(full_url)
status_code(data) # 200 OK
data <- fromJSON(content(data, as = "text"))
range(as.numeric(data$age))


## -----------------------------------------------------------------------------
full_url <- paste0(base_url, "county=Boulder",
                  "&$where=age between 20 and 40",
                  "&$select=year,age,femalepopulation")

full_url <- URLencode(full_url)


## -----------------------------------------------------------------------------
data <- GET(full_url)
status_code(data) # 200 OK
data <- fromJSON(content(data, as = "text"))


## ----eval = TRUE--------------------------------------------------------------
library(tidyverse)
# Mutate variables to numeric
data <- data %>% 
  mutate(year = as.numeric(year),
       age = as.integer(age),
       femalepopulation = as.numeric(femalepopulation))

data %>% 
  ggplot(aes(x = year, y = femalepopulation)) +
  geom_line(aes(group = age, colour = age))


## -----------------------------------------------------------------------------
# Retrieve API key from environment variable
token <- Sys.getenv("WINNIPEG_TOKEN")

baseurl <- paste0("https://api.winnipegtransit.com/",
                  "v3/stops.json?")

full_url <- paste0(baseurl, "lon=-97.138&lat=49.895&", 
                   "distance=250&", "api-key=", token)


## -----------------------------------------------------------------------------
data <- GET(full_url)
status_code(data) # 200 OK
data <- fromJSON(content(data, as = "text"))


## -----------------------------------------------------------------------------
names(data)
glimpse(data$stops)
pull(data$stops, name)


## -----------------------------------------------------------------------------
base_url <- paste0("https://api.winnipegtransit.com/",
                   "v3/stops/10541/schedule.json")
full_url <- paste0(base_url, 
                   "?max-results-per-route=2&",
                   "api-key=", token)

data <- GET(full_url)
status_code(data) # 200 OK
data <- fromJSON(content(data, as = "text"))


## -----------------------------------------------------------------------------
library(purrr)
data %>% 
  pluck("stop-schedule", "route-schedules",
        "scheduled-stops", 1, "times", "arrival")

