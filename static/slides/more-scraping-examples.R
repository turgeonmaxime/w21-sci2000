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
library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/World_population"
world_pop_tables <- read_html(url) %>% 
  html_nodes("table.wikitable") 

length(world_pop_tables)


## -----------------------------------------------------------------------------
# Looking at each of them one at a time
# E.g. world_pop_tables[[1]], world_pop_tables[[2]]
# We recognize it's the 3rd table
library(knitr)

world_pop_tables[[3]] %>% 
  html_table() %>% 
  select(Rank, Country, Population) %>% 
  kable()


## -----------------------------------------------------------------------------
# Alternatively, if you don't like double brackets
library(purrr)

world_pop_tables %>% 
  pluck(3) %>% 
  html_table() %>% 
  select(Rank, Country, Population) %>% 
  kable()


## -----------------------------------------------------------------------------
library(rvest)
# url for "I Care A Lot" (2020)
url <- "https://www.imdb.com/title/tt9893250/"

read_html(url) %>% 
    html_elements("span[itemprop=ratingValue]") %>% 
    html_text()


## -----------------------------------------------------------------------------
url <- paste0("https://pubmed.ncbi.nlm.nih.gov/",
              "?term=%22Hemodialysis%2C+Home%2F",
              "Adverse+Effects%22%5BMesh%5D")

results <- read_html(url) %>% 
  html_nodes("a") %>% 
  html_attr("href") # Extract the attribute

length(results)


## -----------------------------------------------------------------------------
library(stringr)
# This is empty...
str_subset(results, 
           "https://pubmed.ncbi.nlm.nih.gov/\\d+/")
# But this gives us what we want!
urls <- str_subset(results, "/\\d+/")
urls


## -----------------------------------------------------------------------------
# Add domain to relative URLs
full_urls <- paste0("https://pubmed.ncbi.nlm.nih.gov",
                    urls)
full_urls


## -----------------------------------------------------------------------------
# Let's see how it could work
# with the first url
read_html(full_urls[1]) %>% 
  html_nodes("div.abstract-content") %>% 
  html_text()


## -----------------------------------------------------------------------------
# Let's create a function!
# Input: url
extract_abstract <- function(url) {
  read_html(url) %>% 
    html_nodes("div.abstract-content") %>% 
    html_text() %>% 
    str_replace_all("^\\s+|\\s+$", "")
}


## -----------------------------------------------------------------------------
extract_abstract(full_urls[1])


## -----------------------------------------------------------------------------
# Different ways to loop
# We will use map from package purrr
library(purrr)

abstracts <- map(full_urls,
                 extract_abstract)

str(abstracts)


## -----------------------------------------------------------------------------
library(rvest)

url <- "https://weather.gc.ca/city/pages/mb-36_metric_e.html"
forecast <- read_html(url)

days <- forecast %>% 
  html_elements("div.row strong") %>% 
  html_text()
days


## -----------------------------------------------------------------------------
# Not quite... 
# Let's use regex to remove "Night:"
library(stringr)
days <- forecast %>% 
  html_elements("div.row strong") %>% 
  html_text() %>%
  str_subset("Night:", negate = TRUE)
days


## -----------------------------------------------------------------------------
temps <- forecast %>% 
    html_elements("div.row span.wxo-metric-hide") %>% 
    html_text()
temps


## -----------------------------------------------------------------------------
# We also picked up precipitations!
# And other stuff...
# Better: we want span with title attribute
temps <- forecast %>% 
    html_elements("div.row span.wxo-metric-hide[title]") %>% 
    html_text()
temps


## -----------------------------------------------------------------------------
# Put into nice table with caption
library(knitr)
data.frame(
  Day = days,
  Forecast = temps
) %>% 
  kable(caption = "Weather Forecast for Winnipeg")


## -----------------------------------------------------------------------------
library(rvest)
url <- paste0("https://www.imdb.com/search/title/",
              "?groups=top_250&sort=user_rating")

imdb_page <- read_html(url)

top50_titles <- imdb_page %>% 
  html_elements("h3.lister-item-header a") %>% 
  html_text()
head(top50_titles)


## -----------------------------------------------------------------------------
csssel <- "h3.lister-item-header span.lister-item-year"
top50_years <- imdb_page %>% 
  html_elements(csssel) %>% 
  html_text()
head(top50_years)


## -----------------------------------------------------------------------------
library(tidyverse)

# Combine into tibble
tibble(Movie = top50_titles,
       Year = top50_years) %>% 
  mutate(Year = str_replace_all(Year,
                                "\\(|\\)",
                                ""),
         Year = as.numeric(Year))

