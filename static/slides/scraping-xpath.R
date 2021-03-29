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
# From last week----
library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/World_population"
world_pop_tables <- read_html(url) %>% 
  html_elements("table.wikitable") 

length(world_pop_tables)


## -----------------------------------------------------------------------------
# Notice the single and double quotes!
xpath <- '//div[@id="mw-content-text"]/div[1]/table[4]'
exact_table <- read_html(url) %>% 
  html_element(xpath = xpath)

library(knitr)
html_table(exact_table) %>% 
  select(Rank, Country, Population) %>% 
  kable()


## -----------------------------------------------------------------------------
library(rvest)

url <- "https://www.r-project.org/mail.html"
path <- "/html/body/div/div[1]/div[2]/ul[1]/li/p/a"

mail <- read_html(url) %>% 
    html_elements(xpath = path)


## -----------------------------------------------------------------------------
library(tidyverse)

data.frame(
    name = html_text(mail),
    URL = html_attr(mail, "href")
) %>% glimpse


## -----------------------------------------------------------------------------
# Equivalently: once we reach ul[1]
# we want all a elements
path2 <- "/html/body/div/div[1]/div[2]/ul[1]//a"

mail2 <- read_html(url) %>% 
    html_elements(xpath = path)

# Are they the same?
all.equal(mail, mail2)


## -----------------------------------------------------------------------------
library(tidyverse)
url <- "https://coinmarketcap.com/all/views/all/"
path <- paste0('//div[@class="cmc-table__',
               'table-wrapper-outer"]/div/table')

data <- read_html(url) %>% 
    html_element(xpath = path) %>% 
    html_table()
glimpse(data)


## -----------------------------------------------------------------------------
# Clean up Price so we can order it
# NOTE: there's a column name missing
# which can cause some weird errors
library(stringr)
data %>% 
    select(Name, Symbol, Price) %>% 
    mutate(Price = str_replace_all(Price, "\\$|,", ""),
           Price = as.numeric(Price)) %>% 
    top_n(5, Price)


## -----------------------------------------------------------------------------
url <- paste0("https://en.wikipedia.org/wiki/",
              "List_of_cognitive_biases")
path <- "//div/table[2]//tr/td[1]/a"

list_links <- read_html(url) %>% 
  html_elements(xpath = path) %>% 
  html_attr("href")


## -----------------------------------------------------------------------------
str(list_links)


## -----------------------------------------------------------------------------
# It's a good idea to test before looping
path_ref <- '//ol[@class="references"]//cite'

paste0("https://en.wikipedia.org/", 
       list_links[1]) %>% 
  read_html() %>% 
  html_elements(xpath = path_ref) %>% 
  html_text() %>% 
  str()


## -----------------------------------------------------------------------------
# Write a function
extract_refs <- function(url) {
  paste0("https://en.wikipedia.org/", 
         url) %>% 
  read_html() %>% 
  html_elements(xpath = path_ref) %>% 
  html_text()
}


## -----------------------------------------------------------------------------
# Double check
extract_refs(list_links[1]) %>% 
  str


## ----ref-extract, cache = TRUE------------------------------------------------
# Loop over all links----
library(purrr)

full_refs <- map(list_links, 
                 extract_refs)

# full_refs is a list
length(full_refs)

