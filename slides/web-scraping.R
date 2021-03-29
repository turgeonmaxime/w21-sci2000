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
resource <- GET("https://www.ecosia.org")
resource$status_code
http_status(resource)


## -----------------------------------------------------------------------------
# What did we receive?
resource$headers$`content-type`
content(resource)


## -----------------------------------------------------------------------------
# What about images?
url <- paste0("https://www.maxturgeon.ca/", 
              "figure/posts/oscar2019_bestPic.png")
image <- GET(url)
image$status_code
image$headers$`content-type`
str(content(image))


## -----------------------------------------------------------------------------
library(tidyverse)
content(resource, as = "text") %>%
  str_extract("<title>[\\w\\s-]*</title>")


## -----------------------------------------------------------------------------
# List of pharaohs in tables
# en.wikipedia.org/wiki/List_of_pharaohs
GET("en.wikipedia.org/wiki/List_of_pharaohs") %>% 
  content(as = "text") %>% 
  str_extract_all("<table>")


## -----------------------------------------------------------------------------
# List of pharaohs in tables
# en.wikipedia.org/wiki/List_of_pharaohs
GET("en.wikipedia.org/wiki/List_of_pharaohs") %>% 
  content(as = "text") %>% 
  str_extract_all("<table class=\"wikitable\".*>") %>% 
  str()


## -----------------------------------------------------------------------------
library(rvest)

read_html("https://www.ecosia.org") %>% 
  html_elements("title") %>% 
  html_text() # Get text from element


## -----------------------------------------------------------------------------
# Syntax: tag.class
url <- "https://en.wikipedia.org/wiki/List_of_pharaohs"
tables <- read_html(url) %>% 
  html_elements("table.wikitable")
length(tables)


## -----------------------------------------------------------------------------
# Let's look at first one
tables[[1]]

html_table(tables[[1]]) %>% 
  glimpse


## -----------------------------------------------------------------------------
library(rvest)
url <- paste0("https://www.macleans.ca/education/",
              "uniandcollege/top-10-highest-paid-",
              "university-officials-in-canada/")
data <- read_html(url) %>% 
  html_elements("div.single-article-text") %>% 
  html_elements("p") %>% 
  html_text() 


## -----------------------------------------------------------------------------
library(tidyverse)
data <- data.frame(text = data)
glimpse(data)


## -----------------------------------------------------------------------------
data_clean <- data %>% 
  filter(str_detect(text, "^\\d{1,2}\\.")) %>% 
  separate(col = "text", into = c("name", "salary"), 
           sep = "\\n") %>% 
  mutate(name = str_replace(name, 
                            "^\\d{1,2}\\.\\s*", ""), 
         salary = str_extract(salary, "\\$[\\d,]+$"))
glimpse(data_clean)


## -----------------------------------------------------------------------------
library(knitr)
kable(data_clean)

