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


## ---- message=FALSE-----------------------------------------------------------
library(tidyverse)
library(dslabs)

dim(olive)

# Create histogram for oleic acid
ggplot(olive,
       aes(x = oleic)) + 
  geom_histogram()


## ----hist_fill, message=FALSE-------------------------------------------------
# Look at distribution by region
ggplot(olive,
       aes(x = oleic, fill = region)) + 
  geom_histogram()


## ----hist_dodge, message=FALSE------------------------------------------------
# Dodge instead of stack
ggplot(olive,
       aes(x = oleic, fill = region)) + 
  geom_histogram(position = "dodge")


## ----hist_facet, message=FALSE------------------------------------------------
# Or with facets
ggplot(olive, 
       aes(x = oleic)) +
  geom_histogram() +
  facet_grid(. ~ region)


## ----message = FALSE----------------------------------------------------------
# Create a copy of the data to serve as background
olive_bg <- select(olive, -region)
ggplot(olive, aes(x = oleic)) + 
  # Start with grey background
  geom_histogram(data = olive_bg, 
                 fill = 'grey') +
  # Add colour on top
  geom_histogram(aes(fill = region)) +
  facet_grid(. ~ region) +
  # Move legend to top
  theme(legend.position = 'top')


## ---- message = FALSE---------------------------------------------------------
library(tidyverse)
library(openintro)

ggplot(nba_players_19, aes(height)) +
  geom_histogram()


## -----------------------------------------------------------------------------
nba_players_19 %>% 
  filter(position %in% c("Center", "Guard")) %>% 
  ggplot(aes(height)) +
  geom_histogram() +
  facet_grid(~position)


## ---- message=FALSE-----------------------------------------------------------
ggplot(olive, aes(x = oleic)) + 
  geom_density()

# Split by region
ggplot(olive, aes(x = oleic,
                  fill = region)) + 
  geom_density()

# Add transparency
ggplot(olive, aes(x = oleic,
                  fill = region)) + 
  geom_density(alpha = 0.5)

# Alternative: stacked density plots
ggplot(olive, aes(x = oleic,
                  fill = region)) + 
  geom_density(position = "stack")


## ---- message=FALSE-----------------------------------------------------------
ggplot(olive, aes(x = oleic)) + 
  geom_boxplot(y = 0) # y = 0 is a dummy value

# Map region to y-axis
ggplot(olive, aes(x = oleic, 
                  y = region)) + 
  geom_boxplot()

# Add all points on top of boxplots
ggplot(olive, aes(x = oleic, 
                  y = region)) + 
  geom_boxplot() +
  geom_point()

# Add vertical noise to the points to reduce overlap
# Note: need to remove outliers or you will get 
#       duplicates
ggplot(olive, aes(x = oleic, 
                  y = region)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(height = 0.25, width = 0) 

# Flip boxplots by switching the axes
ggplot(olive, aes(x = region, 
                  y = oleic)) + 
  geom_boxplot()


## -----------------------------------------------------------------------------
ggplot(nba_players_19, aes(x = position,
                           y = height)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(height = 0, width = 0.25)


## -----------------------------------------------------------------------------
ggplot(stars, aes(x = magnitude, 
                  y = temp)) + 
  geom_point()

# Add colour for type of stars
ggplot(stars, aes(x = magnitude, 
                  y = temp,
                  colour = type)) + 
  geom_point()


## ---- echo = FALSE, eval = FALSE----------------------------------------------
## library(scatterplot3d)
## 
## greenhouse_gases %>%
##   spread(gas, concentration) %>%
##   with(scatterplot3d(CH4,   # x axis
##                      CO2,   # y axis
##                      N2O    # z axis
## ))


## -----------------------------------------------------------------------------
library(tidyverse)
library(openintro)

ggplot(babies_crawl, aes(x = temperature, 
                         y = avg_crawling_age)) +
  geom_point()


## -----------------------------------------------------------------------------
# First option
# Restrict the data before plotting
babies_crawl %>% 
    filter(temperature > 30, temperature < 70) %>% 
    ggplot(aes(x = temperature,
               y = avg_crawling_age)) +
    geom_point()


## ----linewidth = 50-----------------------------------------------------------
# Second option
# xlim removes the points from the plot
ggplot(babies_crawl, aes(x = temperature,
                         y = avg_crawling_age)) +
    geom_point() +
    xlim(c(30, 70))


## -----------------------------------------------------------------------------
# Third option
# coord_cartesian zooms in/out 
ggplot(babies_crawl, aes(x = temperature,
                         y = avg_crawling_age)) +
    geom_point() +
    coord_cartesian(xlim = c(30, 70))


## ----eval = FALSE-------------------------------------------------------------
## ggplot(stars, aes(x = magnitude,
##                   y = temp)) +
##   geom_density_2d()
## 
## # We can add points on top of the contour lines
## ggplot(stars, aes(x = magnitude,
##                   y = temp)) +
##   geom_density_2d() +
##   geom_point()
## 
## # We can colour points by star type
## # Note: colour is only defined for geom_point
## ggplot(stars, aes(x = magnitude,
##                   y = temp)) +
##   geom_density_2d() +
##   geom_point(aes(colour = type))


## ---- message = FALSE---------------------------------------------------------
library(GGally)

# Select three variables
olive_sub <- olive %>% 
  select(eicosenoic, arachidic, linolenic)

ggpairs(olive_sub)

