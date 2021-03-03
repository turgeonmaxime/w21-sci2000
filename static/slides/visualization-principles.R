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


## ---- eval = TRUE, echo = FALSE-----------------------------------------------
library(tidyverse)
library(dslabs)
library(ggrepel)
library(cowplot)

west <- c("Western Europe", "Northern Europe", "Southern Europe",
          "Northern America", "Australia and New Zealand")

dat <- gapminder %>% 
    filter(year%in% c(2010, 2015) & region %in% west & 
               !is.na(life_expectancy) & population > 10^7) %>% 
    mutate(year = as.factor(year))

plot1 <- dat %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text_repel(data = ~filter(.x, year == 2015),
                    aes(label = country), x = 2,
                    nudge_x = 0.1, direction = "y", 
                    hjust = "left",
                    show.legend = FALSE) +
    xlab("") + ylab("Life Expectancy")

plot2 <- dat %>%
    dplyr::select(country, year, life_expectancy) %>% 
    spread(year, life_expectancy) %>% 
    mutate(diff = `2015` - `2010`) %>% 
    ggplot(aes(`2010`, `2015`)) +
    geom_point(aes(size = diff)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed") +
    geom_text_repel(aes(label = country),
                    show.legend = FALSE) +
    xlab("2010") + ylab("2015") +
    scale_size(name = "Difference")

cowplot::plot_grid(plot2, plot1)


## ----pie1, eval = TRUE, echo = FALSE, message = FALSE-------------------------
library(tidyverse)
# Browser preference
df <- data.frame(
  "Browser" = c("Opera", "Safari", "Firefox", "Chrome", "IE"),
  "Y2000" = c(3, 21, 23, 26, 28),
  "Y2015" = c(2, 22, 21, 29, 27)
)
# Can trick ggplot to make a pie chart:
# Make a bar chart in polar coordinates
ggplot(df, aes(x = "", y = Y2000, fill = Browser)) + 
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) +
  theme_void()


## ----pie3, eval = TRUE, echo = FALSE------------------------------------------
# gather combines Y2000 and Y2015 
# and adds a key column
df_tall <- gather(df, "Year", "Percent", 
                  Y2000, Y2015)
ggplot(df_tall, aes(x = "", y = Percent, fill = Browser)) + 
  geom_bar(stat = "identity") + 
  coord_polar("y", start = 0) +
  facet_grid(~ Year) +
  theme_void()


## ----pie4, eval = TRUE, echo = FALSE------------------------------------------
ggplot(df_tall, aes(x = Browser, y = Percent,
                    fill = Browser)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~Year) +
  theme_minimal() +
  theme(legend.position = "none")


## ----pie5, eval = TRUE, echo = FALSE------------------------------------------
ggplot(df_tall, aes(x = Browser, y = Percent,
                    fill = Year)) + 
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal()


## ----zero1, eval = TRUE, echo = FALSE-----------------------------------------
# Remove Opera from list of browsers
df_tall2 <- filter(df_tall, Browser != "Opera")
# Restrict the range
ggplot(df_tall2, aes(x = Browser, y = Percent,
                     fill = Browser)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~Year) +
  coord_cartesian(ylim = c(20, 30)) +
  theme_minimal()


## ----zero3, eval = TRUE, echo = FALSE-----------------------------------------
library(dslabs)
gapminder %>% 
  filter(year == 2012) %>% # Only keep data from 2012
  ggplot(aes(continent, life_expectancy)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(height = 0) +
  theme_minimal() +
  xlab("") + ylab("Life expectancy (in years)")


## ----reorder1, eval = TRUE, echo = FALSE--------------------------------------
data_murders <- murders %>%
  mutate(murder_rate = total / population * 100000) # Compute rate

data_murders %>%
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() + # For horizontal bars
  theme(axis.text.y = element_text(size = 6)) + # Shrink text
  xlab("")


## ----reorder3, eval = TRUE, echo = TRUE---------------------------------------
library(tidyverse)
library(dslabs)

data_murders %>%
  mutate(murder_rate = total / population * 100000) %>% # Compute rate
  mutate(state = reorder(state, murder_rate)) %>% 
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() + # For horizontal bars
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")


## ----datavizex1-setup, echo = FALSE, eval = TRUE------------------------------
gg1 <- us_contagious_diseases %>% 
  filter(disease == "Measles", year == 1928,
         !state %in% c("Hawaii", "Alaska")) %>% 
  mutate(rate = 10000*count/population) %>% 
  ggplot(aes(state, rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")

gg2 <- us_contagious_diseases %>% 
  filter(disease == "Measles", year == 1928,
         !state %in% c("Hawaii", "Alaska")) %>% 
  mutate(rate = 10000*count/population,
         state = reorder(state, rate)) %>% 
  ggplot(aes(state, rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")
gridExtra::grid.arrange(gg1, gg2, ncol = 2)


## ----compare1, eval = TRUE, echo = TRUE---------------------------------------
library(tidyverse)
library(dslabs)

gapminder %>% 
  filter(year == 2012,
         continent %in% c("Americas", "Europe")) %>%
  ggplot(aes(life_expectancy,
             fill = continent)) +
  geom_histogram()


## ----compare2, eval = TRUE, echo = TRUE---------------------------------------
gapminder %>% 
  filter(year == 2012,
         continent %in% c("Americas", "Europe")) %>%
  ggplot(aes(life_expectancy)) +
  geom_histogram() +
  facet_grid(~ continent)


## ----compare3, eval = TRUE, echo = TRUE---------------------------------------
gapminder %>% 
  filter(year == 2012,
         continent %in% c("Americas", "Europe")) %>%
  ggplot(aes(life_expectancy)) +
  geom_histogram() +
  facet_grid(continent ~ .) # Note: which side of ~ is important!


## -----------------------------------------------------------------------------
gapminder %>% 
  filter(year == 2012,
         continent %in% c("Americas", "Europe")) %>%
  ggplot(aes(continent, life_expectancy)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(height = 0)

