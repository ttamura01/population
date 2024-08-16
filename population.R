setwd("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp")
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(scales)

## download data from csv file
population <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=POPTHM&scale=left&cosd=1959-01-01&coed=2024-05-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-07-10&revision_date=2024-07-10&nd=1959-01-01") %>% 
  rename(date = DATE, population = POPTHM) 
population

## familiarize the dataset
str(population)
summary(population)

population %>% 
  ggplot(aes(x = population)) +
  geom_histogram()

population %>% 
  ggplot(aes(x = date, y = population)) +
  geom_line()

## extract the initial and latest data
# initial data
initial_data <- population %>% 
  slice_min(date)

initial_date <- initial_data$date
initial_population <- round(initial_data$population)
initial_population_label <- format(initial_population, big.mark =",")
# initial_population_label <- comma(initial_population)

initial_year <- year(initial_date)
# initial_month <- month(initial_date)
# initial_day <- day(initial_date)

# latest data
latest_data <- population %>% 
  slice_max(date)

latest_date <- latest_data$date
latest_population <- latest_data$population
latest_population_label <- format(round(latest_population), big.mark = ",")
# latest_population_label <- comma(round(latest_population))

# find population growth over the interval 
multiple <- latest_population/initial_population
multiple <- round(multiple, 2)

# find interval years 
years <- interval(initial_date, latest_date)/years(1)
years <- round(years)

# annualized growth rate
# (gdp_multiple)^(1/years) = (x + 1)
growth_rate <-  round(((multiple)^(1/years) - 1)*100, 2)

## line plot of quaterly data/gdp
population %>% 
  ggplot(aes(x = date, y = population)) +
  geom_line() +
  geom_text(data=latest_data, aes(x = date, y = population, label = latest_population_label, vjust = -0.3), color = "blue") +
  scale_y_continuous(
    limits = c(160000, 350000),
    breaks = seq(160000, 350000, 20000),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US population grew {multiple}x to {latest_population_label},000 past for {years} years from {initial_population_label},000 in {initial_year} (= annual growth rate at {growth_rate}%)"),
       x = NULL,
       y = "Population (x1,000)",
       caption = "source: FRED (Federal Reserve Economic Data") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"), 
    legend.position = "none"
  ) 

ggsave("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp/figures/US_population_grwoth.png", width = 6, height = 4)

