#load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

#step1 - load data
avg_tem_y <- read.csv("average_temperatures_yearly.csv")
co2_gdp <- read.csv("co2-emissions-and-gdp-per-capita_v2.csv")
life_exp <- read.csv("life_exp.csv")
pop <- read.csv("pop.csv")

#step2 - remove region and world data
filter_co2_gdp <- subset(co2_gdp, !grepl("OWID", code) &
                           !grepl("WB", code))
unique(filter_co2_gdp$code)

#step3 - tidy and bring together the Gapminder datasets
life_exp_long <- pivot_longer(life_exp,
                              cols = -c(geo, name),
                              names_to = "year",
                              values_to = "life_exp")
# Remove X and convert to numeric (数值型)
life_exp_long$year <- as.numeric(sub("X", "", life_exp_long$year))

pop_long <- pivot_longer(pop,
                         cols = -c(geo, name),
                         names_to = "year",
                         values_to = "pop")
pop_long$year <- as.numeric(sub("X", "", pop_long$year))

#filter from 1990 to 2025
filter_life <- life_exp_long[1990 <= life_exp_long$year &
                               life_exp_long$year <= 2025, ]

filter_pop <- pop_long[1990 <= pop_long$year &
                               pop_long$year <= 2025, ]

#join life and population datasets together
join_life_pop <- left_join(filter_pop, filter_life, by = c("geo", "name", "year"))

#geo upper case
join_life_pop$geo <- toupper(join_life_pop$geo)
View(join_life_pop)

#step4 - 4 joined datasets
#change column name
join_life_pop <- rename(join_life_pop, code = geo)
avg_tem_y <- rename(avg_tem_y, code = Code)

#join 3 datasets
joint_dataset <- join_life_pop %>% 
  full_join(filter_co2_gdp, by = c("code", "year")) %>% 
  full_join(avg_tem_y, by = c("code", "year"))

#select columns
final_dataset <- joint_dataset[, c("entity", "code", "year", 
                                   "gdp_per_capita", "co2_emissions_per_capita", 
                                   "world_region_according_to_owid", 
                                   "yearly_temperature_c", 
                                   "pop", "life_exp")]
final_dataset <- rename(final_dataset, 
                        region = world_region_according_to_owid)

#step5 - delete missing data
colnames(final_dataset)
nrow(final_dataset)
summary(final_dataset)

clean_dataset <- final_dataset %>% 
  drop_na(region, gdp_per_capita, co2_emissions_per_capita, life_exp)

one_year <- clean_dataset[clean_dataset$year == 2015, ]

#step6 - factor column and reorder
library(forcats)

one_year$region <- fct_relevel(one_year$region, "Europe")

#step7 - scatter plots
library(patchwork)

#p1 — GDP vs Temperature
p1 <- ggplot(one_year, aes(x = yearly_temperature_c, 
                           y = gdp_per_capita,
                           colour = region)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 130000, by = 10000)) +
                       theme_bw()

#p2 - GDP per capita vs co2 emissions per capita
p2 <- ggplot(one_year, aes(x = co2_emissions_per_capita, 
                           y = gdp_per_capita,
                           colour = region)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 130000, by = 10000)) +
                       theme_bw()

#p3 - GDP per capita vs population
p3 <- ggplot(one_year, aes(x = pop, y = gdp_per_capita,
                           colour = region)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 130000, by = 10000)) +
                       theme_bw()

#p4 - GDP per capita vs life expediency
p4 <- ggplot(one_year, aes(x = life_exp, y = gdp_per_capita,
                           colour = region)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 130000, by = 10000)) +
                       theme_bw()

(p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Regression plots to show predictors vs gdp per capita")

#step8 - plot
library(correlation)
library(see)

one_year %>%
  select(gdp_per_capita, co2_emissions_per_capita, yearly_temperature_c,
         life_exp, pop) %>% 
  correlation() %>% 
  summary() %>% 
  plot(show_data = "points")

#step9 - make a simple linear regression
simple_model <- lm(gdp_per_capita ~ yearly_temperature_c, 
                   data = one_year)
summary(simple_model)

#step10 - make a multiple linear regression
multiple_model <- lm(gdp_per_capita ~ yearly_temperature_c +
                       co2_emissions_per_capita +
                       region +
                       life_exp, data = one_year)
summary(multiple_model)

#step11 -report
library(report)
report(multiple_model)
report(simple_model)

