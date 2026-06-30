# load libraries
library(dplyr)
library(ggplot2)
library(effectsize)

premier_league <- read.csv("https://raw.githubusercontent.com/andrewmoles2/premier_league_tables/refs/heads/master/data/premier_league_tables.csv")

View(premier_league)

selected_premier <- premier_league[premier_league$played == 38, ]

# confirm the filter worked correctly
unique(selected_premier$played)

selected_premier$era <- ifelse(selected_premier$season %in% c("1995-1996", 
                                                               "1996-1997", 
                                                               "1997-1998", 
                                                               "1998-1999", 
                                                               "1999-2000"),
                               "Man United dominate",
                               ifelse(selected_premier$season %in% c("2000-2001", 
                                                                      "2001-2002", 
                                                                      "2002-2003", 
                                                                      "2003-2004", 
                                                                      "2004-2005", 
                                                                      "2005-2006", 
                                                                      "2006-2007", 
                                                                      "2007-2008", 
                                                                      "2008-2009", 
                                                                      "2009-2010", 
                                                                      "2010-2011"), 
                                      "The 'Big Four'",
                                      "Man City dominate"))


unique(selected_premier$era)
View(selected_premier)


selected_premier$era <- factor(selected_premier$era,
                               levels = c("Man United dominate",
                               "The 'Big Four'",
                               "Man City dominate"))

unique(selected_premier$era)

filter_position <- selected_premier[selected_premier$position == 17 | selected_premier$position == 18, ]

View(filter_position)

position_summary <- filter_position %>% 
  group_by(position) %>% 
  summarise(avg = mean(points), std_dev = sd(points))
View(position_summary)

position_summary$position <- factor(position_summary$position)

ggplot(position_summary, aes(x = position, y = avg)) +
  geom_bar(stat = "identity", aes(fill = position))+ 
  geom_errorbar(aes(ymin = avg - std_dev,
                    ymax = avg + std_dev),
                colour = "blue",    # change colour
                width = 0.2,       # width of the horizontal caps
                linewidth = 1)  +   # thickness of the lines)
  labs(title = "A bar plot with error bars",
       x = "Position",
       y = "Average Points")

t.test(points ~ position, data = filter_position)

cohens_d(points ~ position, data = filter_position)

era_summary <- selected_premier %>% 
  group_by(era) %>% 
  summarise(avg = mean(points), std_dev = sd(points))
View(era_summary)

ggplot(era_summary, aes(x = era, y = avg)) + 
  geom_bar(stat = "identity", aes(fill = era)) +
  geom_errorbar(aes(ymin = avg - std_dev,
                    ymax = avg + std_dev),
                colour = "blue",
                width = 0.4,
                linewidth = 1) + 
  labs(title = "A bar chart by era",
       x = "Era",
       y = "Average Points")

anova_result <- aov(points ~ era, data = selected_premier)
summary(anova_result)
TukeyHSD(anova_result)

#step 8
era_position_summary <- filter_position %>%
  group_by(position, era) %>%
  summarise(avg = mean(points),
            std_dev = sd(points))

era_position_summary$position <- factor(era_position_summary$position)

ggplot(era_position_summary, aes(x = era, y = avg, 
                        colour = position,
                        group = position)) +
  geom_line() +
  geom_point() +
  labs(title = "A line chart by era",
       x = "Era",
       y = "Average Points")

#step 9
library(car)
filter_position$position <- factor(filter_position$position)
# 建立模型
anova_two_way <- aov(points ~ era + position, 
                     data = filter_position)

# 解读结果 (替代summary)
Anova(anova_two_way, type = "II")

# 后续检验
TukeyHSD(anova_two_way)

#step 10
ggplot(selected_premier, aes(x = factor(position), y = points)) +
  geom_boxplot() +
  facet_wrap(~ era) +
  geom_hline(yintercept = 40, linetype = "dashed") +
  labs(title = "Total points by position and era",
       subtitle = "Horizontal line represents 40 points for survival mark",
       x = "Position",
       y = "points")
