# step1 - load data
data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-08-06/olympics.csv")

# step2 - GB data only
gb_data <- data[data$team == "Great Britain" & 
                  data$event %in% c("Athletics Men's 800 metres", 
                                    "Athletics Women's 800 metres", 
                                    "Athletics Men's 1,500 metres", 
                                    "Athletics Women's 1,500 metres"), ]

# step3 - missing data
gb_data$medal <- ifelse(is.na(gb_data$medal), "No medal", gb_data$medal)

# step4 - aggregation
library(dplyr)

gb_agg <- gb_data %>% 
  group_by(medal, event) %>% 
  summarise(n_indiv_medals = n())

print(gb_agg)

# step5 - initial visualise
library(ggplot2)
ggplot(gb_agg, aes(x = medal, y = n_indiv_medals,
                          fill = medal)) + 
  geom_bar(stat = "identity")

# step6 - reorder
gb_agg$medal <- factor(gb_agg$medal,
                       levels = c("Gold", "Silver",
                                  "Bronze", "No medal"))

# step7 - 8
ggplot(gb_agg, aes(x = medal, y = n_indiv_medals,
                   fill = medal)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ event) +
  scale_fill_manual(values = c("Gold" = "#D6AF36",
                               "Silver" = "#A7A7AD",
                               "Bronze" = "#A77044",
                               "No medal" = "#333333")) +
  labs(title = "Great Britain middle distance (800 & 1500 metres) medals at all Olympics")