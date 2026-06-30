#libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# load data
harry_kane <- read.csv("harry_kane_stats.csv")

harry_kane$goals_assists <- harry_kane$goals + harry_kane$assists
harry_kane$goals_no_pens <- harry_kane$goals - harry_kane$pens
harry_kane$goals_xg_diff <- harry_kane$goals - harry_kane$expected_goals

player_name <- "Harry Kane"
seasons <- length(unique(harry_kane$season))
total_appearances <- sum(harry_kane$matches_played)
total_goals <- sum(harry_kane$goals)
total_combined <- sum(harry_kane$goals_assists)
avg_exp_goals <- round(mean(harry_kane$expected_goals, na.rm = TRUE), 
                       digits = 1)

paste("Player:", player_name, "| Seasons:", seasons, 
      "| Appearances:", total_appearances)
paste("Goals:", total_goals, "| Goals and assists:", total_combined,
      "| Average expected goals:", avg_exp_goals)

cat("Player:", player_name, "\n",
    "Seasons:", seasons, "\n",
    "Appearances:", total_appearances, "\n",
    "Goals:", total_goals, "\n",
    "Goals and assists:", total_combined, "\n",
    "Average expected goals:", avg_exp_goals)

kane_top_seasons <- harry_kane[harry_kane$goals_assists > 30, ]


kane_top_seasons <- kane_top_seasons[ , c("season", "age", "squad", "goals",
                                         "goals_assists", "goals_xg_diff",
                                         "goals_no_pens")]

kane_long <- pivot_longer(harry_kane,
                          cols = c("goals", "assists", "pens"),
                          names_to = "goal_interaction",
                          values_to = "total")


kane_wide <- pivot_wider(kane_long,
                         names_from = "goal_interaction",
                         values_from = "total")


ggplot(kane_long, aes(x = age, y = total,
             colour = goal_interaction,
             group = goal_interaction)) +
  geom_line()

kane_long_agg <- aggregate(total ~ age + goal_interaction,
                           data = kane_long,
                           FUN = sum)

ggplot(kane_long_agg, aes(x = age, y = total,
                          colour = goal_interaction,
                          group =  goal_interaction)) +
  geom_line() +
  geom_point()+
  scale_color_manual(values = c("goals" = "#f3d250",
                                "assists" = "#64b3ff",
                                "pens" = "#ff6f61"))+
  labs(title = "Harry Kane Career Goal Contributions by Age",
       x = "Age",
       y = "Goal Contributions",
       colour = "Metric")
