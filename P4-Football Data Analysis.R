# step1 - load data
origin_data <- read.csv("harry_kane_stats.csv")
View(origin_data)

# step2 - define and calculate new variables
origin_data$goals_assists <- origin_data$goals + origin_data$assists
origin_data$goals_no_pens <- origin_data$goals - origin_data$pens
origin_data$goals_xg_diff <- origin_data$goals - origin_data$expected_goals

# step3 - calculate statistics
name <- "Harry Kane"
seasons <- length(unique(origin_data$season))
appear <- sum(origin_data$matches_played)
total_goals <- sum(origin_data$goals)
total_combined <- sum(origin_data$goals_assists)
avg_exp_goal <- round(mean(origin_data$expected_goals, na.rm = TRUE),
                      digits = 1)
# step4 - print out
# Line 1
print(paste("Player:", name, 
            "| Seasons:", seasons, 
            "| Appearances:", appear))

# Line 2
print(paste("Goals:", total_goals, 
            "| Goals and assists:", total_combined, 
            "| Average expected goals:", avg_exp_goal))

# step5 - filter data
filter_data <- origin_data[origin_data$goals_assists > 30, ]
View(filter_data)

# step6 - select columns
select_data <- filter_data[ ,c("season", "age", "squad", "goals",
                                "goals_assists", "goals_xg_diff",
                                "goals_no_pens"), ]
View(select_data)




