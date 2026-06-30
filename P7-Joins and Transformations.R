# step1 - load data
library(ggplot2)
library(dplyr)
library(tidyr)
imdb <- read.csv("imdb.csv")
rt <- read.csv("rotten_tom_ratings.csv")

# step2 - remove duplicate
library(dplyr)
rt_unique <- distinct(rt, release_date, .keep_all = TRUE)

# step3 - join
join_rate <- inner_join(imdb, rt_unique, by = "title")
View(join_rate)

# step4 - convert number
rotten_tom_critic <- as.numeric(sub("%", "", join_rate$critic_score))
rotten_tom_audience <- as.numeric(sub("%", "", join_rate$audience_score))

join_rate$rotten_tom_critic <- rotten_tom_critic
join_rate$rotten_tom_audience <- rotten_tom_audience

# step5 - scale up
imd <- join_rate$avg_vote * 10
join_rate$imd<- imd

# step6 - average
join_rate <- join_rate %>%
  rowwise() %>%
  mutate(avg_ratings = round(
    mean(c(rotten_tom_critic,
           rotten_tom_audience,
           imd),
         na.rm = TRUE),
    digits = 1)) %>%
  ungroup()

# step7 - reshape to long
long_rate <- join_rate %>%
  select(title, year, rotten_tom_critic, 
         rotten_tom_audience, imd, avg_ratings) %>%
  pivot_longer(cols = c(rotten_tom_critic, 
                        rotten_tom_audience, imd),
               names_to = "rating_type",
               values_to = "ratings")

# step8 - filter top films
top_films <- c("The Shawshank Redemption", "The Godfather",
               "The Dark Knight", 
               "The Lord of the Rings: The Return of the King",
               "Schindler's List",
               "The Lord of the Rings: The Fellowship of the Ring",
               "Pulp Fiction", "The Good, the Bad and the Ugly",
               "Forrest Gump", 
               "The Lord of the Rings: The Two Towers")

top_data <- long_rate %>%
  filter(title %in% top_films)

# step9 - factor
top_data$title <- factor(top_data$title, levels = top_films)

# step10 - plot
avg_line <- mean(top_data$ratings, na.rm = TRUE)

ggplot(top_data, aes(x = title, y = ratings, 
                     colour = rating_type)) +
  geom_point(size = 3) +
  geom_hline(yintercept = avg_line, 
             linetype = "dashed") +
  annotate("text", x = 8, y = avg_line + 1.5,
           label = "Average rating") +
  geom_text(aes(label = ratings), 
            size = 3, vjust = -0.8) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  scale_y_continuous(limits = c(70, 100),
                     breaks = seq(70, 100, by = 5)) +
  scale_colour_manual(values = c("rotten_tom_critic" = "#069880",
                                 "rotten_tom_audience" = "#961E06",
                                 "imd" = "#064F98")) +
  labs(title = "Top 10 IMDb films ratings comparison") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank())
