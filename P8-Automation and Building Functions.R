# Part 1 - load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)


# Part 2 - make a vector of the names of the files
files <- list.files(path = "seasons",
                    pattern = "*.csv",
                    full.names = TRUE)
print(files)


# Part 3 - load all the files into a list
data_list <- lapply(files, read.csv)
length(data_list)


# Part 4 - reviewing it worked!
data_list[[1]]
data_list[[33]]

str(data_list[[1]])


# Part 5 - row binding
all_prem_years <- bind_rows(data_list)
nrow(all_prem_years)

# Part 6 - set up for the visual
cl <- c("#35a35c","#4c3596","#f7e21f",
        "#469E95","#f74734","#95cb62","#8d56a5")


# Part 10
prem_positions <- function(all_prem_years, positions){
  # Part 6 - cont
  all_prem_years$season <- paste0(substr(all_prem_years$season, 3, 4), 
                                  "-\n", substr(all_prem_years$season, 8, 9))
  
  
  # Part 7 - filter your dataset
  select_data <- filter(all_prem_years, 
                        pos %in% positions)
  
  select_data$pos <- factor(select_data$pos, 
                            levels = positions)
  
  
  # Part 8 - make your title
  library(stringr)
  
  var_title <- paste0("Includes teams in positions ", 
             str_flatten_comma(positions, last = ", and "), ".")
  
  
  # Part 9 - building the visual
  ggplot(select_data, aes(x = season, y = pts, 
                          colour = pos,
                          group = pos)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, by = 10)) +
    scale_colour_manual(values = cl) +
    labs(title = var_title,
         x = "Season", 
         y = "Points",
         colour = "Position") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title.position = "plot")
}


# Part 10 - Testing
prem_positions(
  all_prem_years,
  positions = c(2, 7, 18)
)
