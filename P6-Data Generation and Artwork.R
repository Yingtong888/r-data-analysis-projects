# step1 - load vector & step9 - setting a seed
set.seed(08)

co <- c('#ffc09f', '#ffd799', '#ffee93', '#fef2ad', 
'#fcf5c7', '#cee2d0', '#a0ced9', '#a7e3c8', 
'#adf7b6', '#264653', '#287271', '#2a9d8f', 
'#e9c46a', '#f4a261', '#e76f51', '#e97c61')

# step2 - sample color
co_sample <-sample(x = co, size = 4, replace = FALSE)
print(co_sample)

# step3 - generate number sequence
number_seq <- seq(0, 100, length.out = 30)
print(number_seq)

# step4 - make a factor
fct <- factor(1:length(co_sample))
print(fct)

# step5 - generate data frame
df <- expand.grid(x = number_seq, group = fct)
print(df)

# step6 - generate a randomised normal distribution
new_column <- rnorm(nrow(df), 
                    mean = mean(number_seq), sd = sd(number_seq))

library(dplyr)
df <- rename(df, group_var = group)

df$y <- new_column
print(df)

# step7 - stream plot & step8 - aesthetic adjustments
library(ggplot2)
library(ggstream)

ggplot(df, aes(x = x, y = y, 
               fill = group_var, 
               colour = group_var)) +
  geom_stream(type = "proportional") +
  scale_fill_manual(values = co_sample) +
  scale_colour_manual(values = co_sample) +
  theme_void() +
  guides(fill = "none", colour = "none")
