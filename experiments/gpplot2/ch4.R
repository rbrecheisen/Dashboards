library(tidyverse)
library(nlme)

data(Oxboys, package = "nlme")
head(Oxboys)

ggplot(Oxboys, aes(age, height)) +
  geom_point(aes(group = Subject)) +
  geom_smooth(method = "lm", linewidth = 2, se = FALSE)

ggplot(Oxboys, aes(Occasion, height)) +
  geom_boxplot() +
  geom_line(aes(group = Subject), color = "#3366FF", alpha = 0.5)

df <- data.frame(x = 1:3, y = 1:3, color = c(1, 3, 5))

ggplot(df, aes(x, y, color = factor(color))) +
  geom_line(aes(group = 1), linewidth = 2) + 
  geom_point(size = 5)

ggplot(df, aes(x, y, color = color)) +
  geom_line(aes(group = 1), linewidth = 2) +
  geom_point(size = 5)
