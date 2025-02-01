library(tidyverse)

mpg

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 17, color = "pink")

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, stroke = TRUE))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth()
# `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

# Left
ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) + 
  geom_smooth()

# Right
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) + 
  geom_smooth()

ggplot(mpg) +
  geom_smooth(aes(x = displ, y = hwy))
