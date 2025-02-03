library(tidyverse)

ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

economics
ggplot(economics, aes(date, unemploy / pop)) + # number of unemployed divided by population size to get %
  geom_line()
ggplot(economics, aes(date, uempmed)) + # time of unemployment
  geom_line()

ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path() +
  geom_point()

year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year(date)))

ggplot(mpg, aes(cty, hwy)) + geom_jitter()
# cty: city mileage, hwy: highway mileage (miles per gallon)

ggplot(mpg, aes(reorder(class, hwy, FUN = median), hwy)) + geom_boxplot()

ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_grid(~cut)

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_violin()

ggplot(diamonds, aes(x = price, color = cut)) +
  geom_freqpoly()

