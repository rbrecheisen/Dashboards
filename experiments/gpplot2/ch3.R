library(tidyverse)

df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a", "b", "c")
)

p <- ggplot(df, aes(x, y, label = label)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")