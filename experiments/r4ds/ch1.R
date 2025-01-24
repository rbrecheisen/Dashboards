install.packages(c("tidyverse", "palmerpenguins", "ggthemes"))

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

penguins

glimpse(penguins)

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species",
  ) +
  scale_color_colorblind()

# Exercises

# 1. How many rows and columns are in penguins?
nrow(penguins)
ncol(penguins)

# 2. What does the bill_depth_mm variable describe?
# Bill (or beak) depth means the vertical height of the bill
?penguins

# 3. Make a scatter plot of bill depth vs. bill length
ggplot(data = penguins, mapping = aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point()
# There seems to be no clear linear relationship between these variables giving
# the impression that they are not dependent on each other

# 4. Make a scatter plot of species vs. bill depth. Is there a better geom for this?
ggplot(data = penguins, mapping = aes(x = species, y = bill_length_mm)) +
  geom_point()
ggplot(data = penguins, mapping = aes(x = species, y = bill_length_mm)) +
  geom_boxplot()

# 5. Why does the following give an error message?
ggplot(data = penguins) +
  geom_point()
# We didn't specify which variables to display in the scatter plot. You need both
# an X and Y in the aes mapping

# 6. What does the na.rm argument do in geom_point?
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE)

# 7. Add a caption to the original graph
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species",
    caption = "Data come from the palmerpenguins dataset"
  ) +
  scale_color_colorblind()

# 8. Recreate the visualization 
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth(method = "loess")

# 9. What do you think this plot looks like?
# It's a scatter plot where each point is colored based on island. Furthermore,
# the visible regression line will be drawn for each island separately.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

# 10. Will these two graphs look different? Why/why not?
# They will look exactly the same because the global aes settings in the first
# will be forwarded to the geom settings. In the second graph the data and aes
# settings are directly placed in the geoms
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# End of exercises

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Exercises

# 1. Make a bar plot of penguin species where you assign species to the y aesthetic
ggplot(penguins, aes(y = species)) +
  geom_bar()
# The bars are now horizontal instead of vertical

# 2. How are the following two plots different? 
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")
# Fill is more appropriate because color only colors the outline

# 3. What does the bins argument of geom_histogram do?
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 30)
# It specifies the number of bins to use but is overridden by binwidth

# 4. Make a histogram of the carat variable in the diamonds dataset. Which
# bin width shows the most interesting features?
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# End of exercises

