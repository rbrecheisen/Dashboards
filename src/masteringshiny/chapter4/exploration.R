library(shiny)
library(vroom)
library(tidyverse)
library(rstudioapi)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create("neiss")


download <- function(name) {
  target_file <- file.path("neiss", name)
  download.file(paste0("https://raw.github.com/hadley/mastering-shiny/main/neiss/", name), target_file, quiet = TRUE)
}


download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")


injuries <- vroom::vroom(file.path("neiss", "injuries.tsv.gz"))
injuries

products <- vroom::vroom(file.path("neiss", "products.tsv"))
products

population <- vroom::vroom(file.path("neiss", "population.tsv"))
population


selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% 
  count(location, wt = weight, sort = TRUE)

selected %>% 
  count(body_part, wt = weight, sort = TRUE)

selected %>% 
  count(diag, wt = weight, sort = TRUE)


summary <- selected %>% 
  count(age, sex, wt = weight)
summary

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)
summary

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")
