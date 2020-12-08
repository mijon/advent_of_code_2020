library(tidyverse)
library(here)

numbers <- read_csv(here("data", "01_input.csv"), col_names = "number")

# part 1
# the product of two that sum to 2020
crossing(
  a = numbers$number,
  b = numbers$number
) %>% mutate(sum = a + b) %>% 
  filter(sum == 2020) %>% 
  mutate(product = a * b)


# part 2
# the product of 3 that sum to 2020
crossing(
  a = numbers$number,
  b = numbers$number,
  c = numbers$number
) %>% mutate(sum = a + b + c) %>% 
  filter(sum == 2020) %>% 
  mutate(product = a * b * c)
