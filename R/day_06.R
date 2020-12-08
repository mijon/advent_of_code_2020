library(tidyverse)
library(here)

# Part 1
combined_unique <- read_file(here("data", "06_input.txt")) %>% 
  str_split("\\n\\n") %>%
  map(~str_replace_all(.x, "\\n", "")) %>% 
  map(~str_extract_all(.x, ".")) %>% 
  pluck(1) %>% 
  map(unique)

combined_unique %>% 
  map_dbl(length) %>% 
  sum()

## Part 2
groups <- read_file(here("data", "06_input.txt")) %>%
  str_split("\\n\\n") %>%
  map(~str_split(.x, "\\n")) %>%
  pluck(1)

map(groups, ~str_extract_all(.x, ".") %>%
      reduce(intersect) %>%
      length()) %>%
  reduce(sum)
