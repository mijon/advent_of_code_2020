library(tidyverse)

# Part 1
read_file(here("data", "06_input.txt")) %>% 
  str_split("\\n\\n") %>%
  map(~str_replace_all(.x, "\\n", "")) %>% 
  map(~str_extract_all(.x, ".")) %>% 
  pluck(1) %>% 
  map(unique) %>% 
  map_dbl(length) %>% 
  sum()

## Part 2
