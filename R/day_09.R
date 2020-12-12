library(tidyverse)
library(here)

# Part 1

tmp <- read_csv(here("data","09_input.txt"), col_names = "number", col_types = c(col_number())) %>%
  mutate(n = 1:nrow(.)) %>%
  unite(col = "combined", number, n, sep = "_")
  
tmp2 <- crossing(left = tmp$combined, right = tmp$combined) %>%
  separate(left, into = c("number_left", "n_left")) %>%
  separate(right, into = c("number_right", "n_right")) %>%
  mutate_all(as.numeric) %>%
  filter(between(n_left - n_right, 1, 25)) %>%
  select(n_left, number_left, number_right) %>%
  filter(n_left > 25) %>% 
  group_by(number_left) %>%
  nest()

check_sum <- function(target, option_df) {
  checks <- crossing(a = option_df$number_right,
           b = option_df$number_right) %>% 
    filter(a + b == target)
   
  nrow(checks) > 0
}

tmp2 %>% mutate(check = map2_lgl(number_left, data, check_sum)) %>% filter(check == FALSE)

# Part 2

find_sequence <- function(target, values) {

  # browser()
  for (i in 1:length(values)) {
    for (j in (i+1):length(values)) {
      test_sum <- sum(values[i:j])
      
      if (test_sum == target) {
        return(list(i = i, j = j))
      } else if (test_sum > target) {
        break
      }     
    }
  }
}

test_values <- read_csv(here("data","09_input.txt"),
                        col_names = "number",
                        col_types = c(col_number())) %>%
  pull(number)

weakness_range <- find_sequence(target = 530627549,
              values = test_values)

sum(min(test_values[weakness_range$i:weakness_range$j]),
    max(test_values[weakness_range$i:weakness_range$j]))

