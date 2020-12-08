# part 1
library(tidyverse)

decode_seat <- function(seat_str) {
  row_str <- str_sub(seat_str, 1, 7) %>% str_replace_all("F", "a") %>% str_replace_all("B", "b")
  col_str <- str_sub(seat_str, 8, 11) %>% str_replace_all("R", "b") %>% str_replace_all("L", "a")
  
  row_num <- get_num(row_str, 0, 127)
  # print(row_num)
  col_num <- get_num(col_str, 0, 7)
  # print(col_num)
  row_num * 8 + col_num
}

get_num <- function(pattr, low, high) {
  # browser()
  first_letter <- str_sub(pattr, 1, 1)
  
  if (first_letter == "a") {
    high <- low + 0.5 * (high - low) - 0.5
  } else {
    low <- low + 0.5 * (high - low) + 0.5
  }
  
  if (low == high) {
    low
  } else {
    get_num(str_sub(pattr, 2), low, high)
  }
}

results <- read_csv(here("data", "05_input.txt"), col_names = "seat_code") %>% 
  mutate(seat_id = map_dbl(seat_code, decode_seat)) %>% 
  arrange(desc(seat_id))

results 
# Part 2

# answer is seat +1 of that shown
results %>%
  mutate(diff = seat_id - lag(seat_id)) %>% 
  filter(diff != -1)
