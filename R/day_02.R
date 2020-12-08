library(tidyverse)

passwords <- read_csv(here("data", "02_input.txt"), col_names = "tmp")

check_password <- function(password, low, high, letter) {
  low < str_count()
}

# part 1
base <- passwords %>% 
  separate(col = tmp,
           into = c("constraints", "letter", "password"),
           sep = " ") %>% 
  mutate(letter = str_extract(letter, "[a-z]"))

base %>% 
  separate(col = constraints,
           into = c("low", "high"),
           sep = "-") %>% 
  mutate(low = parse_number(low),
         high = parse_number(high),
         num_letters = str_count(password, letter),
         pass_fail = num_letters >= low & num_letters <= high) %>% 
  count(pass_fail)

# part 2
base %>% 
  separate(col = constraints,
           into = c("first", "second"),
           sep = "-") %>% 
  mutate(first = parse_number(first),
         second = parse_number(second)) %>% 
  mutate(entry_first = str_sub(password, first, first) == letter,
         entry_second = str_sub(password, second, second) == letter,
         test = entry_first + entry_second) %>% 
  count(test)
  
  
