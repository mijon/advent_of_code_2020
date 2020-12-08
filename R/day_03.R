# Part 1
library(tidyverse)


load_map <- function(path) {

  # read the file from the path and convert to a list of character vectors
  tmp <- read_file(path) %>% 
    str_split("\n") %>% 
    map(str_split, "")
  
  # convert the list of characters to a matrix with the right orientation
  tmp <- simplify2array(tmp[[1]]) %>% t()
  tmp == "#"
}


test_toboggan <- load_map(here("data", "03_test.txt"))
toboggan <- load_map(here("data", "03_input.txt"))

check_slope <- function(down, right, toboggan) {
  get_is <- function(down, start = 1, end = 323) {
    seq(from = start, to = end, by = down)
  }
  
  get_js <- function(right, start = 1, width = 31, end = 323) {
    ((seq(from = start, length.out = ceiling(end / down), by = right) -1 ) %% width) + 1
  }
  
  
  i_s <- get_is(down = down, start = 1, end = dim(toboggan)[[1]])
  j_s <- get_js(right = right, start = 1, width = dim(toboggan)[2], end = dim(toboggan)[1])
  
  map2_lgl(i_s, j_s, ~toboggan[.x, .y]) %>% sum()
}
# Part 2
downs <- c(1,1,1,1,2)
rights <- c(1,3,5,7,1)

tibble(down = downs, right = rights) %>% 
  mutate(trees = map2_dbl(down, right, check_slope, toboggan)) %>% 
  pull(trees) %>% 
  prod()

