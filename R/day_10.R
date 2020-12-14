library(tidyverse)
library(here)
library(tictoc)
library(tidygraph)
library(igraph)
library(ggraph)

# Part 1
get_device_rating <- function(adapters) {
  max(adapters) + 3
}



jolts <- sort(as.numeric(readLines(here("data", "10_input.txt"))))

results <- table(c(jolts, get_device_rating(jolts)) - c(0, jolts))

results[["1"]] * results[["3"]]

# Part 2

## This should work, but it will be slow. We would be checking every route.

# count_possibilities <- function(jolts) {
#   counter <- 0
#   current_joltage <- 0
#   
#   target <- get_device_rating(jolts)
#   jolts <- c(jolts, target)
#   
#   get_candidates <- function(current_joltage) {
#     jolts[(jolts - current_joltage) <= 3 & (jolts - current_joltage) > 0]
#   }
#   
#   crawl_joltage <- function(current_jolts) {
#     if (current_jolts == target) {
#       counter <<- counter + 1
#     } else {
#       for (next_adapter in get_candidates(current_jolts)) {
#         crawl_joltage(next_adapter)
#       }
#     }
#     return(counter)
#   }
#   
#   tic()
#   counter <- crawl_joltage(current_joltage)
#   toc()
#   counter
# }

make_graph <- function(jolts) {
  get_candidates <- function(current_joltage, adapters) {
    adapters[(adapters - current_joltage) <= 3 & (adapters - current_joltage) > 0]
  }
  
  jolts_df <- tibble(name = jolts)
  
  jolts_edges <- jolts_df %>% 
    mutate(to = map(name, get_candidates, name)) %>% 
    unnest_longer(to) %>% 
    filter(!is.na(to)) %>% 
    rename(from = name) %>% 
    mutate_all(as.character)
  
  tbl_graph(nodes = jolts_df %>% mutate_all(as.character), edges = jolts_edges)
}

load_jolts <- function(path) {
  jolts <- sort(as.numeric(readLines(path)))
  jolts <- c(0, jolts, get_device_rating(jolts))
}

jolts <- load_jolts(here("data", "10_test_1.txt"))
jolt_graph <- make_graph(jolts)

# -----

# Util function to make a graph of nodes from 1 to `n` for which any nodes
# `diff` apart are connected
make_subgraph <- function(max, diff) {
  nodes <- tibble(name = 1:max)
  edges <- tibble(from = 1:max,
                  to = map(from, ~.x + 1:diff)) %>% 
    unnest(to) %>% 
    filter(to <= max)
  
  tbl_graph(nodes = nodes, edges = edges)
}

# count all the simple paths from `from` to `to`
count_paths <- function(graph, from, to) {
  all_simple_paths(graph, from = from, to = to) %>% length()
}

# for a given number `n` count all the simple paths in the graph of nodes from 1
# to `n` for which any nodes `diff` apart are connected
check_graph <- function(n, diff = 3) {
  if (n == 1) {
    1
  } else {
    make_subgraph(n, diff) %>% 
      count_paths(from = 1, to = n)
  }
}

jolts <- sort(as.numeric(readLines(here("data", "10_input.txt"))))
device_rating <- get_device_rating(jolts)
jolts <- c(0, jolts, device_rating)

tibble(runs = split(jolts, cumsum(c(1, diff(jolts) != 1)))) %>%
  mutate(run_length = map_dbl(runs, length)) %>%
  count(run_length) %>%
  mutate(paths = map_dbl(run_length, check_graph),
         all_paths = paths^n) %>% pull(all_paths) %>% prod()



