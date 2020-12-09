library(tidyverse)
library(tidygraph)

# Part 1

# Let's make a function that parses the rule set and makes a graph
make_graph <- function(path, reverse=FALSE) {
  rule_set <- readLines(path) %>%
    str_remove_all("bag[s]?") %>%
    as_tibble() %>% 
    separate(col = value, into = c("to", "contents"), sep = " contain ") %>%
    mutate(to = str_trim(to))

  # At this point, we have a list of all the individual bags
  # in the `to` column of the data.frame.
  nodes <- tibble(name = rule_set$to)
    
  
  edges <- rule_set %>%
    separate_rows(contents, sep = ", ") %>%
    mutate(contents = str_remove_all(contents, "\\.") %>% str_trim()) %>% 
    dplyr::filter(contents != "no other") %>%
    extract(col = contents, into = c("weight", "from"), regex = "([0-9]+) (.*)") %>% 
    mutate(weight = parse_number(weight))
  
  if (reverse) {
    edges <- edges %>% rename(from = to, to = from)
  }
  
  tbl_graph(nodes = nodes, edges = edges)  
  
}

make_graph(here("data", "07_input.txt")) %>% 
  activate(nodes) %>% 
  arrange(desc(name == "shiny gold")) %>% 
  mutate(distance = bfs_dist(root = 1)) %>% 
  filter(!is.na(distance)) %>% 
  as_tibble() %>% 
  nrow() %>% 
  magrittr::add(-1)

# Part 2

# A graph of just those bags held within "shiny gold"
sub_graph <- make_graph(here("data", "07_input.txt"), reverse = TRUE) %>%
  activate(nodes) %>%
  arrange(desc(name == "shiny gold")) %>%
  mutate(rank = bfs_rank(root = 1)) %>%
  filter(!is.na(rank))

# convenience function to get the id from the name - there's probably
# something for this in {tidygraph} but today was the first time
# I looked at {tidygraph}
get_id <- function(graph, name) {
  graph %>%
    activate(nodes) %>%
    filter(name == !!name) %>% 
    pull(rank)
}


get_content_size <- function(graph, id) {

  get_connected <- function(id) {
    graph %>%
      activate(edges) %>% 
      filter(from == id) %>%
      as_tibble()
  }
  
  connected <- get_connected(id)
  
  if (connected %>% nrow() == 0) {
    return(1)
  } else {
    # recurse down
    sum(connected$weight * map_dbl(connected$to, ~get_content_size(graph, .x))) + 1
    # + 1 to count the bag itself
  }
}

get_content_size(sub_graph, get_id(sub_graph, "shiny gold")) - 1 
# -1 here because we have already counted the top bag
