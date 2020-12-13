library(tidyverse)
library(here)
library(tictoc)

# Part 1
get_device_rating <- function(adapters) {
  max(adapters) + 3
}

jolts <- sort(as.numeric(readLines(here("data", "10_input.txt"))))

results <- table(c(jolts, get_device_rating(jolts)) - c(0, jolts))

results[["1"]] * results[["3"]]

# Part 2

## This works, but it's slow - in fact I left it running and got bored, so I
## don't know how long it takes.

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
