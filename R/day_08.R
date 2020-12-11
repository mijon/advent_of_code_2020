library(tidyverse)
library(here)

# Part 1

parse_instruction <- function(instruction) {
  instruction <- str_split(instruction, " ")[[1]]
  
  list(action = instruction[1],
       value = parse_number(instruction[2]))
}

make_history <- function(instruction_set) {
  
  previous_pointers <- c()
  previous_accumulator <- c()
  
  # initialise state
  pointer <- 1
  accumulator <- 0
  
  while (!pointer %in% previous_pointers) {
    previous_pointers <- c(previous_pointers, pointer)
    previous_accumulator <- c(previous_accumulator, accumulator)
    
    instruction <- parse_instruction(instructions[[pointer]])
    
    if (instruction$action == "nop") {
      pointer <- pointer + 1
    } else if (instruction$action == "acc") {
      pointer <- pointer + 1
      accumulator <- accumulator + instruction$value
    } else if (instruction$action == "jmp") {
      pointer <- pointer + instruction$value
    } else {
      stop(paste("unknown instruction:", instruction$action))
    }
  }
  
  tibble(pointer = previous_pointers,
         accumulator = previous_accumulator) 
}

instructions <- readLines(here("data", "08_input.txt"))
make_history(instruction_set = instructions)

# Part 2

