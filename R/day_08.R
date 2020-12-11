library(tidyverse)
library(here)

# Part 1

parse_instruction <- function(instruction) {
  instruction <- str_split(instruction, " ")[[1]]
  
  list(action = instruction[1],
       value = parse_number(instruction[2]))
}

run_prog <- function(instruction_set) {
  # Store max instruction number
  n_instructions <- length(instruction_set)
  
  previous_pointers <- c()
  previous_accumulator <- c()
  previous_actions <- c()
  previous_values <- c()
  
  # initialise state
  pointer <- 1
  accumulator <- 0
  terminates <- FALSE
  
  while (!pointer %in% previous_pointers) {
    previous_pointers <- c(previous_pointers, pointer)
    previous_accumulator <- c(previous_accumulator, accumulator)
    
    # instruction <- parse_instruction(instructions[[pointer]])
    instruction <- instruction_set[[pointer]]
    previous_actions <- c(previous_actions, instruction$action)
    previous_values <- c(previous_values, instruction$value)
    
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
    
    if (pointer > n_instructions) {
      # print(glue::glue("reached the end with accumulator value: {accumulator}"))
      terminates <- TRUE
      break
    }
  }
  
  list(
    path = tibble(pointer = previous_pointers,
                  accumulator = previous_accumulator,
                  action = previous_actions,
                  value = previous_values),
    terminates = terminates
  )
}

instructions <- readLines(here("data", "08_input.txt")) %>% map(parse_instruction)
run_prog(instruction_set = instructions) %>% pluck("path") %>%  tail()

# Part 2
flip_prog <- function(instruction_set, p) {
  current_action <- instruction_set[[p]]$action
  
  if (current_action == "nop") {
    new_action <- "jmp"
  } else if (current_action == "jmp") {
    new_action <- "nop"
  } else {
    new_action <- current_action
  }
  
  instruction_set[[p]]$action <- new_action
  instruction_set
}

correct_program <- function(instruction_set) {
  initial_run <- run_prog(instruction_set)
  path <- initial_run$path$pointer
  
  for (p in path) {
    tmp_prog <- flip_prog(instruction_set, p)
    tmp_run <- run_prog(tmp_prog)
    
    if (tmp_run$terminates) {
      return(tmp_run)
    }
  }
}

corrected_program <- correct_program(instructions)
tail(corrected_program$path)
