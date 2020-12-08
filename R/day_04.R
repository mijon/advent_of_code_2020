# Part 1
library(tidyverse)
library(here)

get_batch <- function(path) {
  read_file(path)
}

required <- c("byr",
              "iyr",
              "eyr",
              "hgt",
              "hcl",
              "ecl",
              "pid"#,
              #"cid" # cid is not required
              )


validate_passport <- function(pass_string) {
  pass_string %>%
    str_detect(glue::glue("{required}:\\S+")) %>%
    all()
}

count_passing_in_file <- function(path, validator) {
  get_batch(path) %>% 
  str_split("\\n\\n") %>% map(~str_replace_all(.x, "\n", " ")) %>% 
  pluck(1) %>% 
  map_lgl(validator) %>% 
  sum()
}

count_passing_in_file(here("data", "04_input.txt"), validate_passport)

# Part 2

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
validate_byr <- function(v) {
  (str_detect(v, "^[0-9]{4}$")) && (as.numeric(v) >= 1920) && (as.numeric(v) <= 2002)
}

# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
validate_iyr <- function(v) {
  (str_detect(v, "^[0-9]{4}$")) && (as.numeric(v) >= 2010) && (as.numeric(v) <= 2020)
}

# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
validate_eyr <- function(v) {
  (str_detect(v, "^[0-9]{4}$")) && (as.numeric(v) >= 2020) && (as.numeric(v) <= 2030)
}

# gt (Height) - a number followed by either cm or in:
#    If cm, the number must be at least 150 and at most 193.
#    If in, the number must be at least 59 and at most 76.
validate_hgt <- function(v) {
  first_test <- str_detect(v, "^[0-9]+[in]|[cm]$")
  
  if(!first_test) {
    return(FALSE)
  }
  
  unit <- str_extract(v, "(in)|(cm)")
  value <- as.numeric(str_extract(v, "[0-9]+"))
  
  if (unit == "in") {
    value >= 59 && value <= 76
  } else if (unit == "cm") {
    value >= 150 && value <= 193
  }
}

# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validate_hcl <- function(v) {
  str_detect(v, "#[0-9a-f]{6}")
}

# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validate_ecl <- function (v) {
  allowed <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  sum(str_detect(v, glue::glue("^{allowed}$"))) == 1
}

# pid (Passport ID) - a nine-digit number, including leading zeroes.
validate_pid <- function(v) {
  str_detect(v, "^[0-9]{9}$")
}

validate_cid <- function(v) {
  TRUE
}


validate_passport_2 <- function(pass_string) {
  
  # must have all the required fields
  if(!validate_passport(pass_string)) {
    return(FALSE)
  }
  
  tibble(pstrings = pass_string %>%
           str_extract_all(glue::glue("{required}:\\S+")) %>%
           as_vector()) %>%
    separate(pstrings, into = c("prefix", "value"), sep = ":") %>%
    mutate(function_string = glue::glue("validate_{prefix}('{value}')"),
           function_expr = map(function_string, rlang::parse_expr),
           result = map_lgl(function_expr, rlang::eval_tidy)) %>%
    pull(result) %>%
    reduce(`&`)
}

count_passing_in_file(here("data", "04_input.txt"), validate_passport_2)
