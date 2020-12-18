library(tidyverse)
library(here)

# Part 1
load_as_matrix <- function(path) {
  opts <- options(tidyverse.quiet = TRUE)
  on.exit(options(opts))
  mat <- read_csv(path, col_names = "seats") %>%
    mutate(seats = str_extract_all(seats, pattern = ".")) %>%
    unnest_wider(seats) %>%
    as.matrix()
  
  
  colnames(mat) <- NULL
  mat
}


is_seat <- function(seat_mat) {
  check <- seat_mat == "L" | seat_mat == "#"
  check[is.na(check)] <- FALSE
  check
}

is_occupied <- function(seat_mat) {
  check <- (seat_mat == "#" & is_seat(seat_mat))
  check[is.na(check)] <- FALSE
  check
}

is_empty <- function(seat_mat) {
  check <- (seat_mat == "L" & is_seat(seat_mat))
  check[is.na(check)] <- FALSE
  check
}

make_empty_padded_mat <- function(base_mat, element, row_plus = 2, col_plus = 2) {
  matrix(element,
         nrow = nrow(base_mat) + row_plus,
         ncol = ncol(base_mat) + col_plus)
}

nudge_mat <- function(base_mat, row_shift, col_shift) {
  n_rows <- nrow(base_mat)
  n_cols <- ncol(base_mat)
  
  tmp_mat <- make_empty_padded_mat(base_mat, NA, 0, 0)
  
  sub_mat <- base_mat[(1 + min(max(0, row_shift), n_rows)):(n_rows + max(min(0, row_shift), -n_rows)),
                      (1 + min(max(0, col_shift), n_cols)):(n_cols + max(min(0, col_shift), -n_cols))]

  tmp_mat[(1 + min(max(0, -row_shift), n_rows)):(n_rows + max(min(0, -row_shift), -n_rows)),
          (1 + min(max(0, -col_shift), n_cols)):(n_cols + max(min(0, -col_shift), -n_cols))] <- sub_mat
  
  tmp_mat
}

pad_seats <- function(seat_mat) {
  base <- make_empty_padded_mat(seat_mat, ".")
  
  base[2:(nrow(seat_mat)+1),2:(ncol(seat_mat)+1)] <- seat_mat
  base
}

count_adjacent <- function(check_fun) {
  function(seat_mat) {
    crossing(i = -1:1, j = -1:1) %>%
      filter(!(i==0 & j==0)) %>%
      mutate(mat = map2(i, j, ~check_fun(nudge_mat(seat_mat, .x, .y)))) %>%
      pull(mat) %>%
      reduce(`+`)
  }
}

count_adjacent_seats <- count_adjacent(is_seat)
count_adjacent_empty <- count_adjacent(is_empty)
count_adjacent_occupied <- count_adjacent(is_occupied)

one_step <- function(seat_mat) {
  to_occupied <- is_empty(seat_mat) & count_adjacent_occupied(seat_mat) == 0
  to_empty <- is_occupied(seat_mat) & count_adjacent_occupied(seat_mat) >= 4
  
  seat_mat[to_occupied] <- "#"
  seat_mat[to_empty] <- "L"

  seat_mat
}


run_until_stationary <- function(seat_mat) {
  next_state <- one_step(seat_mat)
  
  while (!all(next_state == seat_mat)) {
    seat_mat <- next_state
    next_state <- one_step(next_state)
  }

  seat_mat
}

count_occupied <- function(seat_mat) {
  sum(seat_mat == "#")
}

seat_mat <- load_as_matrix(here("data", "11_input.txt"))

seat_mat %>% run_until_stationary() %>% count_occupied()


# Part 2