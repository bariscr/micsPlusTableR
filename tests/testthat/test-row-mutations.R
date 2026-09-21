row_mutation_session <- function(direction, conditions, stats = 'n', columns = c('TRUE', 'sex == 2')) {
  s <- small_plan_session(direction)
  rows <- seq_along(conditions) + 8L
  cols <- seq_along(columns) + 2L
  s$out_glob$tab <- tidyr::expand_grid(row_index = rows, col_index = cols)
  s$out_glob$tab$stat_type <- rep(rep(stats, length.out = length(rows)), each = length(cols))
  s$out_glob$tab_r <- tibble::tibble(row_index = rows, row_lgc = conditions)
  s$out_glob$tab_c <- tibble::tibble(col_index = cols, col_lgc = columns)
  s$out_glob$col_header <- tibble::tibble(col_index = cols, col_header = columns)
  s$out_glob$row_header <- tibble::tibble(row_index = rows, row_header = conditions)
  s$out_glob$indent_rows <- tibble::tibble(row = rows, indent = 1L)
  s$out_glob$group_info <- tibble::tibble(row = rows, grp = 'total')
  s
}

row_mutation_values <- function(s, ...) {
  f <- if (s$out_glob$tab_direction == 'h') tabulate_h else tabulate_v
  f(s, ...) |> dplyr::arrange(row_index, col_index) |> dplyr::pull(value)
}

test_that('row parser supports nested, multiline, multiple and legacy inline mutations', {
  for (separator in c('\n', '\n- - -\n', '\n---\n', ' |> ', ' %>% ')) {
    raw <- paste0('dplyr::mutate(z = if_else(sex %in% c(1, 2), 1, 0))', separator,
                  'mutate(\nz2 = z + 1, note = "a ) ("\n)', separator,
                  'z2 == 2 &\nbetween(sex, 1, 2)')
    parsed <- row_condition_f(data.frame(row_index = 9L, row_lgc = raw))
    expect_identical(parsed$row_condition, 'z2 == 2 & between(sex, 1, 2)')
    expect_silent(rlang::parse_expr(paste('df |>', parsed$calculation)))
  }
  expect_error(row_condition_f(data.frame(row_index = 9L,
    row_lgc = 'mutate(z = if_else(sex == 1, 1, 0)\n- - -\nz == 1')), 'Invalid row mutation')
})

test_that('row mutations propagate downwards without changing earlier results in either direction', {
  conditions <- c('mutate(z = if_else(sex == 2, 1, 0))\n- - -\nz == 1',
    'z == 1', 'mutate(z = if_else(sex == 1, 1, 0))\n- - -\nz == 1', 'z == 1')
  for (direction in c('h', 'v')) {
    s <- row_mutation_session(direction, conditions)
    original <- s$hh
    expect_equal(row_mutation_values(s), c(5, 5, 5, 5, 1, 0, 1, 0))
    expect_identical(s$hh, original)
    expect_equal(row_mutation_values(s), c(5, 5, 5, 5, 1, 0, 1, 0))
    # Excel row order controls execution even when the supplied specification is shuffled.
    s$out_glob$tab_r <- s$out_glob$tab_r[c(4, 2, 1, 3), ]
    expect_equal(row_mutation_values(s), c(5, 5, 5, 5, 1, 0, 1, 0))
    expect_equal(row_mutation_values(s, skip_row_conditions = TRUE), rep(c(6, 5), 4))
  }
})

test_that('row setup runs once before predicates and even on rows without statistics', {
  for (direction in c('h', 'v')) {
    s <- row_mutation_session(direction, c('mutate(x = x + 1)\n- - -\nsex == 1',
      'mutate(x = x + 1)', 'TRUE', 'mutate(x = x + 1)\nTRUE'),
      stats = c('mean(x)', NA, 'mean(x)', 'mean(x)'))
    s$hh$x <- 1
    s$out_glob$filter_row$calculation <- 'mutate(x = x + 1)'
    result <- row_mutation_values(s)
    if (direction == 'h') result <- result[c(1:2, 5:8)]
    expect_equal(result, c(3, NaN, 4, 4, 5, 5))
    expect_equal(s$hh$x, rep(1, 3))
  }
})

test_that('vertical row setup is evaluated on the full filtered population before column predicates', {
  s <- row_mutation_session('v', 'mutate(z = n())\n- - -\nz == 3')
  expect_equal(row_mutation_values(s), c(6, 5))
  s$out_glob$tab_c$col_lgc <- c('z == 3', 'z == 3 & sex == 2')
  expect_equal(row_mutation_values(s), c(6, 5))
})

test_that('horizontal row mutations restart independently for column calculation and filter stages', {
  s <- row_mutation_session('h', c('mutate(x = x + 1)\nTRUE', 'mutate(x = x + 1)\nTRUE'),
    stats = 'mean(x)', columns = rep('TRUE', 4))
  s$hh$x <- 1
  s$out_glob$filter_row$calculation <- 'mutate(x = x + 1)'
  s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row,
    tibble::tibble(row_index = 4L, col_index = 4:6, df = 'hh',
      filter_condition = c(NA, 'filter(sex == 2)', 'unfilter()'),
      calculation = c('mutate(x = x * 10)', NA, NA), weight = 'w'))
  expect_equal(row_mutation_values(s), c(3, 21, 3, 3, 4, 22, 4, 4))
})

test_that('standalone cell calculation executes row mutations as well', {
  s <- row_mutation_session('h', c('mutate(z = if_else(sex == 2, 1, 0))\n- - -\nz == 1', 'z == 0'))
  cols <- col_condition_f(s$out_glob$tab_c)
  result <- calc_cells(s$hh, s$out_glob$tab_r, cols, s$out_glob$tab, 'w', TRUE)
  expect_equal(result$value, c(5, 5, 1, 0))
})
