context("get_correlation")
library(tibble)
library(dplyr)

df <- tibble(
  vec1 = 1:10,
  vec2 = seq(from = 5, to = 50, by = 5),
  vec3 = c(1:9, NA),
  vec4 = rep(c("blue", "green"), 5)
)

grdf <- df %>% group_by(vec4)

group_cor <- grdf %>% summarize(cor = cor(vec1, vec2))


test_that("arguments are appropriate", {
  # vec1 is not a df. vec1 is one column in the df
  expect_error(
    get_correlation(vec1,
                    formula = vec1 ~ NULL
    )
  )
  
  expect_error(
    df %>%
      get_correlation(formula = vec1 ~ NULL)
  )
  
  expect_error(
    df %>%
      get_correlation(formula = NULL ~ vec2)
  )
  
  expect_error(
    df %>%
      get_correlation(formula = vec2)
  )
})

test_that("variables are in data frame", {
  expect_error(
    mtcars %>%
      get_correlation(formula = mpg2 ~ disp)
  )
  
  expect_error(
    mtcars %>%
      get_correlation(formula = mpg2 ~ disp2)
  )
  
  expect_error(
    mtcars %>%
      get_correlation(formula = mpg ~ disp2)
  )
  
  expect_equal(
    object = mtcars %>%
      get_correlation(formula = mpg ~ disp) %>%
      pull(),
    expected = cor(mtcars$mpg, mtcars$disp)
  )
  
  expect_error(
    mtcars %>%
      get_correlation(formula = mpg ~ disp + hp)
  )
})


test_that("missing data handled correctly", {
  expect_true(
    df %>%
      get_correlation(formula = vec1 ~ vec3) %>%
      pull(cor) %>%
      is.na()
  )
  
  expect_equal(
    df %>%
      get_correlation(formula = vec1 ~ vec3, na.rm = TRUE) %>%
      pull(cor),
    1
  )
  
  expect_equal(
    df %>%
      get_correlation(formula = vec1 ~ vec3, use = "complete.obs") %>%
      pull(cor),
    1
  )
})

test_that("test correlations", {
  # test correlations on grouping variables
  expect_equal(
    object = grdf %>% 
      get_correlation(formula = vec1 ~ vec2),
    expected = group_cor
  )
  
  # test correlations without grouping
  expect_equal(
    object = df %>% 
      get_correlation(formula = vec1 ~ vec2),
    expected = tibble(cor = cor(df$vec1, df$vec2))
  )
})