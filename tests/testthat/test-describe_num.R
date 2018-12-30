require(infeR)
context("test-describe_num")

testthat::test_that("Returns data frame with summary metrics", {
  # For testing purposes mtcars data set will be used
  attach(mtcars)
  
  # Find count of numeric columns in data frame
  num_col_count <- mtcars %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()
  
  # Currently function outputs 9 metrics + one variable column
  metric_count <- 9 + 1
  
  # Assign output of function to new variable
  describe_df <- describe_num(mtcars)
  
  # Tests 
  testthat::expect_true(describe_df %>% is.data.frame())
  testthat::expect_equal(dim(describe_df), c(num_col_count, metric_count))
})
