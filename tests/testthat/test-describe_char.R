require(infeR)
context("test-describe_char")

testthat::test_that("Returns data frame with summary metrics", {
  # For testing purposes OrchardSprays data set will be used
  df <- OrchardSprays %>%
    dplyr::mutate(rowpos = as.factor(rowpos),
                  colpos = as.factor(colpos))
  
  # Find count of numeric columns in data frame
  char_col_count <- df %>%
    dplyr::select_if(function(col) {is.factor(col) || is.character(col)}) %>%
    base::names() %>%
    length()
  
  # Currently function outputs 4 metrics + one variable column
  metric_count <- 4 + 1
  
  # Assign output of function to new variable
  describe_df <- describe_char(df)
  
  # Tests 
  testthat::expect_true(describe_df %>% is.data.frame())
  testthat::expect_equal(dim(describe_df), c(char_col_count, metric_count))
})
