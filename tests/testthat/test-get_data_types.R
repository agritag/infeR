require(infeR)
context("test-get_data_types")

test_that("Counts data types in a data frame", {
  data(iris)

  # Test for returning same amount of columns
  testthat::expect_equal(
    # Sum up column count
    {get_data_types(iris) %>%
        dplyr::select(Count) %>%
        sum()}, 
    # Original column count
    ncol(iris)
    )
  
  # Test for finding all column classes
  testthat::expect_equal(  
    # Unique classes in df
    {lapply(iris, base::class) %>%
        unlist() %>%
        unique() %>%
        sort()
      }, 
    # Unique classes from function
    {get_data_types(iris) %>%
        dplyr::distinct(Type) %>%
        unlist() %>%
        as.vector() %>%
        sort()
      }
    )
})
