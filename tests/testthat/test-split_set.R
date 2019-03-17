require(infeR)
context("test-split_set")

testthat::test_that("Input data frame validation", {

  # Error due to empty data frame 
  testthat::expect_error(
    split_data_validation({data.frame(target_col=c(rep("A", 3))) %>% 
        dplyr::filter(target_col == "B")
      }, 
      target_column = "target_col", n=2)
    )
  
  # Error due to non-existing target column
  testthat::expect_error(
    split_data_validation(data.frame(target_column=c(rep("A", 3))), 
                          target_column = "target_col", n=2)
  )
  
  # Error due to not enough observations
  testthat::expect_error(
    split_data_validation(data.frame(column1 = rep(TRUE,10),
                                     target_col = c(rep("A",55), rep("B", 5)),
                                     stringsAsFactors = FALSE), 
                          target_column = "target_col", n=10)
  )
  
})

testthat::test_that("Returns n data sets",{
  df <- data.frame(column1 = rep(TRUE,50),
                   target_col = c(rep("A",40), rep("B", 10)),
                   stringsAsFactors = FALSE)
  
  # Splitting into 4 parts
  testthat::expect_equal( 4, 
                          { split_data(df=df, target_column = "target_col", 
                                       ratio = c(0.1, 0.4, 0.3, 0.2)) %>%
                              length()
                          }
  )
  
  # Splitting into 4 parts
  testthat::expect_equal( 2, 
                          { split_data(df=df, target_column = "target_col", 
                                       ratio = c(0.9, 0.1)) %>%
                              length()
                          }
  )

})


testthat::test_that("Equal distribution",{
  df <- data.frame(column1 = rep(TRUE,50),
                   target_col = c(rep("A",40), rep("B", 10)),
                   stringsAsFactors = FALSE)

  df_list <- split_data(df=df, target_column = "target_col", 
                       ratio = c(0.1, 0.4, 0.3, 0.2))
  
  # Calculate distributions to all dataframes in the list 
  # where first element is original data frame
  distribution_df_list <- lapply(c(list(df), df_list), function(df){
    table(df$target_col) %>% 
      as.data.frame() %>%
      dplyr::mutate(distribution = Freq/(sum(Freq))) %>%
      dplyr::arrange(Var1) %>%
      dplyr::select(distribution)
  })
  
  testthat::expect_true(
    # Test that all data frames are identical to first data frame in the list
    all(sapply(distribution_df_list, identical, distribution_df_list[[1]]))
  )
  
})