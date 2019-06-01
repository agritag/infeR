require(infeR)
context("test-split_clmn_by_rgx_group")

test_that("test splitting functionality", {
  # toy data
  df_test <- data.frame(stringsAsFactors = FALSE,
                        column1 = c("a=1, b=1, c=2", "a=2, b=2, c=4",
                                    "a=1, b=2, c=3", "a=0, b=1, c=1"),
                        column2 = c("a:1 :: b:1 :: c:2", "a:2 :: b:2 :: c:4",
                                    "a:1 :: b:2 :: c:3", "a:0 :: b:1 :: c:1"))
  
  # few test regular expressions
  rgx1 <- ".*b=(.*),.*"
  rgx2 <- "(.*) :: (.*) :: (.*)"
  rgx3 <- "no pattern at all"
  
  # Testing ----
  # Dimensions of output
  testthat::expect_equal(c(4,1), 
                         dim(split_clmn_by_rgx_group(column = df_test$column1, 
                                                    regex = rgx1)
                              )
                         )
  testthat::expect_equal(c(4,3), 
                         dim(split_clmn_by_rgx_group(column = df_test$column2, 
                                                     regex = rgx2)
                         )
  )
  testthat::expect_equal(c(4,0), 
                         dim(split_clmn_by_rgx_group(column = df_test$column1, 
                                                     regex = rgx3)
                         )
  )
  
  # Value of output
  testthat::expect_equal("c(1, 2, 2, 1)", 
                         {split_clmn_by_rgx_group(column = df_test$column1, 
                                                  regex = rgx1) %>%
                             as.character()}
  )
  
  })
