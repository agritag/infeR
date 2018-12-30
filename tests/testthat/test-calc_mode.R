require(infeR)
context("test-calc_moder")

testthat::test_that("Test basic features of function", {
  #create basic vector for testing
  test_vector <- c(rep(NA,10), rep(1, 10), 2:4)
  
  #tests
  testthat::expect_equal(calc_mode(test_vector, 
                                   return.multiple = TRUE,
                                   na.rm = TRUE
                                   ),
                         c(1) %>% as.character() 
  )
  testthat::expect_equal(calc_mode(test_vector, 
                                   return.multiple = TRUE,
                                   na.rm = FALSE
                                   ),
                         c(NA, 1) %>% as.character()
  )
  testthat::expect_equal(calc_mode(test_vector, 
                                   return.multiple = FALSE,
                                   na.rm = TRUE),
                         c(1) %>% as.character()
  )
  testthat::expect_equal(calc_mode(test_vector, 
                                   return.multiple = FALSE,
                                   na.rm = FALSE),
                         NA %>% as.character()
  )
})
