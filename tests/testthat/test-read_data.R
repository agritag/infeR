require(infeR)
context("test-read_data")

test_that("Read in DESCRIPTION file", {
  # Seperator is `: ` 
  df <- read_data("../../DESCRIPTION", sep=": ", header=FALSE)
  
  # Test
  testthat::expect_equal({df[1,] %>% as.data.frame()},
                         data.frame(V1 = "Package", V2 = "infeR",  
                                    stringsAsFactors = FALSE)
                         )
  ## Expect warning due to last line of description file
  testthat::expect_warning(read_data("../../DESCRIPTION", sep=": ", 
                                     header=FALSE)
                           )
})
