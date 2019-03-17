require(infeR)
context("test-read_data")

test_that("Read in DESCRIPTION file", {
  
  url <- "https://raw.githubusercontent.com/agritag/infeR/master/DESCRIPTION"
  
  # Seperator is `: ` 
  df <- read_data(url, sep=": ", header=FALSE)
  
  # Test
  testthat::expect_equal({df[1,] %>% as.data.frame()},
                         data.frame(V1 = "Package", V2 = "infeR",  
                                    stringsAsFactors = FALSE)
                         )
  ## Expect warning due to last line of description file
  testthat::expect_warning(read_data(url, sep=": ", 
                                     header=FALSE)
                           )
})
