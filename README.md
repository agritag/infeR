# infeR
Inferring information from data. R package of boilerplate functions for (big) data analysis. 

## Testing 
* To run all tests in a package: `devtools::test()`. 
* Coverage report on all tests: `covr::report()`. Opens HTML link in browser with all function unit tests, respective coverage and other statistics. 
* Single function testing: `devtools::test_file("<function.R>")`. 
* Single function coverage: `covr::file_coverage("<function.R>", "<test-function.R>")`

## Documentation
* Updating NAMESPACE: `devtools::document()` 
