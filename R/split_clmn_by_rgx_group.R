#' @title Split column by captured regex groups 
#' 
#' @description Split given column by captured regular expression groups. 
#' Function returns data frame with as many columns as groups in passed regular 
#' expression.
#' 
#' @param column vector; column from some dataframe that needs to be 
#' split by groups from regular expression.  
#' @param regex character; string containing regular expression.
#' @return data frame with columns as many as groups passed in regex.
#' @usage split_clmn_by_rgx_group(column = column, regex = regex)
#' @export
#' @import dplyr
#' @importFrom stringi stri_match_all
#' @examples 
#' df <- data.frame(column = c("a:1, b:1, c:1", "a:1, b:2, c:1", 
#'                             "a:1, b:3, c:1", "a:1, b:4, c:1"),
#'                             stringsAsFactors = FALSE)
#' regex <- ".*b:(.*?), .*"
#' split_clmn_by_rgx_group(column = df$column, regex = regex)

split_clmn_by_rgx_group <- function(column, regex){
  
  output_df <- 
    # split column by regex groups 
    stringi::stri_match_all(str = column,
                            regex = regex) %>%
    # combine list of matrices into one matrix
    do.call(rbind, .) %>%
    # convert to data frame for convenience
    as.data.frame() %>%
    # drop full match
    dplyr::select(-c(1))
  
  return(output_df)

}

