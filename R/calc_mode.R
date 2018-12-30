#' @title Mode of given vector's values
#' 
#' @description  Calculate mode of given vector's valies. 
#' @param data vector with given data
#' @param return_multiple boolean. If multiple mode's exist, should function
#' return all of them or only one. Default is TRUE. 
#' @param na.rm boolean. Should missing data be included or omitted. Default is
#' FALSE.
#' @return vector with mode. 
#' @import dplyr
#' @importFrom data.table .N
#' @export
#' @examples
#' #Example of factor column and return.multiple=FALSE
#' calc_mode(iris$Species, return.multiple=FALSE)
#' #Example of na.rm = FALSE
#' calc_mode(c(NA, NA, NA, 2), na.rm = FALSE)

calc_mode <- function(data, return.multiple = TRUE, na.rm = FALSE) {
  # wheter data frame or vector is passed, it is converted to data table
  data <- as.data.frame(data) %>%
    data.table::setDT(.)
  
  # omit missing data if indicated
  if(na.rm){
    data <- stats::na.omit(data)
  }
  
  # get all values with maximum frequency from frequency table 
  freq_table <- data[, .N, by=names(data)] %>%
    dplyr::filter(N == base::max(N)) %>%
    base::as.data.frame()
  
  # outputting
  if(return.multiple){
    # return all elements with max. frequency
    return(freq_table[,1] %>% as.character())
  } else {
    # return first element with max. frequency
    return(freq_table[1,1] %>% as.character())
  }
}
