#' @title Get data types
#' 
#' @description From given data frame get aggregated data frame of all column
#' data types and count
#' @param df data.frame 
#' @return data frame of data type summary
#' @usage get_data_types(df)
#' @export
#' @import dplyr
#' @importFrom data.table .SD setDT transpose 
#' @examples 
#' df <- data.frame(column1 = rep(TRUE,5),
#'                  column2 = LETTERS[1:5],
#'                  column3 = seq(1,5),
#'                  column4 = c(rep("a",4), "b"),
#'                  column5 = seq(1,1.4,by=0.1),
#'                  stringsAsFactors = FALSE)
#' df$column4 <- as.factor(df$column4)
#' get_data_types(df)

get_data_types <- function(df){
  # Convert data frame to data table
  data.table::setDT(df)
  
  # From given input data frame get count of column types
  df_summary <- df[, lapply(.SD, base::class)] %>%
    data.table::transpose() %>%
    dplyr::group_by(V1) %>%
    dplyr::summarize(count = n()) %>%
    stats::setNames(c("Type", "Count")) %>%
    base::as.data.frame()
  
  return(df_summary)
}
