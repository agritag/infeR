#' @title Split data by target's distrubtion
#' 
#' @description Split data set into \emph{n} sets based on target's distribution
#' with or without observation repetition. Each set should contain at least one
#' observation per class to keep needed distribution.
#' 
#' @param df data.frame 
#' @param target_column string; name of the column which is assumed as target
#' column. \code{df} must contain this column.
#' @param ratio numeric vector; represents proportions how to split data. 
#' \code{length(ratio) == n}.
#' @param replace boolean; sampling with or without replacement.
#' @return list of \emph{n} data frames with equal target distribtuion  
#' @usage get_data_types(df = df, target_column = "some_column")
#' @export
#' @import dplyr
#' @importFrom data.table .SD setDT transpose 
#' @importFrom tibble rownames_to_column
#' @examples 
#' df <- data.frame(column1 = rep(TRUE,50),
#'                  column2 = c(LETTERS[1:25], LETTERS[26:2]),
#'                  column3 = seq(1,50),
#'                  column4 = c(rep("a",45), rep("b",5)),
#'                  column5 = seq(1,50,by=1),
#'                  target_col = c(rep("A",25), rep("B", 25)),
#'                  stringsAsFactors = FALSE)
#'split_data(df=df, target_column = "target_col", ratio = c(0.1, 0.4, 0.3, 0.2))

split_data <- function(df, target_column, ratio=c(0.5, 0.5), replace = FALSE){
  # validation
  split_data_validation(df, target_column, n = length(ratio))
  
  # initialize output list
  output_df_list <- list()
  
  # initialize vector of taken rows
  taken_rows <- c()
  
  # add rownames as vector
  df <- df %>%
    tibble::rownames_to_column('rowid')
  
  output_df_list <- lapply(ratio, FUN = function(element){
    
    # stratified sampling by target column
    df_stratified <- df %>%
      dplyr::group_by_(.dots = target_column) %>%
      dplyr::sample_frac(size = element, replace = replace) %>%
      ungroup() %>%
      as.data.frame()
    
    # update output list with result data frame without rowid column
    output_df_list <- c(output_df_list, 
                        list({df_stratified %>% dplyr::select(-rowid)})
    )
    
    # update vector
    taken_rows <- as.numeric(df_stratified$rowid)
    
    # update dataframe 
    df <- df %>%
      dplyr::filter(!(rowid %in% taken_rows))
    
    return(output_df_list)
  })
  
  # remove one level from list
  output_df_list <- unlist(output_df_list, recursive = FALSE)
  
  return(output_df_list)
}


#' @title Validate data frame before splitting
#' 
#' @param df data.frame 
#' @param target_column string; name of the column which is assumed as target
#' column. \code{df} must contain this column.
#' @param n integer specifying how many sets should be created. 
#' @usage split_data_validation(df = df, target_column = "target_col", n=10)
#' @import dplyr
#' @seealso \code{\link{split_data}} for splitting data set into \emph{n} parts.

split_data_validation <- function(df, target_column, n){
  # data.frame dimension validation
  if(dim(df)[1]==0){
    stop("Empty data frame")
  }
  
  # data.frame validation
  if(!(target_column %in% names(df))){
    stop("Target column does not exist in given data frame")
  }
  
  # observation count validiton
  m <- df %>%
    dplyr::select(dplyr::one_of(target_column)) %>% #select target column
    table() %>% #Getting distribution between classes 
    as.data.frame() %>%
    dplyr::filter(Freq == base::min(Freq)) %>% #find smallest category class
    dplyr::select(Freq) %>%
    unlist() %>% .[1] %>% as.numeric()
  
  if(m<n){
    stop(paste0("Cannot split data frame in n=", n, " parts as smallest ",
                "category in target column containts m=", m, " obeservations"))
  }
  
}