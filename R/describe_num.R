#' @title Describe numeric columns
#' 
#' @description  Descriptive statistics like missing data ratio, median, 
#' skewness and others of numeric columns of passed data frame.
#'
#' @param dataframe data.frame 
#' @return data.frame of descriptive statistics
#' @import dplyr
#' @importFrom data.table rbindlist setDT .SD transpose
#' @importFrom moments skewness kurtosis
#' @importFrom tibble rownames_to_column
#' @export
#' @examples 
#' describe_num(iris)

describe_num <- function(df){
  # find all numeric columns in df
  num_columns <- df %>%
    dplyr::select_if(is.numeric) %>%
    base::names()
  
  if(length(num_columns)>0){
    # Select only numerical columns of data frame
    df_num <- df %>%
        dplyr::select(dplyr::one_of(num_columns)) 
    
    # convert data frame to data table
    data.table::setDT(df_num)
    
    # initialize list for descriptive statistics
    summary_list <- list()
    
    ### Descriptive statistics -------------------------------------------------
    # Count of observations for all numerical columns
    summary_list[["length"]] <- 
      df_num[, lapply(.SD, function(x){base::length(x)})]
    # Unique values for all numerical columns
    summary_list[["unique"]] <- 
      df_num[, lapply(.SD, function(x){length(unique(x))})]
    # Calculate NA ratio for all numerical columns
    summary_list[["na_ratio"]] <-
      df_num[, lapply(.SD, function(x){sum(is.na(x))/nrow(df_num)})]
    # Minimum value for all numerical columns
    summary_list[["col_min"]] <- 
      df_num[,lapply(.SD, min, na.rm = T)]
    # Median for all numerical columns
    summary_list[["col_median"]] <- 
      df_num[,lapply(.SD, median, na.rm = T)]
    # Median for all numerical columns
    summary_list[["col_mean"]] <- 
      df_num[,lapply(.SD, mean, na.rm = T)]
    # Maximum value for all numerical columns
    summary_list[["col_max"]] <- 
      df_num[,lapply(.SD, max, na.rm = T)]
    # Skewness for all numeric columns
    summary_list[["col_skew"]] <- 
      df_num[,lapply(.SD, moments::skewness, na.rm = T)]
    # Kurtosis for all numeric columns
    summary_list[["col_kurtosis"]] <- 
      df_num[, lapply(.SD, moments::kurtosis, na.rm = T)]
    ### End --------------------------------------------------------------------
    
    # Bind descriptive statisrics together in a data frame
    summary_df <- data.table::rbindlist(summary_list) %>% 
      data.table::transpose() %>% 
      data.frame(row.names = num_columns) %>%
      tibble::rownames_to_column("Var.") %>%
      stats::setNames(c("Var.", "Obs.","Unique", "NA ratio", "Min.", 
                        "Median", "Mean", "Max.","Skewness", "Kurtosis")
                      )
    
    return(summary_df)
  } else {
    # print message 
    base::warning("No numeric columns in data.frame")
  }
} 
