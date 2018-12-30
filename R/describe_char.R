#' @title Describe nominal columns
#' 
#' @description  Descriptive statistics like missing value ratio and mode for 
#' character and factor columns. s
#' @param dataframe data.frame 
#' @return data.frame of descriptive statistics
#' @import dplyr
#' @importFrom data.table rbindlist setDT .SD transpose
#' @importFrom tibble rownames_to_column
#' @export
#' @examples 
#' describe_char(iris)

describe_char <- function(df){
  # find all character and factor columns in df
  char_columns <- df %>%
    dplyr::select_if(function(col) {is.factor(col) || is.character(col)}) %>%
    base::names()
  
  if(length(char_columns)>0){
    # Select only numerical columns of data frame
    df_char <- df %>%
      dplyr::select(dplyr::one_of(char_columns)) %>%
      dplyr::mutate_if(is.factor, as.character)
    
    # convert data frame to data table
    data.table::setDT(df_char)
    
    # initialize list for descriptive statistics
    summary_list <- list()
    
    ### Descriptive statistics -------------------------------------------------
    # Count of observations for all character columns
    summary_list[["length"]] <- 
      df_char[, lapply(.SD, function(x){base::length(x)})]
    # Unique values for all character columns
    summary_list[["unique"]] <- 
      df_char[, lapply(.SD, function(x){length(unique(x))})]
    # Calculate NA ratio for all character columns
    summary_list[["na_ratio"]] <-
      df_char[, lapply(.SD, function(x){sum(is.na(x))/nrow(df_char)})]
    # Calculate mode for all character columns
    summary_list[["mode"]] <-
      df_char[, lapply(.SD, function(x){paste0(infeR::calc_mode(x, na.rm=TRUE), 
                                               collapse="|")})]
    ### End --------------------------------------------------------------------
    
    # Bind descriptive statisrics together in a data frame
    summary_df <- data.table::rbindlist(summary_list) %>% 
      data.table::transpose() %>% 
      data.frame(row.names = char_columns) %>%
      tibble::rownames_to_column("Var.") %>%
      stats::setNames(c("Var.", "Obs.","Unique", "NA ratio", "Mode")
      )
    
    return(summary_df)
  } else {
    # print message 
    base::warning("No character or factor columns in data.frame")
  }
} 
