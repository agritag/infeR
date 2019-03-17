#' @title Read data with multi-byte separator
#' 
#' @description Read in data set which is seperated with multi-byte symbol. 
#' This function creates workaround for \emph{data.table::fread} function by 
#' replacing multi-byte symbol with single-byte symbol and then uses built-in 
#' functions. 
#' @param filepath character string. Desciption to connection object
#' @param sep string. Description of multi-byte seperator. Single-byte seperator
#' also can be used.
#' @param sbyte_sep string. Single byte seperator that will be used to replace
#' multi-byte seperator. Default is \emph{\t}. 
#' @param header logical. Does data contain header.
#' @return data.frame of read in data set
#' @usage read_data(filepath, sep, sbyte_sep = "\t", header = TRUE)
#' @export
#' @import dplyr
#' @importFrom data.table fread
#' @examples 
#' #MovieTweetings dataset from sidooms github project 
#' df <- read_data(filepath=paste0("https://raw.githubusercontent.com/sidooms/",
#'                                 "MovieTweetings/master/latest/movies.dat"),
#'           sep="::", sbyte_sep = "~", header=FALSE) 

read_data <- function(filepath, sep, sbyte_sep = "\t", header = TRUE){
  # read file line by line
  df <- base::readLines(filepath) %>%
    # replace multiple-byte seperator to tab
    base::gsub(sep, sbyte_sep, .) %>%
    # read in data as data.table
    data.table::fread(text = ., sep = sbyte_sep, header = header) %>%
    # convert to data.frame
    base::as.data.frame()
  return(df)
}
