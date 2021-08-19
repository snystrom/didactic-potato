#' Get a dataframe of average signal from bw within regions
#'
#' @param bw 
#' @param regions 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
get_average_signal <- function(bw, regions, by = NULL) {
 
  #determine the regions size  
  size = unique(width(regions))
  
  
  if(!is.null(by)){ #this is a redundant step -- drop the if statement 
    #build matrix 
    matrix <- get_bw_matrix(bw, regions, by = by)
    #get names of each matrix and make a new df, melt the data, group, and calculate average
    df_list <- lapply(names(matrix), function(name){
      data.frame(matrix[name], check.names = F) %>%
        setNames(1:size) %>%
        reshape2::melt(variable.name = "position") %>%
        dplyr::mutate(id = name,
                      position = as.double(position)) %>%
        dplyr::group_by(position, id) %>%
        dplyr::summarise(mean = mean(value, na.rm = T))  
    })    
    
    signal.df <- dplyr::bind_rows(df_list)
    
    return(signal.df) 
    
  }else{
    matrix <- get_bw_matrix(bw, regions) 
    
    signal.df <- matrix %>% 
      data.frame() %>%
      setNames(1:size) %>%
      reshape2::melt(variable.name = "position") %>%
      dplyr::mutate(position = as.double(position)) %>%
      dplyr::group_by(position) %>%
      dplyr::summarise(value.mean = mean(value, na.rm = T))
    
    return(signal.df)
  }
}