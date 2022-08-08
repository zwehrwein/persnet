#' @title
#' Organizer
#' 
#' @description 
#' Organize persnet csv file into igraph objects
#'
#' @details 
#' 
#' @param persnet_df
#'
#' @import tidyverse
#' @import tidygraph
#'
#' @export
organize_pnd <- function(persnet_df) {
  df_as_list <- persnet_df %>%
    mutate(index=1:n()) %>%
    group_split(index)
  return(
    lapply(df_as_list, FUNCT_WRANGLE_ROW)
  )
}