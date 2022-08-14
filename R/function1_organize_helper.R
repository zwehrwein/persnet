#' @title
#' Organize helper functions. Not exported.
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
#' @import igraph
#'
#' @examples
#'
FUNCTION_TIBBLE_NAMES <- function(df_row_input) {
  #this function identifies the names given to alters in the persnet survey
  if ("name1" %in% colnames(df_row_input)) {
    names_first_15 <- df_row_input %>% select(name1, name2, name3, name4, name5,
                                              name6, name7, name8, name9, name10,
                                              name11,name12,name13,name14,name15)
    tibble_names_first_15 <- as_tibble(t(names_first_15),names="name")
    colnames(tibble_names_first_15)[1] <- "name"
    tibble_names_first_15 <- tibble_names_first_15 %>%
      mutate(alter = 1) %>%
      mutate(node_id = 2:16)
    return(tibble_names_first_15)
  } else {
    return(NULL)
  }
}

FUNCTION_REMOVE_NAME1 <- function(col_name) {
  #this function strips "name1" from a variable name
  return(gsub('name1','',col_name))
}

FUNC_CREATE_ATTRIBUTE <- function(col_name,df_row_input2) {
  #takes in the name of a column and a slice of a dataframe
  #returns a list of the values associated with a given
  #alter variable name
  return(
    unname(
      unlist(
        df_row_input2 %>% 
          select(contains("name")) %>% 
          select(contains(col_name))))
  )
}

FUNCT_WRANGLE_ROW <- function(df_raw_input3){
  #this block of code creates the igraph object from a row of a persnet dataframe
  edges_variables <- select(df_raw_input3, tie1:a_tie105)
  edges_values <- as.integer(edges_variables)
  mat <- matrix(NA,16,16)
  diag(mat) <- 0
  mat[lower.tri(mat)] <- edges_values
  mat <- t(mat) 
  mat[lower.tri(mat)] <- edges_values
  colnames(mat) <- rownames(mat) <- c("EGO", "ALT1", "ALT2", "ALT3",
                                      "ALT4", "ALT5", "ALT6", "ALT7", 
                                      "ALT8", "ALT9", "ALT10", "ALT11",
                                      "ALT12", "ALT13", "ALT14", "ALT15")
  mat <- mat[(!colSums(mat, 1) == 0), (!colSums(mat, 1) == 0)]
  igra <- graph_from_adjacency_matrix(mat,mode='undirected',weighted=T)
  if (length(colnames(mat))==0){
    return(NULL)
  }else{
    V(igra)$node_id <- colnames(mat)
    V(igra)$alter_dummy <- ifelse(colnames(mat)=='EGO',0,1)
    return(igra)
  }
}