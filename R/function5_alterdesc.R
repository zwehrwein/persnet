#' @title
#' Compositional descriptives of persnet data object
#'
#' @description
#' Computes percentage compositions of various alter characteristics in a persnet data object
#'
#' @details
#'
#' @param persnet_list_igra
#'
#' @export
alter_desc_pnd <- function(persnet_df) {
  list_per_dfs <- list()
  for(i in 1:nrow(persnet_df)) {
    list_per_dfs[[i]] <- RAW_DF_ROW2PER_DF_ROW(persnet_df[i,])
  }
  return(as_tibble(bind_rows(list_per_dfs)))
}
