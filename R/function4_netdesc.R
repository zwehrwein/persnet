#' @title
#' Network descriptives of persnet data object
#'
#' @description
#' Computes network statistics of persnet data object
#'
#' @details
#'
#' @param persnet_dataframe
#'
#' @export
net_desc_pnd <- function(persnet_dataframe) {
  persnet_list_igra <- organize_pnd(persnet_dataframe)
  no_null_persnet_list_igra <- persnet_list_igra[!sapply(persnet_list_igra,is.null)]

  net_desc_list <- list()

  net_desc_list$record_id <- persnet_dataframe$record_id
  #net_desc_list$alter_count <- funct_count_more_names_whole_dataframe(persnet_dataframe)
  net_desc_list$alter_count <- funct_calculate_size_dataframe(persnet_dataframe)
  net_desc_list$edges = unlist(lapply(no_null_persnet_list_igra,gsize))
  net_desc_list$density = unlist(lapply(no_null_persnet_list_igra,egoless_density))
  net_desc_list$net_constraint = unname(unlist(lapply(no_null_persnet_list_igra,funct_constraint_ego)))
  net_desc_list$net_effect_size = unlist(lapply(no_null_persnet_list_igra,effective.size))
  net_desc_list$max_degree = unlist(lapply(no_null_persnet_list_igra,max.degree))
  net_desc_list$min_degree = unlist(lapply(no_null_persnet_list_igra,min.degree))
  net_desc_list$avg_path_length = unlist(lapply(no_null_persnet_list_igra,average.path.length))

  return(
    as_tibble(net_desc_list)
  )
}
