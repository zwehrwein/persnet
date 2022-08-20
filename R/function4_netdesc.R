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
  #historical code only computed nodes in igraph object itself
  #net_desc_list$nodes = unlist(sapply(no_null_persnet_list_igra,gorder))
  igra_nodes_counts <- unlist(sapply(no_null_persnet_list_igra,gorder))
  more_names_counts <- funct_count_more_names_whole_dataframe(persnet_dataframe)
  network_size_counts <- igra_nodes_counts + more_names_counts
  net_desc_list$size <- network_size_counts
  net_desc_list$edges = unlist(lapply(no_null_persnet_list_igra,gsize))
  net_desc_list$density = unlist(lapply(no_null_persnet_list_igra,graph.density))
  net_desc_list$net_constraint = unname(unlist(lapply(no_null_persnet_list_igra,funct_constraint_ego)))
  net_desc_list$net_effect_size = unlist(lapply(no_null_persnet_list_igra,effective.size))
  net_desc_list$max_degree = unlist(lapply(no_null_persnet_list_igra,max.degree))
  net_desc_list$min_degree = unlist(lapply(no_null_persnet_list_igra,min.degree))
  net_desc_list$avg_path_length = unlist(lapply(no_null_persnet_list_igra,average.path.length))

  return(
    as_tibble(net_desc_list)
  )
}