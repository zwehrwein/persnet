#' @title
#' Organizer
#'
#' @description
#' Organize persnet csv file into igraph objects
#'
#' @details
#'
#' @param persnet_list_igra
#'
#' @export
net_describe_pnd <- function(persnet_list_igra) {
  no_null_persnet_list_igra <- persnet_list_igra[!sapply(persnet_list_igra,is.null)]

  net_desc_list <- list()
  net_desc_list$nodes = unlist(sapply(no_null_persnet_list_igra,gorder))
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
