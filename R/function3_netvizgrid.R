#' @title
#' Visualize
#'
#' @description
#' Visualize a list of igraph objects
#'
#' @details
#'
#' @param list_igraph_objedts
#'
#' @import tidyverse
#' @import tidygraph
#' @import igraph
#' @import ggraph
#' @import gridExtra
#'
#' @examples
#'
#' @export
viz_grid_pnd <- function(list_igra_objects) {
  #this function visualizes a list of igraph objects in a grid.
  no_nulls_list_tgs <- list_igra_objects[!sapply(list_igra_objects,is.null)]
  number_cols <- ceiling(sqrt(length(no_nulls_list_tgs)))
  margin = theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

  no_nulls_list_tgs_sorted <- no_nulls_list_tgs[order(sapply(no_nulls_list_tgs,
                                                             redcap_id_extractor),
                                                      decreasing=F)]

  tg_plots <- list()
  for (i in seq_along(no_nulls_list_tgs_sorted)){
    tg_plot_iter <- viz_grid_pnd_helper(no_nulls_list_tgs_sorted[[i]])
    tg_plots[[i]] <- tg_plot_iter
  }
  length(tg_plots)
  #could be updated here to just include command from gridExtra
  return(grid.arrange(grobs = lapply(tg_plots, "+", margin),
                      ncol = number_cols))
}
