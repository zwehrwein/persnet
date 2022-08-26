#' @title
#' Visualize
#'
#' @description
#' Visualize a persnet data object
#'
#' @details
#'
#' @param igra_object
#'
#' @import tidyverse
#' @import tidygraph
#' @import igraph
#' @import ggraph
#'
#' @examples
#'
#'
viz_grid_pnd_helper <- function(igra_object) {
  #this function visualizes an igraph object
  tg_plot <- as_tbl_graph(igra_object) %>%
  activate(edges) %>%
  mutate(
    strength_of_tie = ifelse(weight==1,"weak","strong")
  ) %>%
  ggraph(layout = 'focus',focus=1) +
  geom_edge_link(aes(

    color = strength_of_tie,
    linetype = strength_of_tie

  ),
  edge_width = .75,
  show.legend = F) +

  scale_edge_linetype_manual(
    values = c(
      "weak" = "dashed",
      "strong" = "solid"
    )
  ) +

scale_edge_colour_manual(
  values = c(
    "weak" = "#5B8FA8FF",
    "strong" = "#800000FF"
  )
) +
  geom_node_point(aes(
    color = factor(alter_dummy)),
    size=4,
    show.legend = F) +
  scale_colour_manual(values = c('black','grey66'))+
  theme_graph()
  return(tg_plot)
}
