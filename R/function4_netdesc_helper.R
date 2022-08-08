#' @title
#' Organize net descriptive functions. Not exported.
#' 
#' @description 
#' 
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
funct_constraint_ego <- function(tg_graph) {
  constraint_scores <- constraint(tg_graph)
  return(constraint_scores[1])
}
effective.size <- function(tg_graph) {
  n <- vcount(tg_graph)
  t <- ecount(tg_graph)
  return(n - (2 * t) / n)
}
max.degree <- function(tg_graph) {
  return(max(degree(tg_graph)))
}
min.degree <- function(tg_graph) {
  return(min(degree(tg_graph)))
}