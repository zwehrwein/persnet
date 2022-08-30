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
funct_count_more_names <- function(persnet_datarow) {

  specified_names <- persnet_datarow %>% select(name1:name15)
  list_specified_names <- unlist(strsplit(as.character(specified_names),split = ","))

  string_more_names1 <- persnet_datarow %>% select(contains("more_names_1"))
  string_more_names2 <- persnet_datarow %>% select(contains("more_names_2"))
  string_more_names3 <- persnet_datarow %>% select(contains("more_names_3"))

  list_more_names1 <- unlist(strsplit(as.character(string_more_names1),split = ","))
  list_more_names2 <- unlist(strsplit(as.character(string_more_names2),split = ","))
  list_more_names3 <- unlist(strsplit(as.character(string_more_names3),split = ","))

  list_all_names <- c(list_specified_names,list_more_names1,list_more_names2,list_more_names3)
  list_all_names

  #remove na
  list_all_names[list_all_names=="NA"] <- NA
  list_all_names <- list_all_names[!is.na(list_all_names)]

  #remove spaces
  list_all_names <- gsub(" ", "", list_all_names)

  #lowercase
  list_all_names <- tolower(list_all_names)

  #remove duplicates
  list_all_names <- unique(list_all_names)

  return(length(list_all_names))
}
funct_count_more_names_whole_dataframe <- function(persnet_dataframe) {
  list_more_names_conts <- list()
    for(i in 1:nrow(persnet_dataframe)) {
      list_more_names_conts[[i]] <- funct_count_more_names(persnet_dataframe[i,])
    }
  return(unlist(list_more_names_conts))
}
funct_constraint_ego <- function(tg_graph) {
  constraint_scores <- constraint(tg_graph)
  return(constraint_scores[1])
}
effective.size <- function(tg_graph) {
  n <- vcount(tg_graph)
  t <- ecount(tg_graph)
  return(n - (2 * t) / n)
}
egoless_density <- function(tg_graph) {
  return(graph.density(remove_ego_from_igraph(tg_graph)))
}
max.degree <- function(tg_graph) {
  return(max(degree(remove_ego_from_igraph(tg_graph))))
}
min.degree <- function(tg_graph) {
  return(min(degree(remove_ego_from_igraph(tg_graph))))
}
remove_ego_from_igraph <- function(tg_graph){
  tg_graph_egoless <- tg_graph %>% delete_vertices("EGO")
  return(tg_graph_egoless)
}
