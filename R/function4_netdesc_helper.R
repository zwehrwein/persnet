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
# function_count_all_names <- function(persnet_datarow) {
#
#   #specified columns and those that are then removed later
#   select_list_names <- persnet_datarow %>% select(name1, name2, name3, name4, name5,
#                                          name6, name7, name8, name9, name10, name11,
#                                          name12, name13, name14, name15)
#
#   list_named_df <- as.data.frame(t(select_list_names))
#   list_named_df
#
#   #which names should be removed
#   keep_names_list <- persnet_datarow %>% select(name_1:name_15)
#   keep_names_list_df <- data.frame(t(keep_names_list))
#   colnames(keep_names_list_df) = "Value"
#
#   names_combined_df <- cbind(list_named_df, keep_names_list_df)
#   names_combined_df[complete.cases(names_combined_df), ]
#   names_combined_keep <- split(names_combined_df, names_combined_df$Value == 1)$`TRUE`
#
#   #specified_names <- persnet_datarow %>% select(name1, name2, name3, name4, name5,
#   #                                            name6, name7, name8, name9, name10,
#   #                                            name11, name12, name13, name14, name15)
#
#   list_specified_names <- unlist(strsplit(as.character(names_combined_keep$V1),split = ","))
#
#   #more names columns
#   string_more_names1 <- persnet_datarow %>% select(contains("more_names_1"))
#   string_more_names2 <- persnet_datarow %>% select(contains("more_names_2"))
#   string_more_names3 <- persnet_datarow %>% select(contains("more_names_3"))
#
#   list_more_names1 <- unlist(strsplit(as.character(string_more_names1),split = ","))
#   list_more_names2 <- unlist(strsplit(as.character(string_more_names2),split = ","))
#   list_more_names3 <- unlist(strsplit(as.character(string_more_names3),split = ","))
#
#   list_all_names <- c(list_specified_names,list_more_names1,list_more_names2,list_more_names3)
#
#   #remove na
#   list_all_names[list_all_names=="NA"] <- NA
#   list_all_names <- list_all_names[!is.na(list_all_names)]
#
#   #remove spaces
#   list_all_names <- gsub(" ", "", list_all_names)
#
#   #lowercase
#   list_all_names <- tolower(list_all_names)
#
#   #remove duplicates
#   list_all_names <- unique(list_all_names)
#
#   return(length(list_all_names))
# }
# funct_count_more_names_whole_dataframe <- function(persnet_dataframe) {
#   list_more_names_conts <- list()
#     for(i in 1:nrow(persnet_dataframe)) {
#       list_more_names_conts[[i]] <- function_count_all_names(persnet_dataframe[i,])
#     }
#   return(unlist(list_more_names_conts))
# }
funct_calculate_size_dataframe <- function(persnet_dataframe) {
  calculate_size <- function(x) {
    #credit to Liam Bright
    ##########
    # Function: Creates a network_size variable that takes into account any names
    #           written in the extra names boxes
    # Inputs: x = Variable that stores the dataset
    # Ouputs: network_size variable for each ID
    ##########
    #first select all names put in the first 15 columns
    names_first_15 <- persnet_dataframe %>% select(name1, name2, name3, name4, name5,
                                                   name6, name7, name8, name9, name10, name11, name12, name13, name14, name15)

    #next, select the names for id x
    names_first_15 <- names_first_15[x, ]

    #create data frame and transpose it to make it easier to manage
    names_first_15 <- as.data.frame(t(names_first_15))

    #change the column name
    colnames(names_first_15) <- c("Names")

    #select the keep/remove designation, stored as variables "name_1" to "name_15"
    #  for each of the first 15 names
    keep_names <- persnet_dataframe %>% select(name_1:name_15)
    keep_names <- keep_names[x, ]

    #change colnames to numbers 1:15, so that it is easier to do rbind
    colnames(keep_names) <- c(1:15)

    #input the data into a data frame and transpose it
    keep_names <- data.frame(t(keep_names))

    #change the name of the column to "Value"
    colnames(keep_names) = "Value"

    #combine "names_first_15" (the first 15 names entered) and "keep_names" (the
    #  keep/remove designation for each of the first 15 names) using cbind function
    names_combined <- cbind(names_first_15, keep_names)

    #remove any row that contain NA in names_combined
    names_combined <- names_combined[complete.cases(names_combined), ]

    #split names_combined into names designated as "keep" (Value = 1) and
    #  names designated as "remove" (Value = 0)
    names_combined_keep <- split(names_combined, names_combined$Value == 1)

    # Select only the names designated as $`TRUE` ("keep")
    names_combined_keep <- names_combined_keep$`TRUE`

    #Change all characters into Uppercase
    names_combined_keep <- toupper(names_combined_keep$Names)

    #Remove any spaces
    names_combined_keep <- gsub(" ", "", names_combined_keep)

    #Make names_combined_keep into a data frame to make it easier to manage
    names_combined_keep <- data.frame(names_combined_keep)
    colnames(names_combined_keep) <- "Names"

    #Now, take all of the names from the 3 extra names boxes
    #  Strsplit : split names based on coma saparated value and change them into
    #  characters.
    names_box1 <- strsplit(as.character(persnet_dataframe$more_names_1)[x],
                           split = ",")
    names_box2 <- strsplit(as.character(persnet_dataframe$more_names_2)[x],
                           split = ",")
    names_box3 <- strsplit(as.character(persnet_dataframe$more_names_3)[x],
                           split = ",")

    #Unlist names_box1:names_box3 and create a vector of names for each extra names
    #  box
    names_box1 <- as.vector(unlist(names_box1, use.names = FALSE))
    names_box2 <- as.vector(unlist(names_box2, use.names = FALSE))
    names_box3 <- as.vector(unlist(names_box3, use.names = FALSE))

    #combine the 3 extra names vectors into list so that we can combine
    #  names_box1:3 into one vector
    names_box <- list(names_box1, names_box2, names_box3)

    #make the names_box list into a vector
    names_box <- Reduce(c, names_box)

    #remove "NA" in names_box
    names_box <- names_box[!is.na(names_box)]

    #Remove Spaces in names_box
    names_box <- gsub(" ", "", names_box)

    #Change all character in names_box to uppercase
    names_box <- toupper(names_box)

    #Remove duplicates values in names_box vector
    names_box <- unique(names_box)

    #makes names_box into a data frame and change the column name to "Names"
    #  to make it easier to merge with names_combined_keep
    names_box <- data.frame(names_box)
    colnames(names_box) <- "Names"

    # Merge unique names from boxes with unique names of first 15 and
    #remove duplicates between them
    #  Keep this order. Placing names_combined_keep first preserves any duplicate
    #  names that both designated as "keep" by the participant
    names_network <- merge(names_combined_keep,names_box,by = c("Names"), all = TRUE)

    # convert names_network into a vector
    names_network <- as.vector(t(names_network))

    #calculate the total network size
    total_size <- length(names_network)
    return(total_size)
  }
  return(unlist(lapply(1:nrow(persnet_dataframe), calculate_size)))
}



funct_constraint_ego <- function(tg_graph) {
  constraint_scores <- constraint(tg_graph)
  return(constraint_scores[1])
}
#effective.size <- function(tg_graph) {
#  #adopted from influenceR package
#  if (!igraph::is_igraph(tg_graph)) {
#    stop("Not a graph object")
#  }
#  A <- igraph::get.adjacency(tg_graph)#, attr='weight')   # This will be sparse, which is great.
#  S <- Matrix::crossprod(A)       # S[i,j] = # of shared neighbors between i,j
#  Q <- A * S              # Q[i,j] = # of shared neighbors if i and j are neighbors, 0 else
#  qsum <- Matrix::rowSums(Q)
#  deg <- Matrix::rowSums(A)
#  ens <- deg - (qsum / deg)
#  ens[is.nan(ens)] <- 0 # If a vertex has no neighbors, make its ENS 0
#  names(ens) <- igraph::V(tg_graph)$name
#  ens_ego <- ens[names(ens)=="EGO"]
#  return(ens_ego)
#  }

effective.size <- function(tg_graph){
  #imported from 'egonet' package
  dati <- get.adjacency(tg_graph,attr='weight')
  n <- dim(dati)[1]
  if(n < 2) return(NaN)
  Sj <- 0
  for( y in 2:n)          Sj <- Sj + (dati['EGO',y] + dati[y,'EGO'])
  ris <- 0
  for ( j in 2: n){
    cont1 <- setdiff(1:n,j)
    Vetmax <- rep(NA,(length(cont1)))
    for (k in setdiff(1:n,j)){ Vetmax[k] <- (dati[j,k] +dati[k,j])}
    massimo <- max(Vetmax,na.rm=T)
    sumPM <- 0
    for(f in setdiff(2:n,j) ){ #f=q in formula originale
      Piq <- (dati['EGO', f] + dati[f,'EGO'])/ Sj
      Mjq <- (dati[j,f] + dati[f,j])/ massimo
      sumPM <- sumPM +Piq * Mjq
    }
    ris <- ris + (1 - (sumPM))
  }
  ris
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
