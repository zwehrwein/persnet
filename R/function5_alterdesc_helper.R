#' @title
#' Compositional descriptives helper functions. Does not export to user
#' 
#' @description 
#' Organize persnet csv file into igraph objects
#'
#' @details 
#' 
#' @param persnet_dataframe
#'
#' @import tidyverse
#' @import tidygraph
#' @import igraph
#'
#' @examples
#'
RAW_DF_ROW2PER_DF_ROW <- function(persnet_dataframe) {
  redcap_id <- persnet_dataframe$record_id

  #support and negative
  support_cols <- persnet_dataframe %>% select(name1support:name15support)
  alter_support_per <- round(length(which(support_cols == 1)) /  #scales::percent(
                                length(which(support_cols == 1 | support_cols == 0)),2)
  neg_cols <- persnet_dataframe %>% select(name1neg:name15neg)
  alter_neg_per <- round(length(which(neg_cols == 1)) /
                               length(which(neg_cols == 1 | neg_cols == 0)),2)

  #types of support
  supptype_cols <- persnet_dataframe %>% select(name1supptype___1:name15supptype___5)

  supptype_emotional <- supptype_cols[, grepl("___1", names(supptype_cols))]
  support_emotional_per <-  round(length(which(supptype_emotional == 1)) /
                    length(which(supptype_emotional == 1 | supptype_emotional == 0)),2)

  supptype_advice <- supptype_cols[, grepl("___2", names(supptype_cols))]
  support_advice_per <-  round(length(which(supptype_advice == 1)) /
                    length(which(supptype_advice == 1 | supptype_advice == 0)),2)

  supptype_finance <- supptype_cols[, grepl("___3", names(supptype_cols))]
  support_finance_per <-  round(length(which(supptype_finance == 1)) /
                    length(which(supptype_finance == 1 | supptype_finance == 0)),2)

  supptype_health <- supptype_cols[, grepl("___4", names(supptype_cols))]
  support_health_per <-  round(length(which(supptype_health == 1)) /
                    length(which(supptype_health == 1 | supptype_health == 0)),2)

  supptype_camaraderie <- supptype_cols[, grepl("___5", names(supptype_cols))]
  support_camaraderie_per <-  round(length(which(supptype_camaraderie == 1)) /
                    length(which(supptype_camaraderie == 1 | supptype_camaraderie == 0)),2)

  #sex / race /race
  sex_cols <- persnet_dataframe %>% select(name1sex:name15sex)
  gender_male_per <- round(length(which(sex_cols == 1)) / length(which(sex_cols == 1 | sex_cols == 0)),2)
  race_cols <- persnet_dataframe %>% select(name1race:name15race)
  race_white_per <- round(length(which(race_cols == 2)) /
                            length(which(race_cols == 1 | race_cols == 2 | race_cols == 3 |
                                           race_cols == 4 | race_cols == 5 | race_cols == 99)),2)
  educ_cols <- persnet_dataframe %>% select(name1educ:name15educ)
  edu_hs_per <- round(length(which(educ_cols == 1 | educ_cols == 2)) /
                            length(which(educ_cols == 1 | educ_cols == 2 | educ_cols == 3 |
                                           educ_cols == 4 | educ_cols == 5 | educ_cols == 6 | educ_cols == 99)),2)

  #types of relations
  realtype_cols <- persnet_dataframe %>% select(name1relat___1:name15relat___77)

  relat_spouse <- realtype_cols[, grepl("___1", names(realtype_cols))]
  relation_spouse_per <-  round(length(which(relat_spouse == 1)) /
                                    length(which(relat_spouse == 1 | relat_spouse == 0)),2)

  relat_family <- realtype_cols[, grepl("___2", names(realtype_cols))]
  relation_family_per <-  round(length(which(relat_family == 1)) /
                                 length(which(relat_family == 1 | relat_family == 0)),2)

  relat_friend <- realtype_cols[, grepl("___3", names(realtype_cols))]
  relation_friend_per <-  round(length(which(relat_friend == 1)) /
                                  length(which(relat_friend == 1 | relat_friend == 0)),2)

  relat_advice <- realtype_cols[, grepl("___4", names(realtype_cols))]
  relation_advice_per <-  round(length(which(relat_advice == 1)) /
                                 length(which(relat_advice == 1 | relat_advice == 0)),2)

  relat_coworker <- realtype_cols[, grepl("___5", names(realtype_cols))]
  relat_coworker_per <-  round(length(which(relat_coworker == 1)) /
                                      length(which(relat_coworker == 1 | relat_coworker == 0)),2)

  relat_other <- realtype_cols[, grepl("___77", names(realtype_cols))]
  relat_other_per <-  round(length(which(relat_other == 1)) /
                                      length(which(relat_other == 1 | relat_other == 0)),2)
 #FLAG FOR AD: relattype6 MIA

  #distance / length / speak
  distance_cols <- persnet_dataframe %>% select(name1dist:name15dist)
  less15miles_per <- round(length(which(distance_cols == 1 | distance_cols == 2 | distance_cols == 3)) /
                            length(which(distance_cols == 1 | distance_cols == 2 | distance_cols == 3 |
                                           distance_cols == 4 | distance_cols == 5)),2)

  length_cols <- persnet_dataframe %>% select(name1length:name15length)
  fewer6years_per <- round(length(which(length_cols == 1 | length_cols == 2)) /
                             length(which(length_cols == 1 | length_cols == 2 | length_cols == 3
                                          | length_cols == 99)),2)

  speak_cols <- persnet_dataframe %>% select(name1speak:name15speak)
  speakweekly_per <- round(length(which(speak_cols == 1 | speak_cols == 2)) /
                             length(which(speak_cols == 1 | speak_cols == 2 | speak_cols == 3 |
                                            speak_cols == 4 | speak_cols == 99)),2)



  return(
    data.frame(
      redcap_id,

      alter_support_per,
      alter_neg_per,

      support_emotional_per,
      support_advice_per,
      support_finance_per,
      support_health_per,
      support_camaraderie_per,

      gender_male_per,
      race_white_per,
      edu_hs_per,

      relation_spouse_per,
      relation_family_per,
      relation_friend_per,
      relation_advice_per,
      relat_coworker_per,
      relat_other_per,

      less15miles_per,
      fewer6years_per,
      speakweekly_per

    )
  )
}