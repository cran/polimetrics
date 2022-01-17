#' Calculating Geographical Diffusion
#' 
#' @references
#' Berry, William D., Ringquist, Evan J., Fording, Richard C.,
#' and Hanson, Russell L.
#' (1998) 'Measuring Citizen and Government Ideology 
#' in the American States, 1960-93.' 
#' \emph{American Journal of Political Science} 42:327-348.
#' \doi{10.2307/2991759}.\cr
#' Soule, Sarah A., and King, Brayden G.
#' (2006) 'The Stages of the Policy Process 
#' and the Equal Rights Amendment, 1972-1982.' 
#' \emph{American Journal of Sociology} 111:1871-1909.
#' \doi{10.1086/499908}.\cr
#'
#' This function calculates the percent (or proportion) of geographically contiguous neighbors that have engaged in some \emph{event} (e.g. policy adoption) in a given year. This function can be applied to any unit of analysis and time level for any type of event.
#' @import dplyr
#' @importFrom stats time
#' @importFrom rlang .data
#' @param df data frame to read in. Data frame should include a variable that is a character list of each observation's neighbors.
#' @param id the grouping variable, usually states or counties
#' @param neighbors a variable that is a \code{character} list of each observation's neighbors. The elements of the character list of neighbors should be separated by commas. 
#' @param time the time variable, at which observations are measured.
#' @param status binary, user-defined measure of the status of policy or event in a state in a given year. \code{0} equates to \emph{policy has not yet occurred in the year, for the state}, \code{1} equates to \emph{policy event has already been adopted in the year, for the state} -- a value of \code{1} should exist for a state in the year it was adopted and every year thereafter). The example below relies on ERA ratification data from Soule and King (2006) <doi:10.1086/499908>, merged with ideology data from Berry et al. (1998) <doi:10.2307/2991759>, but the user should include the measure of adoption of their choice.
#' @param end logical (default set to \code{F}). When set to \code{end = T}, will calculate the percent of neighbors that had adopted policy by year-end. Otherwise, will calculate based on number of neighbors that had adopted the policy at year-start. 
#' @param keep logical (default set to \code{F}). When set to \code{end = T}, will include additional variables (\emph{number of neighbors} and \emph{number of neighbors that had adopted the policy}) in the updated data frame. 
#' @return This function updates the data frame with a new variable capturing the geographical diffusion score.
#' @examples
#' data <- Ideology_ERA
#'
#' geog.diffuse(data, state, neighbors, year, era_status)
#' @export



geog.diffuse <- function(df, id, neighbors, time, status, end=FALSE, keep=FALSE){
  df2 <- as.data.frame(df)
  df2$t <- eval(substitute({{ time }}), df2)
  df2$s <- eval(substitute({{ status }}), df2)
  df2$i <- eval(substitute({{ id }}), df2)
  df2$neigh <- eval(substitute({{ neighbors }}), df2)

  df2 <- df2 %>%
    mutate(num_neighbors = unlist(lapply(1:length(.data$i), 
                                         function(j) {neighbors_j<-unlist(strsplit(as.character(.data$neigh[j]),","));
                                         length(neighbors_j)}))
           )
  # uses status at start of time period
  if(end==FALSE){
    df2 <- df2 %>%
      mutate(num_neighbors_event = unlist(lapply(1:length(.data$i),
                                           function(j) {neighbors_j<-unlist(strsplit(as.character(.data$neigh[j]),","));
                                           sum(unlist(lapply(neighbors_j,function(x) .data$s[.data$t==df2[j,]$t-1 & .data$i==x])))}))
      )
    df2 <- df2 %>%
      mutate(pct_n_event=((.data$num_neighbors_event/.data$num_neighbors)*100)) %>%
      mutate(pct_n_event=replace(.data$pct_n_event, is.na(.data$pct_n_event), 0))
    df2 <- df2 %>% dplyr::select(-.data$t,-.data$s,-.data$i,-.data$neigh)
    if(keep==FALSE){
      df2 <- df2 %>% dplyr::select(-.data$num_neighbors,-.data$num_neighbors_event)
    }
  }
  # uses status at end of time period
  if(end==TRUE){
    df2 <- df2 %>%
      mutate(num_neighbors_event = unlist(lapply(1:length(.data$i), 
                                           function(j) {neighbors_j<-unlist(strsplit(as.character(.data$neigh[j]),","));
                                           sum(unlist(lapply(neighbors_j,function(x) .data$s[.data$t==df2[j,]$t & .data$i==x])))}))
      )
    df2 <- df2 %>%
      mutate(pct_n_event=((.data$num_neighbors_event/.data$num_neighbors)*100)) %>%
      mutate(pct_n_event=replace(.data$pct_n_event, is.na(.data$pct_n_event), 0))
    df2 <- df2 %>% dplyr::select(-.data$t,-.data$s,-.data$i,-.data$neigh)
    if(keep==FALSE){
      df2 <- df2 %>% dplyr::select(-.data$num_neighbors,-.data$num_neighbors_event)
    }
  }
  #output is an updated data frame
  dfname <- deparse(substitute(df))
  pos <- 1
  envir = as.environment(pos)
  #assign("trellis.par.theme", trellis.par.get(), envir = envir)
  assign(dfname, data.frame(df2), envir = envir)
}
