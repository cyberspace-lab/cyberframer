#' Title
#'
#' @param obj
#' @param index
#'
#' @return
#' @export
#'
#' @examples
summarise_trials <- function(obj, index = NA){
  if(is.na(index)) index <- get_finished_trials_indices(obj)
  out <- data.frame()
  for(i in index){
    obj_trial <- filter_trial(obj, i)
    res <- summarise_position(obj_trial$data$position)
    res <- append(res, list(index = i))
    out <- rbind(out, as.data.frame(res))
  }
  return(out)
}

summarise_experiment <- function(obj, index=1){
  # filter experiment
  obj <- obj
}

summarise_position <- function(position_data){
  dat <- position_data$data
  out <- list(
    duration = diff(range(dat$timestamp)),
    distance = tail(dat$distance_total,1))
  return(out)
}
