filter_experiment <- function(){

}

#' Title
#'
#' @param obj
#' @param index
#'
#' @return
#' @export
#'
#' @examples
filter_trial <- function(obj, index){
  # getstart and end time
  times <- get_trial_times(obj, index)
  if(is.null(times)) return(NULL)
  # filter experiment log

  # filter position
  pos <- obj$data$position$data
  pos <- pos[pos$timestamp >= times$Running & pos$timestamp <= times$Finished,]
  obj$data$position$data <- pos
  return(obj)
}

#' Title
#'
#' @param obj
#' @param index
#'
#' @return
#' @export
#'
#' @examples
get_trial_times <- function(obj, index){
  dat <- obj$data$experiment_log$data
  dat <- filter(dat, Sender=="Trial", Type=="StateChange", Index == index)
  if(nrow(dat) == 0) return(NULL)
  out <- setNames(as.list(dat$Time), dat$Event)
  return(out)
}

#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
get_finished_trials_indices <- function(obj){
  dat <- obj$data$experiment_log$data
  dat <- filter(dat, Sender=="Trial", Type=="StateChange", Event=="Finished")
  return(dat$Index)
}
