filter_experiment <- function() {

}

#' Returns cyberframe object with only the data from a single trial
#'
#' @param obj Cyberframe object
#' @param index Index of the trial. Zero based
#'
#' @return Cyberframe object with only the data from a single trial
#' @export
#'
#' @examples
filter_trial <- function(obj, index) {
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

#' returns a list of starting and ending times of a trial
#'
#' @param obj Cyberframe object
#' @param index Index of the trial. Works with trial numbers as
#' they appear in the framework, so typically zero based
#'
#' @return list of lists. Each list contains a single trial,
#' and parameters from the cyberframe (Running, Finished, Started etc.)
#' @export
#'
#' @examples
get_trial_times <- function(obj, index) {
  dat <- obj$data$experiment_log$data
  dat <- filter(dat,
    Sender == "Trial",
    Type == "StateChange",
    Index %in% index)
  if(nrow(dat) == 0) return(NULL)
  out <- setNames(as.list(dat$Time), dat$Event)
  return(out)
}

#' Returns indices of trials which were finished completely
#'
#' @param obj Cyberframe object
#'
#' @return vector of indices of finished trials. Typically zero based
#' @export
#'
#' @examples
get_finished_trials_indices <- function(obj){
  dat <- obj$data$experiment_log$data
  dat <- filter(dat,
    Sender == "Trial",
    Type == "StateChange",
    Event == "Finished")
  return(dat$Index)
}
