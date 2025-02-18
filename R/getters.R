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
  if (is.null(times)) return(NULL)
  # filter experiment log

  # filter position
  pos <- obj$data$position$data
  pos <- pos[pos$timestamp >= times$Running & pos$timestamp <= times$Finished, ]
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
  dat <- filter(
    dat,
    Sender == "Trial",
    Type == "StateChange",
    Index %in% index
  )
  if (nrow(dat) == 0) return(NULL)
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
get_finished_trials_indices <- function(obj) {
  dat <- obj$data$experiment_log$data
  dat <- filter(
    dat,
    Sender == "Trial",
    Type == "StateChange",
    Event == "Finished"
  )
  return(dat$Index)
}


#' Returns the position data from the cyberframe object for a single trial
#' @param obj Cyberframe object
#' @param iTrial Index of the trial
#' @return navr::object with position data
#' 
#' @export
get_trial_position <- function(obj, iTrial) {
  times <- get_trial_times(obj, iTrial)
  navr_obj <- navr::filter_times(obj$data$position, c(times$Running, times$Finished))
  return(navr_obj)
}

#' Returns the position data from the cyberframe object
#' @param obj Cyberframe object
#' @return navr::object with position data
#' 
#' @export
get_position <- function(obj) {
  return(obj$data$position)
}

#' Returns the position data from the cyberframe object at a specific time
#' @param obj Cyberframe object
#' @param time Time to get the position data for
#' @param limit "exact", "after" or "before". If "after", the position data is returned
#' if the timestamp is greater than or equal to the given time. If "before",
#' the position data is returned if the timestamp is less than or equal to the
#' given time.
#' @return data frame with position data or NULL
#' 
#' @export
get_position_at_time <- function(obj, time, limit = "exact") {
  pos <- obj$data$position$data
  if (!(limit %in% c("exact", "after", "before"))) {
    stop("Limit must be one of: 'exact', 'after', 'before'")
  }
  row_idx <- switch(limit,
    "exact" = which(pos$timestamp == time),
    "after" = which(pos$timestamp >= time)[1],
    "before" = tail(which(pos$timestamp <= time), 1)
  )
  
  if (length(row_idx) == 0) return(NULL)
  return(pos[row_idx, ])
}

#' Returns the experiment log from the cyberframe object
#' @param obj Cyberframe object
#' @return list with experiment log data
#' 
#' @export
get_experiment_log <- function(obj) {
  return(obj$data$experiment_log)
}
