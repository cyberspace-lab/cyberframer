#' Plot the path of a trial
#' @param obj Cyberframe object
#' @param iTrial Index of the trial
#' @param ... Additional arguments passed to navr::plot_path
#' @export
plot_trial_path <- function(obj, iTrial, ...) {
  UseMethod("plot_trial_path")
}

#' @describeIn plot_trial_path Plot the path of a trial
#' @export
plot_trial_path.cyberframe <- function(obj, iTrial, ...) {
  navr_obj <- get_trial_position(obj, iTrial)
  if (is.null(navr_obj)) {
    warning("No position data found for trial ", iTrial)
    return(NULL)
  }
  navr::plot_path(navr_obj, ...)
}
