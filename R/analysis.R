#' Makes a quick summary of the given trials
#'
#' @param obj Cyberframe object
#' @param index indices of the trials to summarise. In the original
#' format, which is generally zero based.
#'
#' @return Dataframe with summary of the trials
#' @export
#'
#' @examples
summarise_trials <- function(obj, indices = c()) {
  if (length(indices) < 1) indices <- get_finished_trials_indices(obj)
  out <- data.frame()
  for (i in indices){
    obj_trial <- filter_trial(obj, i)
    res <- summarise_position(obj_trial$data$position)
    res <- append(res, list(index = i))
    out <- rbind(out, as.data.frame(res))
  }
  return(out)
}

summarise_experiment <- function(obj, index = 1) {
  # filter experiment
  obj <- obj
}

summarise_position <- function(position_data) {
  dat <- position_data$data
  out <- list(
    duration = diff(range(dat$timestamp)),
    distance = tail(dat$distance_total, 1)) - head(dat$distance_total, 1)
  return(out)
}
