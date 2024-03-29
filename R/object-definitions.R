#' Title
#'
#' @return
#' @export
#'
#' @examples
CyberframeData <- function() {
  obj <- list()
  obj$participant_id <- ""
  obj$experiment_name <- ""
  obj$session <- NA
  obj$timestamp <- NA
  obj$map_limits <- NULL
  obj$data <- list()
  obj$data$experiment_info <- NA
  obj$data$position <- navr::NavrObject()
  obj$data$experiment_log <- NA
  obj$data$results_log <- NA
  class(obj) <- append(class(obj), "cyberframe")
  return(obj)
}
