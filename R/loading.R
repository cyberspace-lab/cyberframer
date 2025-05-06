#' Goes through the folder and loads every experiment info into separate object
#'
#' @param override if TRUE, deletes and recomputes preprocessed player.
#' default is FALSE
#' @param folder where to look for cyberframe files
#' @param save if true, then preprocessed logs are saved to the folder
#'
#' @return list of objects
#' @export
load_experiments <- function(folder, override = FALSE, save = TRUE) {
  if (is.null(folder)) stop("no folder set")
  # open experiment_logs to see how many do we have
  session_infos <- open_cyberframe_logs(folder, "SessionInfo",
                                        func = load_session_info,
                                        flatten = FALSE)
  if (is.null(session_infos)) stop("Session info not found")
  res <- list()
  for (i in seq_len(length(session_infos))) {
    info <- session_infos[[i]]
    res[[i]] <- load_experiment(
      folder, exp_timestamp = info$session_header$Timestamp,
      override = override, save = save
    )
  }
  return(res)
}

#' Loads files form a folder into CyberframeData
#'
#' @param folder path to the folder respective to the working directory
#' @param exp_timestamp timestamp of a particular experiment to search for
#' @param override if a preprocessed file is found, should it be overridden?
#' IF TRUE, then all files are preprocessed again. Default is FALSE
#' @param save logical should the files be saved after being preprocessed?
#' default is TRUE
#'
#' @returns CyberframeData object
#' @example
#' @export
load_experiment <- function(folder, exp_timestamp = NULL,
                            override = FALSE, save = TRUE) {
  if (is.null(folder)) stop("No folder set")
  # TODO - this should return only a single one per timestamp
  session_info <- open_cyberframe_log(folder, log_name = "SessionInfo",
                                      exp_timestamp = exp_timestamp,
                                      func = load_session_info)
  if (is.null(session_info)) {
    stop("Session info not found")
  }
  # if multiple logs or no logs, quit
  if (is.null(exp_timestamp)) {
    exp_timestamp <- session_info$session_header$Timestamp
  }
  ## TODO separate preprocess and opening
  navr_object <- open_player_log(folder, exp_timestamp = exp_timestamp,
                                 override = override, save = save)
  if (is.null(navr_object)) {
    stop("Player log not found")
  }
  # preprocesses player log
  # checks if there is everything we need and if not, recomputes the stuff
  test_log <- open_experiment_logs(folder, exp_timestamp, flatten = TRUE)
  result_log <- open_cyberframe_log(folder, "results", exp_timestamp, optional = TRUE)
  obj <- CyberframeData()
  obj$participant_id <- session_info$session_header$Participant
  obj$timestamp <- exp_timestamp
  obj$data$session_info <- session_info
  obj$data$position <- navr_object
  # TODO - this might be an issue due to uneven terminology
  obj$data$experiment_log <- test_log
  obj$data$results_log <- result_log
  obj$experiment_name <- obj$data$experiment_log$name
  return(obj)
}

#' Loads particular info file into a list
#'
#' @param filepath path to the file
#'
#' @return list object
#' @export
load_session_info <- function(filepath) {
  res <- load_headers(filepath)
  return(res)
}

#' Iterates over all __experiment_ files in a folder an saves
#' them one by one to a return list
#'
#' @param directory directory where the file is located
#' @param flatten in case of only a single list is returned, unnests the list.
#' Beware, unnested list causes issues with opening experiments
#' @param exp_timestamp time of the
#'
#' @return
open_experiment_logs <- function(dir, exp_timestamp = NULL, flatten = FALSE) {
  out <- open_cyberframe_logs(dir, log_name = "experiment",
                              exp_timestamp = exp_timestamp,
                              func = load_experiment_log, flatten = flatten)
  return(out)
}

#' Loads expeirment log into a predefined list
#'
#' @param filepath path tot he expeirment log
#' @return list with loaded settings files and data
load_experiment_log <- function(filepath) {
  res <- load_cyberframe_log(filepath)
  res$name <- experiment_name_from_filename(filepath)
  return(res)
}


#' Generic loading of all the results, experiment and other logs
#'
#' @param directory directory where to look for the log
#' @param log_name name of the log to be searched for and loaded
#' @param exp_timestamp timestamp of the particular type of log
#' @param flatten in case only a single file is found, should it be unnested? defaults to false
#' @param func R functions which actually loads the object (contains code to preprocess the
#' log, extract some log specific information etc.). If null, default function is used
#'
#' @return
#' @export
#' @examples
open_cyberframe_logs <- function(directory, log_name, exp_timestamp = NULL,
                              func = NULL, flatten = FALSE) {
  logs <- find_cyberframe_logs(directory, log_name, exp_timestamp)
  if (is.null(logs)) return(NULL)
  out <- list()
  for (i in seq_len(length(logs))) {
    out[[i]] <- load_cyberframe_log(logs[i], func = func)
  }
  if (flatten && (length(out) == 1)) out <- out[[1]]
  return(out)
}

#' Searches for and loads a generic cyberframe framework log.
#' Contains framework specific header, and optionally data.frame data. See
#' \code{\link{load_cyberframe_log}} for specifics
#'
#' @param directory Where to search for the log
#' @param log_name name of the log (e.g.)
#' @param exp_timestamp necessary if multiple logs are in the same folder
#' @param func function used to load the log. Optional. IF NULL, default loading
#' function is used
#'
#' @return
#' @export
#'
#' @examples
open_cyberframe_log <- function(directory, log_name, exp_timestamp = NULL,
                                func = NULL, optional = FALSE) {
  pths <- find_cyberframe_logs(directory, log_name, exp_timestamp)
  if (is.null(pths)) {
    if (!optional) warning("Cannot open log ", log_name, " in ", directory)
    return(NULL)
  }
  if (length(pths) > 1) {
    warning("Cannot open log ", log_name, " in ", directory,
            ". Multiple logs of the same name. You need to specify the timestamp")
    return(NULL)
  }
  res <- load_cyberframe_log(pths[1], func = func)
  return(res)
}

#' Loads a generic cyberframe framework log. These logs have specific header and
#' data notations. The default logs should be saved in UTF-8 encoding with semicolon 
#' as a separator, dot for decimal points and unquoted strings. 
#' The function loads the header and the data frame.  
#'
#' @param filepath path to the log
#' @param func optional loading function, it loads the log instead of the
#' default
#' @param ... additional parameters passed to read.table. skip, sep, header,
#' stringsAsFactos and encoding cannot be changed
#'
#' @return list with parsed data and optionally $data field with log's dataframe
#' @export
#'
#' @examples
load_cyberframe_log <- function(filepath, func = NULL, ...) {
  if (!is.null(func)) {
    result <- func(filepath)
    return(result)
  }
  result <- load_headers(filepath)
  i_bottom <- get_bottom_header_index(filepath)
  df_data <- try(read.table(filepath,
                            skip = i_bottom, sep = ";", header = TRUE,
                            stringsAsFactors = FALSE, encoding = "UTF-8",
                            dec = ".", quote = "",
                            ...
  ), silent = TRUE)
  if (class(df_data) == "data.frame"){
    # removes empty last columns in many cyberframe framework logs
    n_scanning <- ifelse(nrow(df_data) < 50, nrow(df_data), 50)
    if(grepl("X", colnames(df_data)[ncol(df_data)]) &
       all(is.na(df_data[1:n_scanning, ncol(df_data)]))){
      df_data[, ncol(df_data)] <- NULL
    }
    result$data <- df_data
  }
  return(result)
}

find_cyberframe_logs <- function(directory, log_name, exp_timestamp = NULL,
                                 warning_many = TRUE) {
  ptr <- create_log_search_pattern(log_name, exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = TRUE)
  if (length(logs) < 1) return(NULL)
  if (length(logs) > 1 && !is.null(exp_timestamp)) {
    if(warning_many) {
      warning("Multiple logs of the same name in ", directory,
              ". You need to specify the timestamp")
    }
  }
  return(logs)
}

#' Searches a directory for a player log. Returns player log data.table
#'
#' @param directory where the log should be located
#' @param exp_timestamp provides timestamp of a log to load
#' @param override if true, deletes processed player log and loads the
#' unprocessed if FALSE, load preprocessed log if present
#' @param remove should the existing prepricessed log be removed
#' @param save Should the log be saved after being preprocessed
#'
#' @return data.table with the loaded player log or NULL.
#' @export
#' @import data.table
open_player_log <- function(directory, exp_timestamp = NULL, override = FALSE,
                            save = TRUE, remove = FALSE) {
  ls_log_path <- find_player_path(directory, exp_timestamp)
  if (nchar(ls_log_path$path) == 0) return(NULL)
  if (nchar(ls_log_path$path_preprocessed) > 0) {
    if (override) {
      if (remove) {
        message("Removing preprocessed log ", ls_log_path$path_preprocessed)
        file.remove(ls_log_path$path_preprocessed)
      }
    } else {
      message("Loading preprocessed player log ", ls_log_path$path_preprocessed)
      # TODO - remove data.table
      navr_object <- navr::NavrObject()
      navr_object$data <- read.table(ls_log_path$path_preprocessed,
        header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE,
        encoding = "UTF-8")
      return(navr_object)
    }
  }
  message("Loading unprocessed player log ", ls_log_path$path)
  # TODO - chagne so it doesn't read text so friggin much :(
  text <- readLines(ls_log_path$path, warn = FALSE, encoding = "UTF-8")
  i_bottom <- get_header_end_index(text)
  df_position <- read.table(ls_log_path$path, header = TRUE, sep = ";",
                            dec = ".", skip = i_bottom,
                            stringsAsFactors = FALSE)
  df_position <- prepare_navr_log(df_position)
  navr_object <- navr::load_position_data(navr::NavrObject(), df_position)
  navr_object <- navr::prepare_navr(navr_object)
  if (override && !remove) return(navr_object)
  if (save) save_preprocessed_player(directory, exp_timestamp, navr_object$data)
  return(navr_object)
}

find_player_path <- function(directory, exp_timestamp = NULL) {
  ls <- list(path = "", path_preprocessed = "")
  ptr <- create_log_search_pattern("player", exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = TRUE)
  if (length(logs) == 0) warning("Could not find the file for player log in ", directory)
  if (length(logs) > 2) warning("Multiple player logs in ", directory)
  if (length(logs) == 1) ls$path <- logs[1]
  if (length(logs) == 2) {
    # check if there is a preprocessed player file
    preprocessed_index <- grep("*_preprocessed", logs)
    if (length(preprocessed_index) == 1) {
      ls$path_preprocessed <- logs[preprocessed_index]
      ls$path <- logs[-preprocessed_index]
    } else {
      warning("There is more player logs with appropriate timestamp in the
            same folder. Have you named and stored everything appropriately?")
    }
  }
  return(ls)
}
