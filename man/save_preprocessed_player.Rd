% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{save_preprocessed_player}
\alias{save_preprocessed_player}
\title{Saves preprocessed player ot hte given folder. Receives either specific name,
or gets the name from already present player logs. If there are multiple
player logs in the folder,takes the name from the first available}
\usage{
save_preprocessed_player(
  directory,
  exp_timestamp = NULL,
  log,
  orig_filename = NULL,
  precision = 4
)
}
\arguments{
\item{directory}{Where shoudl i save the file. Should end with a slash!}

\item{orig_filename}{what is the name of the original file}

\item{player_log}{table to be saved}
}
\value{

}
\description{
Saves preprocessed player ot hte given folder. Receives either specific name,
or gets the name from already present player logs. If there are multiple
player logs in the folder,takes the name from the first available
}
