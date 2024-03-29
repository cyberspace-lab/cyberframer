% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loading.R
\name{open_player_log}
\alias{open_player_log}
\title{Searches a directory for a player log. Returns player log data.table}
\usage{
open_player_log(
  directory,
  exp_timestamp = NULL,
  override = FALSE,
  save = TRUE,
  remove = FALSE
)
}
\arguments{
\item{directory}{where the log should be located}

\item{exp_timestamp}{provides timestamp of a log to load}

\item{override}{if true, deletes processed player log and loads the unprocessed.
if FALSE, load preprocessed log if present}

\item{save}{Should the log be saved after being preprocessed}

\item{remove}{should the existing prepricessed log be removed}
}
\value{
data.table with the loaded player log or NULL.
}
\description{
Searches a directory for a player log. Returns player log data.table
}
