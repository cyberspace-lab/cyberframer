#' @noRd
prepare_navr_log <- function(df_position){
  ## Converting position
  df_position <- prepare_navr_column_names(df_position)
  df_position <- positions_to_xyz(df_position)
  #renames player position and rotation to position and rotation
  colnames(df_position) <- gsub("player_", "", colnames(df_position))
  return(df_position)
}

positions_to_xyz <- function(df_position){
  ## convert
  ## flip y and z
  position_columns <- colnames(df_position)[grepl("position", colnames(df_position))]
  for(position_col in position_columns){
    df_position <- vector3_to_columns(df_position, position_col)
  }
  rotation_columns <- colnames(df_position)[grepl("rotation",colnames(df_position))]
  for(rotation_col in rotation_columns){
    df_position <- vector3_to_columns(df_position, rotation_col, flip_yz = FALSE)
  }
  return(df_position)
}

is_column_present <- function(table, name){
  return(name %in% names(table))
}

json_to_list <- function(text){
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("needs jsonlite to continue")
  }
  if (is.null(text)) return(NULL)
  if (length(text) <= 1) return(NULL)
  if (!is_json(text)) return(NULL)
  ls <- jsonlite::fromJSON(text)
  return(ls)
}

is_json <- function(text){
  bool <- c()
  bool <- c(bool, any(grepl("\\{", text))) #has braces
  return(all(bool))
}

replace_strings <- function(vec, strings, replacements){
  if (length(strings) != length(replacements)){
    cat("Strings and replacements need to have the same length")
    return(NULL)
  }
  for (i in 1:length(strings)){
    vec[vec == strings[i]] <- replacements[i]
  }
  return(vec)
}

#' Tries to rename columns so they correspond to proper naming conventions
#' @description changes "." to "_" to correspond with python conventions,
#' makes everything lowercase, renames Time column if present to "timestamp"
#'
#' @param df data.frame
#'
#' @return modified data.frame
#' @noRd
prepare_navr_column_names <- function(df){
  df <- rename_column(df, "Time", "timestamp")
  new_names <- tolower(gsub("[.]", "_", colnames(df))) #replaces . with _
  colnames(df) <- new_names
  return(df)
}

rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df)==old_column] <- new_column
  return(df)
}

# UNITY -------------
#turns vector columns in string "(x, y, z)" into three columns(position_x, position_y, position_z) and returns the table
vector3_to_columns <- function(df_position, column, flip_yz = TRUE, remove=TRUE){
  # TODO - remove the requirements
  if (!requireNamespace("stringr", quietly = TRUE)) {
    print("Cannot continue withouth stringr package. Please install it")
    return(FALSE)
  }
  ifelse(flip_yz, xyz <- c("x", "z", "y"), xyz <- c("x", "y", "z"))
  #TODO - remove the data.table
  positions <- df_position[[column]]
  pos <- stringr::str_match(positions, "\\((.*),(.*),(.*)\\)")[, 2:4]
  pos <- as.data.frame(pos)
  new_names <- stringr::str_c(column, sep="_", xyz)
  colnames(pos) <- new_names
  pos <- mutate(pos, across(everything(), as.numeric))
  df_position <- add_column(df_position, pos, .after = column)
  if (remove){
    df_position <- select(df_position, -column)
  }
  return(df_position)
}

#pure helpers for my particular unity logging
position_to_vector <- function(positions){
  res <- positions
  all_names <- names(positions)
  for(name in all_names){
    temp <- positions[[name]]
    n_items <- length(positions)
    df <- data.frame(position_x = numeric(n_items),
                     position_y = numeric(n_items),
                     position_z = numeric(n_items))
    for (i in seq_len(length(temp))){
      df[i, ] <- unity_vector_to_numeric(temp[i])
    }
    res[[name]] <- df
  }
  return(res)
}
