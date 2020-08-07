#' Parse Preferences
#'
#' @param file_path a file path to the folder where prefernces are stored
#' @param fellows a character vector containing the names of the fellows
#' @param month an integer vector of length 1 detailing the month of interest
#'
#' @importFrom magrittr `%>%`
#' @importFrom readr read_csv
#' @importFrom rlang abort .data
#' @importFrom dplyr select mutate across if_else
#' @importFrom tidyselect everything contains
#' @importFrom lubridate days_in_month as_date
#' 
#' @return
#' @export
#'
#' @examples
parse_preferences <- function(file_path = NULL,
         fellows = c("matt", "muska", "zainab", "dermot", "ed", "justin"),
         month = 1) {
  
  if (is.null(file_path)) {
    abort("please enter a file path")
  }
  
  prefs <- list.files(file_path)
  selections <- prefs[grepl("Select Preferences", prefs)]
  requirement <- prefs[grepl("Work Requirment", prefs)]
  
  p_raw <- read_csv(file.path(file_path, selections), col_types = "Dcccccc")
  req <- read_csv(file.path(file_path, requirement), col_types = "ciiiiiilllc")

  names(req) <- tolower(names(req))
  
  req <- req %>%
    # Extremely fragile. Needs Fixing
    select(fellows) %>%
    mutate(across(everything(), ~ if_else(is.na(.), 0L, .)))

  # Number of days required to work
  n <- as.integer(req[4,]-req[3,]-req[2,]-req[1,])
  
  # Numerically assign the rota requirement
  curr_month <- days_in_month(month)
  days_ <- rep(1:length(fellows), n)

  empty_shifts <- as.integer(curr_month - length(days_))
  
  if (empty_shifts > 0) {
    
    next_ <- max(days_)+1
    days_ <- c(days_, next_:(next_+empty_shifts-1))
    
  }

  p_raw <- p_raw %>%
    select(contains(fellows)) %>%
    mutate(across(everything(), ~ if_else(is.na(.), "Ambivalent", .)))
  
  names(p_raw) <- tolower(names(p_raw))
  
  return(list(preferences = p_raw,
              days = days_,
              requests = req,
              fellows = fellows))

}

#' Title
#'
#' There is a potential target solution, but it doesn't strictly adhere to the
#' constraint functions. We can do this with GAs by defining a penalized fitness
#' function. So can we now use this method to optimise the rota. Let's start
#' with the simple case of 2 doctors, each to fill 2 days, with alternate day
#' preferences. I.e. the solution is already in their preferences.
#'
#' @param x
#' 
#'   
#' @return
#' @export
#'
#' @examples
encode_preferences <- function(x) {
  
  y <- x$preferences
  
  pref_strong <- y %>%
    apply(1, function(x) which(x == "Strongly Request"))
  
  pref_weak <- y %>%
    apply(1, function(x) which(x == "Request"))
  
  meh <- y %>%
    apply(1, function(x) which(x == "Ambivalent"))
  
  pref_no <- y %>%
    apply(1, function(x) which(x == "Don't Want"))
  
  pref_bigno <- y %>%
    apply(1, function(x) which(x == "Literally Cannot Work"))

  return(list(
    pref_strong = pref_strong,
    pref_weak = pref_weak,
    meh = meh,
    pref_no = pref_no,
    pref_bigno = pref_bigno
  ))
  
}

#' Solve Rota
#'
#' @param parsed_files 
#' @param preference_encodings 
#'
#' @importFrom GA ga
#'
#' @return
#' @export
#'
#' @examples
solve <- function(parsed_files, preference_encodings) {
  
  days_ <- parsed_files$days
  
  ga(
    type = "permutation",
    fitness = fitness_func,
    days = days_,
    pref_strong = preference_encodings$pref_strong,
    pref_weak = preference_encodings$pref_weak,
    pref_no = preference_encodings$pref_no,
    pref_bigno = preference_encodings$pref_bigno,
    popSize = 350,
    parallel = 4,
    lower = 1, upper = length(days_),
    maxiter = 2000, run = 500)

}

inspect_consec <- function(x, df) {
  apply(x@solution, 1, function(x) c_consec(df$days[x]))
}

#' Export Rota
#'
#' @param x 
#' 
#' @importFrom magrittr `%>%`
#' @importFrom readr write_csv
#' @importFrom dplyr mutate
#' 
#' @return
#' @export
#'
#' @examples
export <- function(file_path, x, solution, assign = 1) {
  
  x$preferences %>%
    mutate(assignment = x$fellows[x$days[solution@solution[1,]]]) %>%
    write_csv(file.path(file_path, "allocation.csv"), na = "")
  
}