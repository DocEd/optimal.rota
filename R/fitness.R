#' Fitness Function
#'
#' @param x 
#' @param days 
#' @param pref_strong 
#' @param pref_weak 
#' @param pref_no 
#' @param pref_bigno 
#'
#' @return
#' @export
#'
#' @examples
fitness_func <- function(x, days, pref_strong, pref_weak, pref_no, pref_bigno) {
  
  d <- days[x]
  
  # no consecutive days
  n_consec <- c_consec(d)
  if (n_consec == 0) {
    ans <- 30
  } else {
    ans <- - n_consec^3 - 100
  }
  
  # minimise strong anti-pref
  n_pref_bigno <- c_pref(d, pref_bigno)
  if (n_pref_bigno == 0) {
    ans <- ans + 10
  } else {
    ans <- ans - n_pref_bigno^2 - 10
  }
  
  # minimise anti-pref
  n_pref_no <- c_pref(d, pref_no)
  if (n_pref_no == 0) {
    ans <- ans + 5
  } else {
    ans <- ans - n_pref_no
  }
  
  # maximise pref
  n_pref_weak <- c_pref(d, pref_weak)
  ans <- ans + n_pref_weak
  
  # maximise strong-pref
  n_pref_strong <- c_pref(d, pref_strong)
  ans <- ans + n_pref_strong^2

  return(ans)
}

#' Constraints: Preferences
#'
#' @param days 
#' @param preferences 
#' 
#' @importFrom purrr map2_lgl
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
#'
#' @examples
c_pref <- function(days, preferences) {
  # number of assignments in preferences
  map2_lgl(days, preferences, ~ .x %in% .y) %>%
    sum()
}

#' Constraints: Consecutive Days
#'
#' @param assignment 
#'
#' @return
#' @export
#'
#' @examples
c_consec <- function(assignment) {
  sum(assignment[2:length(assignment)] == assignment[1:(length(assignment)-1)])
}
    
    
