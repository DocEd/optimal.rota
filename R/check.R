#' Check Fellow
#'
#' @param rota a rota list returned from `fit_rota()`
#' @param fellow a string of the fellow to check
#'
#' @return
#' @export
check_fellow <- function(rota, fellow = "dermot") {
  
  # Check for me
  1:nrow(rota[[3]]@solution) %>%
    purrr::walk(
      function(x) {
        n_bad <- rota[[1]]$preferences %>%
          mutate(assignment = rota[[1]]$fellows[rota[[1]]$days[rota[[3]]@solution[x,]]]) %>%
          dplyr::filter(.data[[fellow]] == "Literally Cannot Work" & assignment == fellow) %>%
          nrow()
        
        if (n_bad > 0) {
          print(paste0(x, ": is bad"))
        } else {
          print(paste0(x, ": is good"))
        }
      }
    )

}

