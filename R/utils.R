#' Copy All Bindings from an Environment to Another
#'
#' @param from source; <environment>
#' @param to target; <environment>
#'
#' @return invisible NULL
copy_all_bindings <- function(from, to){
  from_names <- ls(from,
                   all.names = TRUE)
  mapply(
    function(x, value){
      assign(x, value, envir = to)
    },
    x = from_names,
    value = mget(from_names,
                 from))
  invisible(NULL)
}
