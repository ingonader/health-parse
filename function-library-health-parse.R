## ############################################################################
## function library for parsing script of health.txt
## ------------------------------------------------------------------------
## Created 25 Sep 2018
## Author Ingo Nader
## ############################################################################

#' Get paths needed for script
#'
#' @return List with elements
#'   \code{base} (path of the current project),
#'   \code{r} (path containing r script files),
#'   \code{dat} (path containing the data files),
#' @examples
#'   path <- get_paths()
#' @export
#'
get_paths <- function() {
  path <- list()
  ## get dropbox path relative to current user:
  path$base <- file.path(
    Sys.getenv("HOME"),   ## home dir of current user
    "Dropbox/lists"       ## Dropbox directory
  )
  ## define script and data paths relative to raw path:
  ## (currently not used)
  path$r <- file.path(path$base, "")
  path$dat <- file.path(path$base, "")
  path$out <- file.path(path$base, "")
  return(path)
}