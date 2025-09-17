#' Purge directory
#' @author Simon Frey
#' @export
#' @param x character string. Path of the dir to purge.
#' @description Purge a directory. All files and subdirectories will be deleted.
#' @examples 
#'     #### Not run ####
#'     purge.dir(x="C:/TEMP")
#' @seealso \link{unlink}
#' @seealso \link{file.remove}
#' 
purge.dir <- function(x){
  xx <- dir(path=x, full.names = TRUE)
  file.remove(xx)
  unlink(xx, recursive = TRUE)
}