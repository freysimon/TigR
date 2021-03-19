#' Purge direcory
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
  file.remove(dir(path=x))
  unlink(dir(path=x), recursive = TRUE)
}