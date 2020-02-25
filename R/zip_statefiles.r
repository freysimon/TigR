#' Create zipfiles from COSERO statevarfiles for use in Delft-FEWS
#' @author Simon Frey
#' @description Create zipfiles from a set of statevarfiles to be used in Delft-FEWS.
#' @details The zipfiles are named according to the folder they are located in combined with a prefix and a postfix.
#' The standard filename then will be "COSERO_Alz_Update Default.zip", for instance.
#' @param wd Character string. Working directory
#' @param selection chracter string. Either "all" or a vector of directories within wd that will be processed.
#' @param prefix character string. Prefix for the name of the zipfile
#' @param postfix chracter string. Postfix for the name of the zipfile
#' @export
#' @return Zipfiles are written to the harddisk.

zip_statefiles <- function(wd, selection = "all", prefix = "COSERO_", postfix = "_Update Default"){
  

  if(selection == "all"){
    selection <- list.dirs(path=wd)
    selection <- selection[2:length(selection)]
  } else {
    selection <- paste(addSlash(wd),selection,sep="")
  }
  
  for(k in 1:length(selection)){
    setwd(selection[k])
    f <- dir(pattern="dmp$")
    zip(zipfile = paste(TigR::addSlash(wd),prefix,basename(selection[k]),postfix,".zip",sep=""),
        files = f)
  }
  
}