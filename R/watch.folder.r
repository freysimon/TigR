
#' Check for changes and append changed textfiles
#' @description Check for changed files and append them. Works with single or multiple files
#' @param f character or character vector of files to be checked
#' @param af character or character vector of files to be appended
#' @param sleep.time numeric. time in [s] between checks of files exists
#' @param skipLines numeric. Lines to be skipped for reading the files f
#' @details Checks for the existance of files. If f exists it is appended to af, and the system waits as long as 
#' f is deleted and created again. Designed for the use with Provis stand alone.
#' 
#'     Note that the function needs to be killed using esc. It doesn't exit on it own.
#' @author Simon Frey
#' @return a message is printed if a file is appended. Nothing gets returned. 

watch.files <- function(f, af, sleep.time = 2, skipLines = 0){
  
  library(tools)
  
  skipLines <- skipLines + 1
  
  repeat{
    
    if(all(file.exists(f))){
      for(j in 1:length(f)){
        temp <- readLines(con = f[j])
        temp <- temp[skipLines:length(temp)]
        write.table(temp, file = af[j], append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = " ")
        print(paste("appended file ", f[j], " to ", af[j], sep = ""))
      }
      
      while(all(file.exists(f))){
        Sys.sleep(sleep.time)
      }
    }
  }
}

  
    