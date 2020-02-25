#' Run the hydrological model COSERO
#' @description Update the paramters of the hydrological model COSERO via a shell command
#' @author Simon Frey
#' @param path Path to the executable of COSERO.
#' @param saveold logical. Save the old parameter file?
#' @param batchfile character string. Use a predefined batchfile for updating COSEROs parameters or use the standard setting (see details). In the latter case, set batchfile = NA.
#' @param parafile chracter string. Path to the existing parameter file. If NULL \code{"./input/Parameter_COSERO.par"} is assumed.
#' @param new.parameters character string. Path to the file containing the new parameter matrix.
#' @param show.output logical. Show the console output of COSERO in the console of R?
#' @param ... Further arguments passed to \code{\link{shell}} 
#' @details 
#' update.parameters calls a batchfile via the command \code{\link{shell}}. In this batchfile, COSERO is fed with the necessary user input.
#' The user can provide a batchfile (e.g. with specific commands to COSERO) or let update.parameters write the batchfile itself.
#' In the first case, batchfile must not be NA but a path to the user defined batchfile. In the latter case, both the batchfile and the necessary commandsfile are deleted again after the excecution. 
#' 
#' path must be set to a full path to the executable of COSERO including the exe itself. If the exe is not specified, "COSERO.exe" is assumed.
#' @references 
#'    Frey, S., and Holzmann, H.: A conceptual, distributed snow redistribution model. Hydrol. Earth Syst. Sci., 19, 4517–4530, doi:10.5194/hess-19-4517-2015 2015
#'    
#'    Herrnegger, M., Nachtnebel, H.-P., and Haiden, T.: Evapotranspiration in high alpine catchments – an important part of the water balance!, Hydrol. Res., 43, 460–475, doi:10.2166/nh.2012.132, 2012.
#'    
#'    Kling, H., Stanzel, P., Fuchs, M., and Nachtnebel, H.-P.: Performance of the COSERO precipitation-runoff model under nonstationary conditions in basins with different climates, Hydrolog. Sci. J., 60, 1374–1393, doi:10.1080/02626667.2014.959956, 2015
#' @examples
#' update.parameters(path = "C:/COSERO/COSERO.exe", saveold = TRUE, which.copy = "all", show.output = TRUE)
#' @return Returns nothing to R but executes the model COSERO
#' @seealso For reading the output of COSERO see \code{\link{readCosero}} and \code{\link{read.qobsqsim}}
#' @export

update.parameters <- function(path, saveold=TRUE, batchfile=NA, parafile = NULL, new.parameters,
                      show.output = FALSE, ...){
  
  if(Sys.info()['sysname'] != "Windows"){
    stop("Due to the fact, that COSERO is available for Windows only, this function is available on windows only, too.")
  }
  pathparts <- strsplit(path, "/", fixed = TRUE)[[1]]
  exe <- pathparts[length(pathparts)]
  if(TigR::substrRight(exe,3) != "exe"){
    exe <- "COSERO.exe"
    inpath = paste(dirname(path),"/input/",sep="")
    path <- TigR::addSlash(paste(pathparts, collapse = "/"))
  } else {
    inpath = paste(path,"/input/",sep="")
    path <- TigR::addSlash(paste(pathparts[1:(length(pathparts)-1)], collapse = "/"))
  }
  
  if(!file.exists(paste(path,exe,sep=""))){
    stop("The executable of COSERO must be specified. I tried COSERO.exe without luck.")
  }
  
  
  if(is.na(batchfile)){
    del <- TRUE
    # write batchfile
    writeLines(c(paste("cd /D ",TigR::changeSlash(path),sep=""),
                 paste("COSERO.exe < commands_load_para.txt",sep="")),
               con=paste(path,"/COSERO_load_para.bat",sep="")
    )
    # write commands_singlerun.txt
    # continue with default data and parameter file
    # Confirm diversions and redirections from links.txt
    # Simulate all basins
    # Confirm start and end date
    # Perform a singe run
    # load statevariables from statevar.dmp according to use.statevar
    # start the model run
    if(is.null(parafile)){
      parafile <- "Parameter_COSERO.par"
    }
    file.existing <- file.exists(paste(inpath,"/",pathparafile,sep=""))
    writeLines(c("4","2",parafile,"1",new.parameters,"3",parafile,"5"),
               con = paste(path,"commands_load_para.txt",sep = ""))
    batchfile <- paste(path,"COSERO_load_para.bat",sep="")
  } else {
    del <- FALSE
  }
  
  if(!file.exists(batchfile)) stop(paste("ERROR: Cannot find batchfile in: ",batchfile,sep=""))
  
  if(saveold){
    if(!dir.exists(paste(path,"output.old",sep=""))){
      dir.create(paste(path,"output.old",sep=""))
    }
    if(which.copy[1] == "all"){
      which.copy <- dir(path = paste(path,"output",sep=""), recursive = TRUE)
    }
    file.copy(from=paste(path,"output/",which.copy,sep=""), 
              to=paste(path,"output.old/",which.copy,sep=""),overwrite=TRUE)
    
    # In addition, copy parameterfile to output.old
    parfile <- dir(path = paste(path,"input",sep = ""), pattern = ".par")
    file.copy(from=paste(path,"input/",parfile,sep=""), 
              to=paste(path,"output.old/",parfile,sep=""),overwrite=TRUE)
  }
  
  if(show.output){
    shell(cmd = batchfile, intern = FALSE, ...)
  } else {
    invisible(shell(cmd = batchfile, intern = TRUE, ...))
  }
  
  if(del){
    invisible(file.remove(c(batchfile,paste(path,"commands_singlerun.txt",sep=""))))
  }
  
}

