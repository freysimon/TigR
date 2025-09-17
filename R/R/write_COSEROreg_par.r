#' Write a COSEROReg Parameterfile
#' @author Simon Frey
#' @description write a COSEROReg Parameterfile
#' @param x output from \link{read_COSERO_par}
#' @param file character string. Filename of the output file. If NULL the output will be returned to R.
#' @details Intended use of this function is the translation of old parameter files of COSERO into new ones used by COSEROreg
#' @examples 
#'     ## Do not run
#'     
#'     # Return matrix to R
#'     mat <- write_CoSEROreg_par(x=output_from_read_COSERO_par, file = NULL)
#'     
#'     # Write file on disk
#'     write_CoSEROreg_par(x=output_from_read_COSERO_par, file = "Path_to_file")
#'     
#' @export
#' @seealso \link{read_COSERO_par} for reading old COSERO parameter files
#' @seealso \link{write_COSERO_par} for writing old COSERO parameter files
#' 
write_COSEROreg_par <- function(x, file = "Parameter_COSERO.par"){
  options("encoding" = "UTF-8")
  
  Strpl <- function(x){
    strsplit(x, "_", fixed = TRUE)[[1]][1]
  }
  
  # get dimenstions of the model
  dims  <- TigR::get.dimensions(x, full=FALSE)
  
  
  namesx <- sapply(names(x[[2]]),Strpl)
  
  monthly_par_names <- c(
    # Monthly parameters
    "PCOR", "TCOR","TMMON", "DAYSDRY", "DAYSWET", "ETSYSCOR"
  )
  
  NC_NM_par_names <- c(
    # Parameters using dimensions of land use and months
    "INTMAX", "ETVEGCOR"
  )
  
  
  pars <- c(
    # Runoff parameters and dimensions
    "NB", "IZ", "NZ", "WATERBODY", "DFZON", "ELEV", "NC", "ETSLPCOR", "CTMAX", "CTMIN", "NVAR",
    "RAINTRT", "SNOWCOR", "SNOWTRT", "THRT", "M", "FK", "PWP", "KBF", "BETA", "FKFAK", "H1", "H2",
    "TAB1", "TAB2", "TVS1", "TVS2", "TAB4", "TAB3", "TAB5", 
    
    # Monthly parameters
    "PCor1", "PCor2", "PCor3", "PCor4", "PCor5", "PCor6", "PCor7", "PCor8", "PCor9", "PCor10", "PCor11", "PCor12", 
    "TCor1", "TCor2", "TCor3", "TCor4", "TCor5", "TCor6", "TCor7", "TCor8", "TCor9", "TCor10", "TCor11", "TCor12", 
    "TMMon1", "TMMon2", "TMMon3", "TMMon4", "TMMon5", "TMMon6", "TMMon7", "TMMon8", "TMMon9", "TMMon10", "TMMon11", "TMMon12", 
    "INTMAX1", "INTMAX2", "INTMAX3", "INTMAX4", "INTMAX5", "INTMAX6", "INTMAX7", "INTMAX8", "INTMAX9", "INTMAX10", "INTMAX11", "INTMAX12", 
    "ETVEGCOR1", "ETVEGCOR2", "ETVEGCOR3", "ETVEGCOR4", "ETVEGCOR5", "ETVEGCOR6", 
    "ETVEGCOR7", "ETVEGCOR8", "ETVEGCOR9", "ETVEGCOR10", "ETVEGCOR11", "ETVEGCOR12", 
    "DAYSDRY1", "DAYSDRY2", "DAYSDRY3", "DAYSDRY4", "DAYSDRY5", "DAYSDRY6", 
    "DAYSDRY7", "DAYSDRY8", "DAYSDRY9", "DAYSDRY10", "DAYSDRY11", "DAYSDRY12", 
    "DAYSWET1", "DAYSWET2", "DAYSWET3", "DAYSWET4", "DAYSWET5", "DAYSWET6", 
    "DAYSWET7", "DAYSWET8", "DAYSWET9", "DAYSWET10", "DAYSWET11", "DAYSWET12",
    "ETSYSCOR1", "ETSYSCOR2", "ETSYSCOR3", "ETSYSCOR4", "ETSYSCOR5", "ETSYSCOR6",
    "ETSYSCOR7", "ETSYSCOR8", "ETSYSCOR9", "ETSYSCOR10", "ETSYSCOR11", "ETSYSCOR12",
    
    # Ini parameters
    "KMELTRINI", "KSHINI", "KSWINI", "TSOILINI", "BW0INI", "BW1INI", "BW2INI", "BW3INI", "BW4INI",
    
    # snow related parameters
    "BAREGR", "CTNeg", "CTRed", "DWHCAP", "EVPNS", "EVPSNO", "NSRHOMAX", 
    "PEX2", "PEX3", "SETCON", "SNOWDET", "SOILTYPE", "SRHOMAX", "TSOILMAX", "TSOILMIN", "TVAR", 
    "UADJ", "WHCAP", "SPARE1", "SPARE2", "SPARE3"
  )
  
  paras <- NULL
  for(k in 4:length(pars)){
    wnames <- which(namesx %in% pars[k])
    
    # distributed groundwater storages 
    if(pars[k] == "TAB3"){
      tab3 <- NULL
      for(NB in dims[,1]){
        tab3 <- c(tab3, rep(unlist(x[[3]][wnames[NB]]),dims[NB,2]))
      }
      paras <- cbind(paras, tab3)
      
      
    # monthly distributed parameters  
    } else if(!is.na(adist_dels(x=pars[k],y=monthly_par_names, dels = 2))) {
      
      #stop()
      w <- adist_dels(x=pars[k],y=monthly_par_names, dels = 2)
      parname <- monthly_par_names[w]
      
      wnames <- which(namesx %in% parname)
      
      tmp <- split_monthly(x, parname, wnames)
      paras <- cbind(paras, tmp)
      
      k = k + 12 # jump to next unique parameter 
      
      
    # parameters distributed by landuse and months  
    } else if(!is.na(adist_dels(x=pars[k],y=NC_NM_par_names, dels = 2))){
      
      w <- adist_dels(x=pars[k],y=NC_NM_par_names, dels = 2)
      parname <- NC_NM_par_names[w]
      
      wnames <- which(namesx %in% parname)
      
      tmp <- split_monthly(x, parname, wnames)
      paras <- cbind(paras, tmp)
      
      k = k + 12 # jump to next unique parameter 
      
    # all other parameters  
    } else {
      paras <- cbind(paras,unlist(x[[3]][wnames]))
    }
    
    
    
  
  }
  
  NBIZNZ <- TigR::get.dimensions(x)
  
  paras <- cbind(NBIZNZ, paras)
  
  colnames(paras) <- c("NB","IZ","NZ",pars)
  
  if(is.null(file)){
    return(paras)
  } else {
    write.table(paras, file = file, quote =F, sep = "\t", row.names = F, col.names = T, dec = ".")
  }
  
}

#' Disaggregate monthly parameters from a COSERO parameter file
#' @description Disaggregate parameters with dimensions NBIZNM to monthly columns
#' @details 
#'       In COSERO (old) some parameters use the dimensions NBIZNN (subbasin, cell within the subbasin, month) and are stored in the parameterfile
#'       in one column where all NBIZ of month 1 are placed above the ones of NBIZ of month 2 and so on and so forth. In COSEROreg, however, the
#'       parameterfile uses distinct columns per month. This routine splits the monhtly based parameters and stores them in a matrix where every
#'       column represents a month.
#' @author Simon Frey
#' @export
#' @param x the output from \link{read_COSERO_par}
#' @param parameter character. Name of the parameter that will be split and returned. Note the the parameter name must not contain numbers indicating the month. E.g. PCOR instead of PCOR1
#' @param wnames integer or integer vector. Position(s) of names in vector holding all COSERO Parameter names.
#' @return a matrix containing 12 columns

split_monthly <- function(x, parameter, wnames){
  dims <- TigR::get.dimensions(x, full = FALSE)
  
  # Monthly disaggrgated values
  monval <- matrix(ncol=12,nrow=sum(dims[,2]), data = NA)
  
  iz <- c(1,cumsum(dims[,2]))
  iz_from <- iz[1:(length(iz)-1)]+1
  iz_from[1] <- 1
  iz_to <- iz[2:length(iz)]
  
  for(m in 1:12){
    
  # Values of one month for the whole model
  
    tmp <- NULL
    
    
  
    for(NB in dims[,1]){
      
      tmp <- c(tmp, unlist(x[[3]][wnames[NB]])[iz_from[NB]:iz_to[NB]])
    }
    
    monval[,m] <- tmp
  }
  
  colnames(monval) <- paste(parameter,1:12,sep="")
  
  return(monval)
  
}



#' Disaggregate parameters using the dimensions landuse and months from a COSERO parameter file
#' @description Disaggregate parameters with dimensions NC,NM to monthly columns
#' @details 
#'       In COSERO (old) some parameters use the dimensions NC,NM (landuse, month) and are stored in the parameterfile
#'       in one column where all NBIZ of month 1 are placed above the ones of NBIZ of month 2 and so on and so forth. In COSEROreg, however, the
#'       parameterfile uses distinct columns per month. This routine searches for the corresponding land use (NC) per cell (NZ) and assigns the
#'       monthly values to each column.
#' @author Simon Frey
#' @export
#' @param x the output from \link{read_COSERO_par}
#' @param parameter character. Name of the parameter that will be split and returned. Note the the parameter name must not contain numbers indicating the month. E.g. INTMAX instead of INTMAX1
#' @param wnames integer or integer vector. Position(s) of names in vector holding all COSERO Parameter names.
#' @return a matrix containing 12 columns

split__NZNM_monthly <- function(x, parameter, wnames){
  dims <- TigR::get.dimensions(x, full = FALSE)
  
  # Monthly disaggrgated values
  monval <- matrix(ncol=12,nrow=sum(dims[,2]), data = NA)
  
  # Get NC values
  namesx <- sapply(names(x[[2]]),Strpl)
  NC <- as.numeric(unlist(x[[3]][which(namesx %in% "NC")]))

  
  for(m in 1:12){
    
    # Values of one month for the whole model
    
    tmp <- NULL
    
    
    
    monval[,m] <- tmp
  }
  
  colnames(monval) <- paste(parameter,1:12,sep="")
  
  return(monval)
  
}


#' Approximate Distance allowing only for deletions
#' @author Simon Frey
#' @description Calculate the approximate distance of two strings returning the matches, where deletions where necessary only.
#' @details This function calls \code{adist} to calculate the approximate distance and the operations (insertions, substitutions and deletions) necessary to transform string x in string y. It then evaluates only the cases, where deletions where made only. The user may set a maximum of allowed deletions. If the transformation fails it throws a warning (if warn == TRUE) and return NA.
#' @param x string or vector of strings that the function will try to transform into y
#' @param y string that the function uses to transform x into
#' @param dels NULL or integer. If NULL (the default) set no limitations on how many deletions are allowed. If dels is an integer, allow only that many deletions as dels is set.
#' @param returnstrings logical. Should the strings be returned (TRUE) or the positions within x.
#' @param warn logical. Activate or deactivate warnings.
#' @export
#' @return a string (vector) or a numerical (vector)
#' @examples
#'    y <- c("PCOR", "TCOR", "TMMON", "INTMAX", "ETVEGCOR", "DAYSDRY", "DAYSWET", "ETSYSCOR")
#'    x <- "PCor1"
#'    
#'    adist_dels(x,y, dels = 1, returnstrings = TRUE)
#'    
#'    x <- "PCORXXX"
#'    
#'    adist_dels(x,y, dels = 1)
#'    adist_dels(x,y, dels = NULL)


adist_dels <- function(x, y, dels = NULL, returnstrings = FALSE, warn = FALSE){
  
  
  result <- drop(attr(adist(x,y, ignore.case=TRUE, counts = TRUE), "counts"))
  
  if(is.null(dels)){
    mat <- cbind(result,result[,1] == 0 & result[,3] == 0 & result[,2] != 0)
  } else {
    mat <- cbind(result,result[,1] == 0 & result[,3] == 0 & result[,2] <= dels)
  }
  
  if(length(mat) == 0){
   warning("No match found. Consider a higher number of allowed deletions") 
   return(NA)
    
  } else {
    mat <- which(mat[,4] == 1)
    
    if(length(mat) == 0){
      warning("No match found. Consider a higher number of allowed deletions") 
      return(NA)
    } else {
      if(returnstrings){
      return(x[mat])
    } else {
      return(mat)
    }
      }
    }
}

