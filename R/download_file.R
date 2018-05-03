#' @title Download file if it does not exist in the local path 
#' 
#' @description This function downloads a list of files they do not exist 
#' in the local path 
#' 
#' @param url a character string naming the URL of a resource to be downloaded.
#' 
#' @param destfile a character string with the name where the downloaded file is saved. 
#' Tilde-expansion is performed.
#' 
#' @param mdoe a character. The mode with which to write the file. 
#' Useful values are "w", "wb" (binary), "a" (append) and "ab". 
#' Not used for methods "wget" and "curl". (See also ‘Details’.).
#' Default is "wb".
#' 
#' @param ... other arguments to pass to download.file, 
#' for details see ?utils::download.file
#' 
#' @return The function returns an integer code 0 for success 
#' and non-zero for failure
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
download_file <- function(url, destfile, mode = "wb", ...){
  if(file.exists(destfile))
    return(0)
  download.file(url = url, destfile = destfile, mode = mode, ...)[[1]]
}