
#' Check seasonal and x13binary packages installation
#'
#' @description checks that the x13binary and seasonal packages are installed in the C drive location
#'     on the users computer. Restrictions with Bank software mean that executable files must be 
#'     stored in the C drive. Packages can be installed if missing.  
#'
#' @param install logical, whether the packages should be installed if they are missing
#'
#' @export
#' @importFrom utils install.packages
#' 
#' @examples
#' \dontrun{
#'     seasonal_x13_package_install_check(FALSE)
#' }
seasonal_x13_package_install_check <- function(install = FALSE){
  
  r_major <- R.Version()$major
  r_minor <- R.Version()$minor
  
  futile.logger::flog.debug(paste0("You are running R version ", r_major, ".", r_minor))
  
  r_path <- file.path("C:", 
                      "Program Files", 
                      "R", 
                      paste0("R-", r_major, ".", r_minor),
                      "library")
  
  if(!file.exists(file.path(r_path, "x13binary", "DESCRIPTION"))){
      futile.logger::flog.info("x13binary package not installed")
      if(install == TRUE){
        futile.logger::flog.info("Installing x13binary...")
        utils::install.packages("x13binary", lib = file.path(r_path))
        futile.logger::flog.info("Installed.")
      }
  }
  if(!file.exists(file.path(r_path, "seasonal", "DESCRIPTION"))){
    futile.logger::flog.info("seasonal package not installed")
    if(install == TRUE){
      futile.logger::flog.info("Installing seasonal...")
      utils::install.packages("seasonal", lib = file.path(r_path))
      futile.logger::flog.info("Installed.")
    }
  }
  
  futile.logger::flog.debug("Both packages installed in correct location.")
  
  
}






