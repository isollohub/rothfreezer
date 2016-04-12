#-- src_rothfreezer -----------------------------------------------------------
#' Connect to the rothfreezer database
#' 
#' Connect to the rothfreezer SQLite database. 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom easydb db_config db_build
#' @export
#' 
#' @examples 
#' # For a description of each table
#' vignette('database-overview', package = 'rothfreezer')
#' 
#' # Connect to the database
#' db <- src_rothfreezer()
#' db
#' 

src_rothfreezer <- function() {
  # Read configuration file
  cnf <- system.file('db/_rothfreezer.yaml', package = 'rothfreezer') %>% db_config()
  
  # Build the database if it does not exist
  if (!file.exists(cnf$db)) suppressWarnings(db_build(cnf))
  
  # Connect to the database
  src_sqlite(cnf$db) %>% add_class('src_rothfreezer')
}


#-- src_rothscreen methods ----------------------------------------------------
#' @export
src_desc.src_rothfreezer <- function(x) {
  paste0(
    'sqlite ', x$info$serverVersion, 
    ' [rothfreezer - ', packageVersion('rothfreezer'), ']'
  )
}


#-- OO Utilities --------------------------------------------------------------
# Add an S3 subclass in pipeline
# @param x an object
# @param ... Character; S3 subclasses to add to object
add_class <- function(x, ...) { class(x) <- c(..., class(x)); return(x) }
