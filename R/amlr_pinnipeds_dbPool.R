#' Connect to an AMLR pinnipeds database
#'
#' Connect to either the AMLR pinnipeds production or test database
#'
#' @param Database character; see \code{\link[pool]{dbPool}}
#' @param Driver character; see \code{\link[pool]{dbPool}}
#' @param Server character; see \code{\link[pool]{dbPool}}
#' @param idleTimeout integer; default is one hour. See \code{\link[pool]{dbPool}}
#'
#' @details Wrapper...
#'
#' @return Output of \code{\link[base]{try}(\link[pool]{dbPool})} call
#'
#' @examples
#' \dontrun{
#' amlr_pinnipeds_dbPool("AMLR_PINNIPEDS")
#' }
#'
#' @export
amlr_pinnipeds_dbPool <- function(Database, Driver = "SQL Server", Server = "swc-estrella-s", idleTimeout = 3600000) {
  # Based on https://github.com/rstudio/pool
  try(pool::dbPool(
    drv = odbc::odbc(),
    Driver = Driver,
    Server = Server,
    Database = Database,
    Trusted_Connection = "True",
    idleTimeout = 3600000  # 1 hour
  ), silent = TRUE)
}
