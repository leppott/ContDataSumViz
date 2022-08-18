#' @title run Shiny Example
#'
#' @description Launches Shiny app for ContDataSumViz package.
#'
#' @details The Shiny app based on the R package ContDataSumViz is included in
#' the R package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/ContDataSumViz
#'
#' @param shinyappname Shiny appplication name, default = ContDataSumViz_EMVL
#'
#' @examples
#' \dontrun{
#' # Run Function (full EMVL version, default)
#' runShiny()
#'
#' # Shiny, EMVL slim server version
#' runShiny("ContDataSumViz_EMVL_slim")
#'
#' # Shiny, test
#' runShiny("ContDataSumViz_test")
#'
#' }
#
#' @export
runShiny <- function(shinyappname = "ContDataSumViz_EMVL"){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples"
                        , shinyappname
                        , package = "ContDataSumViz")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ContDataSumViz`."
         , call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END
