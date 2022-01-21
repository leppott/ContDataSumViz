#' @title run Shiny Example
#'
#' @description Launches Shiny app for ContDataSumViz package.
#'
#' @details The Shiny app based on the R package ContDataSumViz is included in the
#' R package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/ContDataSumViz
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShiny()
#' }
#
#' @export
runShiny <- function(){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples"
                        , "ContDataSumViz"
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
