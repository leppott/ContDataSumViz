#' @title Build Missing Data Report
#'
#' @description Build a missing data summary report based on ContDataQC user
#' input files.
#'
#' @details This function will create a missing data summary report based on
#' files provided by the user.
#'
#' The files to use are the ones already processed by the main QC function in
#' ContDataQC.  File format is CSV.
#'
#' Output is a self contained HTML file (default).
#'
#' @param dir_data Directory name with files.
#' @param file_data Data file (CSV).
#' @param file_path_config ContDataQC config file (path and file name).
#' Default = NULL (use config.ORIG.R from extdata).
#' @param file_path_rmd Path and file name of RMD used to generate the missing
#' data summary report.  Default = NULL (internal package)
#' @param output_format File type for output (html only).  Default = 'html'
#' @param output_dir Output file path.  Default = working directory
#'
#' @return Returns summary report in the specified format (HTML, PDF, or Word).
#'
#' @examples
#' # Copy Example files to Temp Directory
#' ## Data
#' fn_data <- "DATA_test2_Aw_20130101_20141231.csv"
#' file.copy(file.path(path.package("ContDataSumViz"), "extdata", fn_data)
#'           , file.path(tempdir(), fn_data))
#' ## Config
#' fn_config <- "config.ORIG.R"
#' fp_config <- file.path(tempdir(), fn_config)
#' file.copy(file.path(path.package("ContDataSumViz"), "extdata", fn_config)
#'           , fp_config)
#'
#' # Render report
#' build_missingdata(dir_data = tempdir()
#'                  , file_data = fn_data
#'                  , file_path_config = fp_config
#'                  , file_path_rmd = NULL
#'                  , output_format = "html"
#'                  , output_dir = tempdir())
#'
#' # Open Temp Directory (Windows only)
#' # shell.exec(tempdir())
#'
#' @export
build_missingdata <- function(dir_data = tempdir()
                          , file_data = NULL
                          , file_path_config = NULL
                          , file_path_rmd = NULL
                          , output_format = "html"
                          , output_dir = tempdir()) {

  # DEBUG----
  boo_debug <- FALSE
  debug_version <- c("local", "pkg")[2]
  if(boo_debug == TRUE) {
    str_file <- "DATA_test2_Aw_20130101_20141231.csv"
    str_rmd <- "MissingData.Rmd"
    if(debug_version == "local") {
      dir_data         <- tempdir()
      file_data        <- str_file
      file_path_config <- NULL
      file_path_rmd    <- file.path("inst", "rmd", str_rmd)
      output_format    <- "html"
      output_dir       <- tempdir()
    } else if(debug_version == "pkg") {
      dir_data         <- tempdir()
      file_data        <- str_file
      file_path_config <- NULL
      file_path_rmd    <- file.path(path.package("ContDataSumViz")
                                    , "rmd"
                                    , str_rmd)
      output_format    <- "html"
      output_dir       <- file.path(tempdir()
                                   , paste0("SiteSummary.", output_format))
    }## IF ~ debug_version
  }## IF ~ boo_debug == TRUE

  # QC----
  ## QC, dir_data----
  if(is.null(dir_data) | dir.exists(dir_data) == FALSE) {
    msg <- "Data directory is null or doesn't exist."
    stop(msg)
  }## IF ~ dir_data

  ## QC, file_data----
  if(is.null(file_data) | file.exists(file.path(dir_data, file_data)) == FALSE) {
    msg <- "Data filename is null or doesn't exist."
    stop(msg)
  }## IF ~ dir_data

  ## QC, config ----
  if(is.null(file_path_config)) {
    file_path_config <- system.file("extdata/config.ORIG.R"
                                    , package="ContDataSumViz")
  }## IF ~ is.null(file_path_config)
  if(file.exists(file_path_config) == FALSE) {
    msg <- "config file does not exist."
    stop(msg)
  }## IF ~ file_path_rmd, exists
  # Load
  source(file_path_config)

  ## QC, rmd, null----
  if(is.null(file_path_rmd)) {
    file_path_rmd <- system.file("rmd/MissingData.Rmd"
                            , package="ContDataSumViz")
  }## IF ~ file_path_rmd, null

  ## QC, rmd, exists----
  if(file.exists(file_path_rmd) == FALSE) {
    msg <- "RMD file does not exist."
    stop(msg)
  }## IF ~ file_path_rmd, exists

  ## QC, output_dir ----
  if(dir.exists(output_dir) == FALSE) {
    msg <- "output directory does not exist."
    stop(msg)
  }## IF ~ output_dir, exists

  ## QC, output_file----
  file_datetime <- format(Sys.time(), '%Y%m%d_%H%M%S')
  output_file <- file.path(paste0("MissingData_"
                                  , file_datetime
                                  , "_."
                                  , output_format))


  # Munge ----
  output_format <- tolower(output_format)
  if(output_format == "html") {
    output_format <- paste0(output_format, "_document")
  } else if(output_format == "pdf") {
    output_format <- paste0(output_format, "_document")
  } else if(output_format == "docx") {
    output_format <- "word_document"
  } else if(is.null(output_format)) {
    # outputs all formats based on yaml header
  } else {
    msg <- "Allowable formats are 'html', 'pdf', or 'docx'."
    stop(msg)
  }## IF ~ output_format

  # QC, print
  if(boo_debug == TRUE){
    msg <- paste0("rmd = ", file_path_rmd)
    message(msg)
  }## IF ~ boo_debug

  # define pipe, otherwise code RMD fails
  `%>%` <- dplyr::`%>%`

  # Render RMD ----
  rmarkdown::render(input = file_path_rmd
                    , output_format = output_format
                    , output_file = output_file
                    , output_dir = output_dir
                    , quiet = TRUE)

}## FUNCTION ~ metval.excel
