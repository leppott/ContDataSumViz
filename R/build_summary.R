#' @title Build Summary Report
#'
#' @description Build a site summary report based on user input files.
#'
#' @details This function will create a site summary report based on files
#' provided by the user.
#'
#' There will be a main metadata file (Excel) that references the other files.
#' The files will be used in the file order they are listed.
#'
#' * Prefix
#'
#'     + File prefix
#'
#' * Type
#'
#'     + Figure, Table, Text, or Header
#'
#' * Caption
#'
#'     + Caption of line item
#'
#' Can also include "text" or "header" lines.
#'
#' "text" will be inserted as plain text.
#'
#' "header" with a number (e.g., Header1 or Header2 or Header3) will be use the
#' Caption to generate header text in the output.
#'
#' "figures" can be one of any image type (e.g., png, jpg, or pdf).  All images
#' should be in the same format.
#'
#' "tables" are only allowed as CSV files at this time.
#'
#' Output is a self contained HTML file (default).  Future plans for PDF and
#' Word (docx) formats.
#'
#' Special characters (e.g, #, %, (, etc) in table headers will be translated
#' by R into ".".  Nor can table column headers start with a number.  R will
#' translate them to an "X".
#'
#' Special characters in text do not always get translated properly.
#'
#' @param dir_data Directory name with files.
#' @param file_main Main metadata file.  Default = '_metadata.xlsx'
#' @param sheet_main Sheet name in file_main with metadata. Default = 'metadata'
#' @param file_prefix_sep File names separator for prefix used in 'Order' column to
#' sort the files for inclusion in the summary report.  Default = '_'
#' @param rmd_template Path and file name of RMD used to generate the summary
#' report.  Default = NULL (internal package)
#' @param output_format File type for output (html only).  Default = 'html'
#' @param output_file Output file path.  Default = working directory
#'
#' @return Returns summary report in the specified format (HTML, PDF, or Word).
#'
#' @examples
#' # Copy Example Files to TempDir
#' file.copy(system.file("extdata/report", package="ContDataSumViz")
#'           , tempdir()
#'           , recursive = TRUE)
#'
#' # Inputs
#' dir_data        <- file.path(tempdir(), "report")
#' file_main       <- "_Captions_SiteX.xlsx"
#' sheet_main      <- "metadata"
#' file_prefix_sep <- "_"
#' rmd_template    <- system.file("rmd/SiteSummary.Rmd"
#'                                , package="ContDataSumViz")
#' output_format   <- "html"
#' output_file     <- file.path(tempdir()
#'                              , paste0("SiteSummary.", output_format))
#'
#' build_summary(dir_data = dir_data
#'               , file_main = file_main
#'               , sheet_main = sheet_main
#'               , file_prefix_sep = file_prefix_sep
#'               , rmd_template = rmd_template
#'               , output_format = output_format
#'               , output_file = output_file)
#'
#' # Open tempdir (Windows only)
#' # shell.exec(tempdir())
#' @export
build_summary <- function(dir_data = NULL
                          , file_main = "_metadata.xlsx"
                          , sheet_main = "metadata"
                          , file_prefix_sep = "_"
                          , rmd_template = NULL
                          , output_format = "html"
                          , output_file = NULL) {

  # DEBUG----
  boo_debug <- FALSE
  debug_version <- c("local", "pkg")[2]
  if (boo_debug == TRUE) {
    if (debug_version == "local") {
      dir_data        <- file.path("inst", "extdata", "report")
      file_main       <- "_Captions_SiteX.xlsx"
      sheet_main      <- "metadata"
      file_prefix_sep <- "_"
      rmd_template    <- file.path("inst", "rmd", "SiteSummary.Rmd")
      output_format   <- "html"
      output_file = NULL
    } else if (debug_version == "pkg") {
      dir_data        <- system.file("extdata/report"
                                     , package = "ContDataSumViz")
      file_main       <- "_Captions_SiteX.xlsx"
      sheet_main      <- "metadata"
      file_prefix_sep <- "_"
      rmd_template    <- system.file("rmd/SiteSummary.Rmd"
                                     , package = "ContDataSumViz")
      output_format   <- "html"
      output_file     <- file.path(tempdir()
                                   , paste0("SiteSummary.", output_format))
    }## IF ~ debug_version
  }## IF ~ boo_debug == TRUE

  # QC----
  ## QC, dir_data----
  if (is.null(dir_data) | dir.exists(dir_data) == FALSE) {
    msg <- "Data directory is null or doesn't exist."
    stop(msg)
  }## IF ~ dir_data

  ## QC, rmd, null----
  if (is.null(rmd_template)) {
    path_rmd <- system.file("extdata/rmd/SiteSummary.Rmd"
                            , package = "ContDataSumViz")
  }## IF ~ rmd_template, null

  ## QC, rmd, exists----
  if (file.exists(rmd_template) == FALSE) {
    msg <- "RMD file does not exist."
    stop(msg)
  }## IF ~ rmd_template, exists

  ## QC, output_file----
  if (is.null(output_file)) {
    output_file <- file.path(paste0("SiteSummary.", output_format))
  }## IF ~ output_file, null

  # Munge ----
  output_format <- tolower(output_format)
  if (output_format == "html") {
    output_format <- paste0(output_format, "_document")
  } else if (output_format == "pdf") {
    output_format <- paste0(output_format, "_document")
  } else if (output_format == "docx") {
    output_format <- "word_document"
  } else if (is.null(output_format)) {
    # outputs all formats based on yaml header
  } else {
    msg <- "Allowable formats are 'html', 'pdf', or 'docx'."
    stop(msg)
  }## IF ~ output_format

  # Read Meta ----
  df_meta <- as.data.frame(readxl::read_excel(file.path(dir_data, file_main)
                                  , sheet = sheet_main))
  df_meta[, "Type"] <- tolower(df_meta[, "Type"])

  dir_data_full <- normalizePath(dir_data)

  # Add file names to df_meta
  df_files <- data.frame("Files" = list.files(dir_data))
  # https://stackoverflow.com/questions/4350440/
  #            split-data-frame-string-column-into-multiple-columns
  df_files[, "Prefix"] <- sapply(strsplit(as.character(df_files$Files), "_")
                                 , "[", 1)
  # merge (need counter to sort otherwise order is messed up by merge)
  df_meta[, "rowID"] <- 1:nrow(df_meta)
  df_merge <- merge(df_meta
                    , df_files
                    , by.x = "Prefix"
                    , by.y = "Prefix"
                    , all.x = TRUE
                    , sort = FALSE)
  df_merge <- df_merge[order(df_merge[, "rowID"]), ]

  # Render RMD ----
  rmarkdown::render(input = rmd_template
                    , output_format = output_format
                    , output_file = output_file
                    , quiet = TRUE)

}## FUNCTION ~ metval.excel
