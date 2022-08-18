#' @title process metadata for a given data file
#'
#' @description Generates the following metadata, including:
#' 1) Period of record
#' 2) Number of days with missing data for each parameter
#' 3) Number of days with data flagged as suspect for each parameter
#' 4) Number of days with data flagged as fail for each parameter
#'
#' @details The input is output file of the QC operation in ContDataQC().  That
#' is, a file with Date.Time, and parameters (matching formats in config.R).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param fun.myFile Filename (no directory) of data file.  Must be CSV file.
#' @param fun.myDir.import Directory for import data.
#' Default is current working directory.
#' @param fun.myDir.export Directory for export data.
#' Default is current working directory.
#' @param fun.myParam.Name Column name in myFile to perform summary statistics.
#'  One or two parameters can be specified.
#' @param fun.myDate.Name Column name in myFile for date.
#' Default = "Date.Time".
#' @param fun.myDate.Format Format of DateTime field.
#' Default = \%Y-\%m-\%d.
#' @param fun.myThreshold Value to draw line on plot.  For example, a regulatory
#'  limit.  Default = NA
#' @param fun.myConfig Configuration file to use for this data analysis.
#' The default is always loaded first so only "new" values need to be included.
#' This is the easiest way to control date and time formats.
#' @param df.input a date frame as input if not providing a csv file
#' @return Returns a data frame
#' @examples
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Save example files from Package to use for example
#' ## This step not needed for users working on their own files
#' df.x <- DATA_period_test2_Aw_20130101_20141231
#' write.csv(df.x,"DATA_period_test2_Aw_20130101_20141231.csv")
#' myFile <- "config.ExcludeFailsFalse.R"
#' file.copy(file.path(path.package("ContDataQC"), "extdata", myFile)
#'           , file.path(getwd(), myFile))
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # Load File to use for PeriodStats
#' myDir <- tempdir()
#' myFile <- "DATA_period_test2_AW_20130101_20141231.csv"
#' df.x <- read.csv(file.path(myDir, myFile))
#'
#' # function inputs
#' myFile <- "DATA_period_test2_Aw_20130101_20141231.csv"
#' myDir.import <- tempdir()
#' myParam.Name <- "Water.Temp.C"
#' myDateTime.Name <- "Date.Time"
#' myDateTime.Format <- "%Y-%m-%d %H:%M:%S"
#' myThreshold <- 20
#' myConfig <- ""
#' # Custom Config
#' myConfig.Fail.Include  <- "config.ExcludeFailsFalse.R"
#'
#' # Run Function
#' ## Example 1. default report format (html)
#' SumStats.updated(myFile
#'           , myDir.import
#'           , myParam.Name
#'           , myDateTime.Name
#'           , myDateTime.Format
#'           , myThreshold
#'           , myConfig
#'           , df.input)
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
ReportMetaData <- function(fun.myFile
                        , fun.myDir.import = getwd()
                        , fun.myParam.Name
                        , fun.myDateTime.Name = "Date.Time"
                        , fun.myDateTime.Format = NA
                        , fun.myThreshold = NA
                        , fun.myConfig = ""
                        , df.input = NULL
)
{##FUN.fun.Stats.START
  # 00. Debugging Variables####
  boo_DEBUG <- FALSE
  if(boo_DEBUG==TRUE) {##IF.boo_DEBUG.START
    fun.myFile <- "DATA_test2_Aw_20130101_20141231.csv"
    fun.myDir.import <- file.path(".","data-raw")
    fun.myParam.Name <- c("Water.Temp.C", "Sensor.Depth.ft")
    fun.myDateTime.Name <- "Date.Time"
    fun.myDateTime.Format <- NA
    fun.myThreshold <- 20
    fun.myConfig <- ""
    df.input <- NULL
    # Load environment
    #ContData.env <- new.env(parent = emptyenv()) # in config.R
    source(file.path(getwd(),"R","config.R"), local=TRUE)
    # might have to load manually
  }##IF.boo_DEBUG.END
  
  # 0a.0. Load environment
  # config file load, 20170517
  if (fun.myConfig!="") {##IF.fun.myConfig.START
    config.load(fun.myConfig)
  }##IF.fun.myConfig.START
  
  # change the default settings in Environment if needed
   # ContData.env$myName.Flag        <- "Flag" # flag prefix
   # ContData.env$myStats.Fails.Exclude <- TRUE  #FALSE #TRUE
   # ContData.env$myFlagVal.Fail    <- "F"
  # 0b.2. Format, DateTime
  if (is.na(fun.myDateTime.Format)) {##IF.fun.myConfig.START
    fun.myDateTime.Format <- ContData.env$myFormat.DateTime
  }
  # 2.0. Load File
  # load data (data.frame or from CSV)
  # if no data frame then import file.
  if (!is.null(df.input)) {##IF.START
    df.load <- df.input
  } else {
    # 2.1. Error Checking, make sure file exists
    if(fun.myFile %in% list.files(path=fun.myDir.import)==FALSE) {##IF.file.START
      #
      myMsg <- paste0("Provided file ("
                      ,fun.myFile
                      ,") does not exist in the provided import directory ("
                      ,fun.myDir.import
                      ,").")
      stop(myMsg)
      #
    }##IF.file.END
    
    df.load <- utils::read.csv(file.path(fun.myDir.import, fun.myFile)
                               ,as.is=TRUE,na.strings=c("","NA"))
  }##IF.END
  
  # 2.3. Error Checking, data field names
  param.len <- length(fun.myParam.Name)
  myNames2Match <- c(fun.myParam.Name, fun.myDateTime.Name)
  #myNames2Match %in% names(df.load)
  if(sum(myNames2Match %in% names(df.load))!= (param.len + 1)){##IF.match.START
    # find non match
    Names.NonMatch <- myNames2Match[is.na(match(myNames2Match, names(df.load)))]
    myMsg <- paste0("Provided data file ("
                    ,fun.myFile
                    ,") does not contain the column name ("
                    ,Names.NonMatch,").")
    stop(myMsg)
  }##IF.match.END
  # 2.4.  Error Checking, DateTime format
  #df.load[,fun.myDateTime.Name] <- as.Date()
  
  # 2.5.  Number of Parameters
  # Check for 1 vs. 2 parameters
  param.len <- length(fun.myParam.Name)
  
  # Loop, Stats ####
  if(boo_DEBUG==TRUE) {##IF.boo_DEBUG.START
    i <- fun.myParam.Name[1]
  }##IF.boo_DEBUG.END
  # 20181114, added for 2nd parameter
  df.list <- list()
  for (i in fun.myParam.Name){##FOR.i.START
    #
    i.num <- match(i, fun.myParam.Name)
    print(paste0("WORKING on parameter (", i.num,"/",param.len,"); ", i))
    utils::flush.console()
    myCol <- c(fun.myDateTime.Name, i)
    
    # check NA for each parameter
    # Subset Fields
    df.param <- df.load[,myCol]
    # 3.2. Add "Date" field
    fd01 <- ContData.env$myFormat.Date
    myDate.Name <- "Date"
    df.param[,myDate.Name] <- as.Date(df.param[,fun.myDateTime.Name], fd01)
    # 3.3. Data column to numeric
    df.param[,i] <- suppressWarnings(as.numeric(df.param[,i]))
    df.toCheck <- df.param[,c(myDate.Name,i)]
    df.summaryNA <- df.toCheck %>% group_by(Date) %>% summarise(sumNA=sum(is.na(!!rlang::sym(i))))
    # check if flag field is in data
    # if yes, summarise the fail and suspect data points
    # Default values from config.R
    # ContData.env$myFlagVal.Fail    <- "F"
    # ContData.env$myFlagVal.Suspect <- "S"
    # ContData.env$myName.Flag       <- "Flag" # flag prefix
    
    ## If flag parameter names is different from config then it won't be found
    myParam.Name.Flag <- paste(ContData.env$myName.Flag, i, sep=".")
    # Modify columns to keep based on presence of "flag" field
    if (myParam.Name.Flag %in% names(df.load)) {##IF.flagINnames.START
      # Flag field present in data
      myCol <- c(fun.myDateTime.Name, myParam.Name.Flag)
      df.param.flag <- df.load[,myCol]
      df.param.flag[,myDate.Name] <- as.Date(df.param.flag[,fun.myDateTime.Name], fd01)
      df.toCheck.flag <- df.param.flag[,c(myDate.Name,myParam.Name.Flag)]
      # summarise the fail and suspect data points 
      df.summaryFlag <- df.toCheck.flag %>% group_by(Date) %>% summarise(sumFail=sum(!!rlang::sym(myParam.Name.Flag)==ContData.env$myFlagVal.Fail),
                                                                         sumSuspect=sum(!!rlang::sym(myParam.Name.Flag)==ContData.env$myFlagVal.Suspect),
                                                                         sumNoFlagData=sum(!!rlang::sym(myParam.Name.Flag)==ContData.env$myFlagVal.NoData))
    } else {
      # No Flag column
      df.summaryFlag <- data.frame()
      myMsg <- "No QC Flag field was found so no summary on flags"
      cat(paste0(myMsg, "\n"))
    }##IF.flagINnames.END
    if (length(df.summaryFlag)>0){
      df.summary <- merge(df.summaryNA,df.summaryFlag,by="Date")
    }else{
      df.summary <- df.summaryNA
    }
    new.list <- list(df=df.summary)
    names(new.list) <- i
    df.list <- c(df.list,new.list)
  }##FOR.i.END
  return(df.list)
  
}##FUNCTION.END
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~