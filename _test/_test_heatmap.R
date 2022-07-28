# EMVL
# heatmap of Fails by day
# EWL
# 20220609
#~~~~~~~~~~~~~~~~~~~~~~~

# Packages
library(ContDataQC)

# Generate Data ----

# 00. Set up
# Parameters
Selection.Operation <- c("GetGageData"
                         , "QCRaw"
                         , "Aggregate"
                         , "SummaryStats")
Selection.Type      <- c("Air","Water","AW","Gage","AWG","AG","WG")
Selection.SUB <- c("Data0_Original"
                   , "Data1_RAW"
                   , "Data2_QC"
                   , "Data3_Aggregated"
                   , "Data4_Stats")
(myDir.BASE <- tempdir()) # create and print temp directory for example data

# Create data directories
myDir.create <- file.path(myDir.BASE, Selection.SUB[1])
ifelse(dir.exists(myDir.create) == FALSE
       , dir.create(myDir.create)
       , "Directory already exists")
myDir.create <- file.path(myDir.BASE, Selection.SUB[2])
ifelse(dir.exists(myDir.create) == FALSE
       , dir.create(myDir.create)
       , "Directory already exists")
myDir.create <- file.path(myDir.BASE, Selection.SUB[3])
ifelse(dir.exists(myDir.create) == FALSE
       , dir.create(myDir.create)
       , "Directory already exists")
myDir.create <- file.path(myDir.BASE, Selection.SUB[4])
ifelse(dir.exists(myDir.create) == FALSE
       , dir.create(myDir.create)
       , "Directory already exists")
myDir.create <- file.path(myDir.BASE, Selection.SUB[5])
ifelse(dir.exists(myDir.create) == FALSE
       , dir.create(myDir.create)
       , "Directory already exists")

# Save example data (assumes myDir.BASE directory exists)
myData <- data_raw_test2_AW_20130426_20130725
write.csv(myData, file.path(myDir.BASE
                            , Selection.SUB[2]
                            , "test2_AW_20130426_20130725.csv"))
myData <- data_raw_test2_AW_20130725_20131015
write.csv(myData, file.path(myDir.BASE
                            , Selection.SUB[2]
                            , "test2_AW_20130725_20131015.csv"))
myData <- data_raw_test2_AW_20140901_20140930
write.csv(myData, file.path(myDir.BASE
                            , Selection.SUB[2]
                            , "test2_AW_20140901_20140930.csv"))
myData <- data_raw_test4_AW_20160418_20160726
write.csv(myData, file.path(myDir.BASE
                            , Selection.SUB[2]
                            , "test4_AW_20160418_20160726.csv"))
myFile <- "config.TZ.Central.R"
file.copy(file.path(path.package("ContDataQC"), "extdata", myFile)
          , file.path(myDir.BASE, Selection.SUB[2], myFile))


# 02.A. QC Raw Data
myData.Operation       <- "QCRaw" #Selection.Operation[2]
myData.SiteID          <- "test2"
myData.Type            <- Selection.Type[3] #"AW"
myData.DateRange.Start <- "2013-01-01"
myData.DateRange.End   <- "2014-12-31"
myDir.import           <- file.path(myDir.BASE, Selection.SUB[2]) #"Data1_RAW"
myDir.export           <- file.path(myDir.BASE, Selection.SUB[3]) #"Data2_QC"
myReport.format        <- "docx"
ContDataQC(myData.Operation
           , myData.SiteID
           , myData.Type
           , myData.DateRange.Start
           , myData.DateRange.End
           , myDir.import
           , myDir.export
           , fun.myReport.format = myReport.format)

# 03. Aggregate Data
myData.Operation       <- "Aggregate" #Selection.Operation[3]
myData.SiteID          <- "test2"
myData.Type            <- Selection.Type[3] #"AW"
myData.DateRange.Start <- "2013-01-01"
myData.DateRange.End   <- "2014-12-31"
myDir.import           <- file.path(myDir.BASE, Selection.SUB[3]) #"Data2_QC"
myDir.export           <- file.path(myDir.BASE, Selection.SUB[4]) #"Data3_Aggregated"
#Leave off myReport.format and get default (docx).
ContDataQC(myData.Operation
           , myData.SiteID
           , myData.Type
           , myData.DateRange.Start
           , myData.DateRange.End
           , myDir.import
           , myDir.export)


#~~~~~~~~~~~~~~~~~~~~~~~~~
shell.exec(tempdir())

# RMD, Set up ----
# Packages
library(dplyr)
# Config
myConfig <- file.path(system.file(package="ContDataQC"), "extdata"
                      , "config.ORIG.R")
source(myConfig)
# inputs
strFile <- "DATA_test2_Aw_20130101_20141231.csv"
data.import <- read.csv(file.path(tempdir(), "Data3_Aggregated", strFile))

# RMD, run ----
strFile.RMD <- file.path("inst", "rmd", "MissingData.RMD")
strFile.out.ext <- paste0(".", "html") #".docx" # ".html"
strFile.RMD.format <- "html_document"
strFile.out <- paste0("Example_Report_MissingData", strFile.out.ext)
fun.myDir.export <- file.path(tempdir())

rmarkdown::render(strFile.RMD
                  , output_format = strFile.RMD.format
                  , output_file = strFile.out
                  , output_dir = fun.myDir.export)
                  #, quiet = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extra code for creating heat map

# # heat map ----
#
# # Data
# df_data <- read.csv(file.path(tempdir(), "Data3_Aggregated", "DATA_test2_Aw_20130101_20141231.csv"))
#
#
# ## _base----
# # Packages
# library(dplyr)
# library(tidyr)
#
# # Munge
# table(df_data$Flag.Water.Temp.C, useNA = "ifany")
#
# df_base <- df_data %>%
#   filter(Flag.Water.Temp.C == "P") %>%
#   group_by(MonthDay, Year) %>%
#   summarise(n = n())
# df_plot <- pivot_wider(df_base, names_from = "Year", values_from = "n")
#
# # add leading zero to MonthDay
# df_base$MonthDay <- sprintf("%04d", df_base$MonthDay)
# # change class
# df_base$MonthDay <- as.character(df_base$MonthDay)
# df_base$Year <- as.character(df_base$Year)
#
#
#
# #myParam <- "Flag.Water.Temp.C"
#
#
# # Plot
#
#
# ## _ggplot----
# # Packages
# library(ggplot2)
#
#
# # determine parameters present
# # check for "Flag" columns
#
# col_flag <- c("Flag.Water.Temp.C")
#
# # summarize by Year, MonthDay for number of "F"
#
# # Plot
# # replace Green and Red with dark green and orange
# #  scale_fill_gradient(low = "#DB4325", high = "#006164") +
# x <- ggplot(df_base, aes(x = Year, y = MonthDay, fill = n)) +
#   geom_tile() +
#   scale_fill_gradient(low = "orange", high = "dark green") +
#   labs(title = "test2, Passing flags, Water.Temp.C") +
#   scale_y_discrete(limits = rev)
#
#
# library(plotly)
# ggplotly(x)




