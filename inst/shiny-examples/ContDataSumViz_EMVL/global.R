# Global
#
# Add package from UI and Server
#
# Erik.Leppo@tetratech.com
# 2022-08-17
#~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----

# ## UI----
library(shiny)
library(shinyjs) # fails without it
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(DT)
library(plotly)
#library(shinycustomloader)
library(shinycssloaders)
#
# ## Server----
library("readxl")        # to read excel files
library("writexl")
library("data.table")
#library("DT")
library("tidyverse") # fails without dplyr, stringr
library("tibbletime")
#library("shiny")
#library("shinydashboard")
#library("shinyjs")
library("shinyBS")
#library("shinythemes")
#library("shinyalert")
library("conflicted") # fails without it
library("dataRetrieval")
library("doBy")
library("knitr")
library("htmltools")
library("rmarkdown")
library("highr")
library("survival")
library("shinyFiles")
#library("plotly")
library("zip")
library("reshape2")
library("ContDataQC")
library("ContDataSumViz")
library("StreamThermal")
library("IHA")
library("XLConnect")
# fails without dplyr and stringr from tidyverse

# Functions ----
source("_moved/import_raw_data.R")
source("update_ContDataQC/config.R")
source("update_ContDataQC/CompSiteCDF.updated.R")
source("update_ContDataQC/SumStats.updated.R")
source("update_ContDataQC/ReportMetaData.R")

# Other ----
options(shiny.maxRequestSize = 100*1024^2)
