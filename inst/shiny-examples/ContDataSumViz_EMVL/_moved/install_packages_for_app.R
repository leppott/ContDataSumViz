
##########################################################################
# install_packages_for_app.R is for installing packages to run the Shiny App
# Only need to be ran the first time when a user loads the app
# latest update
#
#
###################################################################
packages <- c("devtools"        # install helper for non CRAN libraries
         , "installr"       # install helper
         , "readxl"         # to read excel files
         , "writexl"        # to write excel files
         , "data.table"
         , "tibbletime"
         , "digest"         # caused error in R v3.2.3 without it
         , "dataRetrieval"  # loads USGS data into R
         , "knitr"          # create documents in other formats (e.g., PDF or Word)
         , "doBy"           # summary stats
         , "zoo"            # z's ordered observations, use for rolling sd calc
         , "htmltools"      # needed for knitr and doesn't always install properly with Pandoc
         , "rmarkdown"      # needed for knitr and doesn't always install properly with Pandoc
         , "evaluate"       # a dependency that is sometimes missed.
         , "highr"          # a dependency that is sometimes missed.
         , "ggplot2"        # plots
         , "ggthemes"
         , "reshape2"
         , "installr"       # used to install Pandoc
         , "rsconnect"
         , "rLakeAnalyzer"
         , "plotly"
         , "shiny"          # shiny example
         , "shinydashboard"
         , "shinyjs"
         , "shinyBS"
         , "shinyalert"
         , "conflicted"     # deal with package conflicts 
         , "survival"
         , "shinyFiles"     # more Shiny
         , "shinythemes"    # more Shiny
         , "XLConnect"      # read Excel files
         , "zip"            # read/save zip files
         
)

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages==FALSE)){
  install.packages(packages[!installed_packages])
}
devtools::install_github("tsangyp/StreamThermal")
devtools::install_github("jasonelaw/IHA")
devtools::install_github("leppott/ContDataQC")
devtools::install_github("leppott/ContDataSumViz")
tinytex::install_tinytex()
