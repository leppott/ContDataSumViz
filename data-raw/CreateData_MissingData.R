# Copy Config file from ContDataQC
# Erik.Leppo@tetratech.com
# 2022-07-27
#~~~~~~~~~
# Create file from ContDataQC

# copy file to extdata folder
#~~~~~~~~~~~~~

# Packages
library(ContDataQC)

# Copy
 config_orig <- file.path(path.package("ContDataQC")
                          , "extdata"
                          , "config.ORIG.R")
 file.copy(config_orig
           , file.path("inst", "extdata", "config.ORIG.R"))

