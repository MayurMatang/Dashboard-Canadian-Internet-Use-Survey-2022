library(memisc)
library(dplyr)
library(srvyr)

# import and create the file with variable names and labels using memisc package
# Download link: https://www150.statcan.gc.ca/n1/pub/56m0003x/2020001/2022.zip
# Only the 5 files below are needed, rest can be deleted
cius <- 
  spss.fixed.file("4432_CIUS_ECUI_2022/Data/CIUS_PUMF.txt",
  columns.file = "4432_CIUS_ECUI_2022/Data/Layouts/SPSS/CIUS_PUMF_i.sps",
  varlab.file  = "4432_CIUS_ECUI_2022/Data/Layouts/SPSS/CIUS_PUMF_vare.sps",
  codes.file   = "4432_CIUS_ECUI_2022/Data/Layouts/SPSS/CIUS_PUMF_vale.sps",
  missval.file = "4432_CIUS_ECUI_2022/Data/Layouts/SPSS/CIUS_PUMF_miss.sps"
  )

data_set <- as.data.set(cius)
description(data_set$AC_010A)       # lists the variable with its description
codebook(data_set$AC_010A)  # shows codes, labels, and missing values for one variable

# Set dbl vars so that the next step (as.data.frame) converts the rest to fct
num_vars <- c("PUMFID",
              "EC_G011A", "EC_G011B", "EC_G011C", "EC_G011D",
              "EC_G011E", "EC_G011F", "EC_G011X",
              "EC_G012A", "EC_G013A", "EC_G014A",
              "SUM_GTOT", "WTPG",
              paste0("WRPG", 1:1000))


measurement(data_set) <- list(interval = num_vars)
df <- as.data.frame(data_set)
glimpse(df[1:341])
glimpse(df[342:1342])

# Save data frame with labels (optional)
# saveRDS(df, "df.rds", compress = "xz")

# Survey design
design <- df %>%
  as_survey_rep(
    weights          = WTPG,
    repweights       = WRPG1:WRPG1000,
    type             = "bootstrap",
    combined.weights = TRUE,
    mse              = TRUE
  )

# Save the survey design object
saveRDS(design, "design.rds", compress = "xz")
