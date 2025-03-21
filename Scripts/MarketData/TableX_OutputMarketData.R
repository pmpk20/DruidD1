#### DRUID D1: Table of all market data  ###############
# Function: To plot market data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 17/08/2023


# *****************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# *****************************************************************************



## sessionInfo()---------------------------------------------------------------
# R version 4.3.1 (2023 - 06 - 16 ucrt)
# Platform:x86_64 - w64 - mingw32 / x64 (64 - bit)
# Running under:Windows 10 x64 (build 19045)
#
# Matrix products:default
#
#
# locale:[1] LC_COLLATE = English_United Kingdom.utf8  LC_CTYPE = English_United Kingdom.utf8
# [3] LC_MONETARY = English_United Kingdom.utf8 LC_NUMERIC = C
# [5] LC_TIME = English_United Kingdom.utf8
#
# time zone:Europe / London
# tzcode source:internal
#
# attached base packages:[1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:[1] RColorBrewer_1.1 - 3 lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      dplyr_1.1.2
# [6] purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       tidyverse_2.0.0
# [11] ggplot2_3.4.2      readxl_1.4.2       here_1.0.1         magrittr_2.0.3
#
# loaded via a namespace (and not attached):[1] utf8_1.2.3        generics_0.1.3    stringi_1.7.12    lattice_0.21 -
#   8    hms_1.1.3
# [6] grid_4.3.1        timechange_0.2.0  cellranger_1.1.0  rprojroot_2.0.3   Matrix_1.5 -
#   4.1
# [11] processx_3.8.1    pkgbuild_1.4.2    ps_1.7.5          mgcv_1.8 - 42       fansi_1.0.4
# [16] scales_1.2.1      cli_3.6.1         rlang_1.1.1       crayon_1.5.2      munsell_0.5.0
# [21] splines_4.3.1     withr_2.5.0       tools_4.3.1       tzdb_0.4.0        colorspace_2.1 -
#   0
# [26] vctrs_0.6.3       R6_2.5.1          lifecycle_1.0.3   pkgconfig_2.0.3   callr_3.7.3
# [31] pillar_1.9.0      gtable_0.3.3      glue_1.6.2        tidyselect_1.2.0  rstudioapi_0.14
# [36] farver_2.1.1      nlme_3.1 - 162      labeling_0.4.2    compiler_4.3.1    prettyunits_1.1.1


## Libraries here: ------------------------------------------------------------
## Setting up libraries in order of use in the script
library(here)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
rm(list = ls())


# ******************************************************************************
#### Section 1: Data Importing ####
# ******************************************************************************


Data <-
  here("MarketData","AllCropData_V1.xlsx") %>% readxl::read_xlsx(sheet = "Sheet1") %>% data.frame()


# ******************************************************************************
#### Section 2: Cleaning ####
# ******************************************************************************


## Trim years column names
Years <- Data[3:51] %>% colnames() %>% gsub(pattern = "X", replacement = "") %>% as.numeric()
colnames(Data) <- c("Crop", "Measure", Years)


## Convert columns to numeric values
Data_Numeric <- Data %>% mutate(across(3:51, as.numeric))


## Rename some variables for ease of reference
Data_Numeric$Measure[Data_Numeric$Measure == "Milling oats real"] <- "Prices"
Data_Numeric$Measure[Data_Numeric$Measure == "Malting barley real"] <- "Prices"
Data_Numeric$Measure[Data_Numeric$Measure == "Milling wheat real"] <- "Prices"

Data_Numeric <-
  Data_Numeric[Data_Numeric$Measure %in% c("Area",
                                           "Prices",
                                           "Volume",
                                           "Yield"),] 

Data_Numeric <-
  Data_Numeric[Data_Numeric$Crop %in% c("Wheat",
                                           "Barley",
                                           "Oats",
                                           "SugarBeet"),] 

Data_Trimmed <- Data_Numeric[, c(1:2, 41:51)] %>% as.data.frame() 

Data_Pivot <- Data_Trimmed %>% pivot_longer(cols = 3:13, names_to = "Year")
Data_Pivot$Year %<>% as.numeric()


## Reorganise, convert characters to numbers, and export
Data_Pivot %>% 
  pivot_wider(names_from = Year) %>% 
  mutate(across(3:13, as.numeric)) %>% 
  mutate(across(where(is.numeric), round ,digits = 3)) %>%
  data.frame() %>%
  fwrite(sep = ",", here("Output/Tables", 
                    "TableX_MarketDataAllCrops.txt"))

## Reorganise, convert characters to numbers, and export
Data_Pivot %>% 
  pivot_wider(names_from = Measure) %>% 
  mutate(across(3:6, as.numeric)) %>% 
  mutate(across(where(is.numeric), round ,digits = 3)) %>%
  data.frame() %>%
  fwrite(sep = ",", here("Output/Tables", 
                         "TableX_MarketDataAllCrops.txt"))



# ******************************************************************************
#### Section 3: Separating by crop ####
# ******************************************************************************

