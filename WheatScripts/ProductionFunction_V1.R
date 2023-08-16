#### DRUID D1: Intro plotting of wheat data  ###############
# Function: To plot wheat data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 16/08/2023


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


Wheat <- Data[Data$Crop == "Wheat", ] 


# ******************************************************************************
#### Section 2: Cleaning and trimming data ####
# ******************************************************************************


## Trim years column names
Years <- Wheat[3:51] %>% colnames() %>% gsub(pattern = "X", replacement = "") %>% as.numeric()
colnames(Wheat) <- c("Crop", "Measure", Years)

## Drop some incomplete years
Wheat_Trimmed <- Wheat[, c(1:2, 41:51)] 

Wheat_Trimmed_DF <- as.data.frame(x = t(Wheat_Trimmed[, 3:13]), stringsAsFactors = FALSE)
colnames(Wheat_Trimmed_DF) <- Wheat_Trimmed$Measure

Wheat_Trimmed_DF$Area %<>% as.numeric()
Wheat_Trimmed_DF$Yield %<>% as.numeric()
Wheat_Trimmed_DF$Volume %<>% as.numeric()
Wheat_Trimmed_DF$ValueReal %<>% as.numeric()
Wheat_Trimmed_DF$`Milling wheat real` %<>% as.numeric()
Wheat_Trimmed_DF$Area_Total <- Wheat_Trimmed_DF$Area * 1000

Area <- Wheat_Trimmed_DF$Area_Total
Price <- Wheat_Trimmed_DF$`Milling wheat real`
Yield <- Wheat_Trimmed_DF$Yield

# ******************************************************************************
#### Section 3: Export summary data ####
# ******************************************************************************

Wheat_Trimmed_DF[c("Area","Yield", "ValueReal", "Milling wheat real", "Volume")] %>% round(3) %>% write.csv(quote = FALSE)




# ******************************************************************************
#### Section 4: Initial model data ####
# ******************************************************************************

# Wheat_Trimmed_DF$ValuePerHectare <- Wheat_Trimmed_DF$Yield * Wheat_Trimmed_DF$`Milling wheat real`
# 
# Wheat_Trimmed_DF$ValueTotal <- Wheat_Trimmed_DF$ValuePerHectare * Wheat_Trimmed_DF$Area_Total
# 
# Aphids_InjuryLevel <- 5
# 
# YieldLoss <- 4.5 * log(Aphids_InjuryLevel) - 5.5
# 
# LostValue_PerHectare <- (Wheat_Trimmed_DF$Yield * Wheat_Trimmed_DF$`Milling wheat real`) * (YieldLoss %>% divide_by(100))
# 
# LostValue_Total <- (LostValue_PerHectare * Wheat_Trimmed_DF$Area_Total)
# 
# 
# cbind(Wheat_Trimmed_DF$ValueTotal, LostValue_Total) %>% round(3) %>% write.csv(quote = FALSE)


# ******************************************************************************
#### Section 5: First run through ####
# ******************************************************************************


## Initial conditions here
Aphids_Low <- 5
Aphids_Medium <- 7.5
Aphids_High  <- 10


## Presence of natural enemies
NE <- 0.5
# NE <- seq.int(from = 0, to = 1, by = 0.1)


## Data point from the rapid evidence assessment
### Schmidt et al 2004 for wheat
REA <- 0.55


## Modelling density
AphidDensity_Low <- Aphids_Low * (1 - REA * NE)
AphidDensity_Medium <- Aphids_Medium * (1 - REA * NE)
AphidDensity_High <- Aphids_High * (1 - REA * NE)


## YieldLoss percentage
YieldLoss_Low <- 4.5 * log(AphidDensity_Low) - 5.5
YieldLoss_Medium <- 4.5 * log(AphidDensity_Medium) - 5.5
YieldLoss_High <- 4.5 * log(AphidDensity_High) - 5.5


## Lost production value per hectare
LostValue_PH_Low <- (Yield * Price) *  YieldLoss_Low
LostValue_PH_Medium <- (Yield * Price) *  YieldLoss_Medium
LostValue_PH_High <- (Yield * Price) *  YieldLoss_High


## Total loss
TotalLoss_Low <- (Area * LostValue_PH_Low)
TotalLoss_Medium <- (Area * LostValue_PH_Medium)
TotalLoss_High <- (Area * LostValue_PH_High)


