#### DRUID D1: Intro plotting of wheat data  ###############
# Function: To make a function that will output curves for given inputs
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 09/11/2023
# Change:
## - Saving lots and lots of outputs from the loop


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


rm(list = ls())
## Libraries here: ------------------------------------------------------------
## Setting up libraries in order of use in the script
library(here)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(Rfast)
library(ggdist)
library(distributional)


# ******************************************************************************
#### Section 1: Data Importing ####
## Here we import all the crop data and isolate the wheat estimates
# ******************************************************************************


## Import all crops then drop all not wheat
Data <-
  here("MarketData", "AllCropData_V1.xlsx") %>%
  readxl::read_xlsx(sheet = "Sheet1") %>%
  data.frame()


Wheat <- Data[Data$Crop == "Wheat", ]


# ******************************************************************************
#### Section 2: Cleaning and trimming data ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************



## Trim years column names
Years <- Wheat[3:51] %>%
  colnames() %>%
  gsub(pattern = "X", replacement = "") %>%
  as.numeric()

colnames(Wheat) <- c("Crop", "Measure", Years)

## Drop some incomplete years
Wheat_Trimmed <- Wheat[, c(1:2, 41:51)]

Wheat_Trimmed_DF <-
  as.data.frame(x = t(Wheat_Trimmed[, 3:13]), stringsAsFactors = FALSE)
colnames(Wheat_Trimmed_DF) <- Wheat_Trimmed$Measure

Wheat_Trimmed_DF$Area %<>% as.numeric()
Wheat_Trimmed_DF$Yield %<>% as.numeric()
Wheat_Trimmed_DF$Volume %<>% as.numeric()
Wheat_Trimmed_DF$ValueReal %<>% as.numeric()
Wheat_Trimmed_DF$`Milling wheat real` %<>% as.numeric()
Wheat_Trimmed_DF$Area_Total <- Wheat_Trimmed_DF$Area * 1000


## These are used later
Area <- Wheat_Trimmed_DF$Area_Total
Price <- Wheat_Trimmed_DF$`Milling wheat real`
Yield <- Wheat_Trimmed_DF$Yield
Value_PH <- Price * Yield 


## For the table we're going to use the latest year data for now
## Hence 2021 data
Column_Area <- Area[11]
Column_Yield <- Yield[11]
Column_Price <- Price[11]


Variable_Area <- Area
Variable_Yield <- Yield
Variable_Price <- Price
Variable_Years <- Wheat_Trimmed_DF %>% rownames() %>% as.numeric()


# ******************************************************************************
#### Section 3: Define initial variable values ####
## So we use the low/med/high scenarios from Zhang et al. 2018
## We need the following variables:
# area, price, yield [all from the market data above]
## aphids, density, NEs, yield loss [calculated below]
# ******************************************************************************



## Aphids  *****************************************************************
## Aphid density for low/medium/high levels
Aphids_Low <- 5
Aphids_Medium <- 7.5
Aphids_High  <- 10


## NEs  *****************************************************************
# NE <- c(0.00, 0.25, 0.50, (1 - 0.34), 0.75, 1)
# NE <- seq.int(from = 0, to = 1, by = 0.1) %>% c()

NE <- c(0.01, seq.int(from = 0.1, to = 1, by = 0.1))
Column_NE <- rep(
  NE,
  times = 3
)


## Corresponds to: c("Low", "Medium", "High"),
Column_Density <- rep(
  c(5, 7.5, 10),
  each = Column_NE %>% unique() %>% length()
)



# ******************************************************************************
#### Section 4: Useful functions ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************




## Define here for later
## Lots of gsub to fix formatting
Transformer <- function(Input) {
  Input %>%
    round(2) %>% 
    as.numeric() %>%
    format(trim = TRUE, 
           nsmall = 2,
           big.mark = ",") %>%
    paste0("£", .)
}



## Makes the input report zero if negative valued
Constrainer <- function(Input) {
  ifelse(Input < 0,
         0,
         Input) 
}


## Calculate and format the percentage change in LPH
Summarise_LPH_PCT <- function(Data, New, Old) {
  
  Input <- ((Data[, New] - Data[, Old])/Data[, Old]) %>% 
    data.frame() %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% round(2) %>% sprintf("%.2f",. ),
      "% (", 
      sd(.) %>% round(2) %>% sprintf("%.2f",. ), 
      "%)"))) 
  
  return(Input %>% as.character())
  
}



Summarise_LPH_All <- function(Data, New, Old) {
  
  Input <- ((Data[, New] - Data[, Old])) %>% 
    data.frame() %>% 
    summarise(across(everything(), list(
      "Mean" = mean, 
      "Median" = median, 
      "SD" = sd, 
      "Y0" = ~quantile(., 0.05), 
      "Y25" = ~quantile(., 0.25),
      "Y75" = ~quantile(., 0.75), 
      "Y100" = ~quantile(., 0.95))))
  
  return(Input %>% data.frame())
  
}


# ******************************************************************************
#### Section 5: Estimate REA value ####
## Using data from Schmidt et al. 2004
## The REA parameter is important for the interaction of Aphids:NEs
# ******************************************************************************


## Setup  *****************************************************************

# If you have the standard error (SE) and 
# want to compute the standard deviation (SD) from it, 
# simply multiply it by the square root of the sample size.
SampleSize = 32 ## number of plots they report
SEtoSD <- sqrt(SampleSize)
DistSize <- 1000


## Control *****************************************************************

## Write down the parameters from the REA ##
## then generate a distribution ##
Control_Mean <- 366
Control_SE <- 27
Control_SD <- Control_SE %>% multiply_by(SEtoSD)
Control_Distribution <- rnorm(n = DistSize, mean = Control_Mean, sd = Control_SD) 


## Treatment *****************************************************************

Treatment_Mean <- 83
Treatment_SE <- 7.60
Treatment_SD <- Treatment_SE %>% multiply_by(SEtoSD)
Treatment_Distribution <- rnorm(n = DistSize, mean = Treatment_Mean, sd = Treatment_SD) 

## PCT Change  ****************************************************************

## Calculate mean change from the entire distribution ##
REA_Old <- 0.773224 ## (83-366)/366
REA_New <- (((Treatment_Distribution - Control_Distribution) / Control_Distribution)) %>% abs()


## Now constraint to [0, 1]
REA_New <- ifelse(REA_New > 1, 
                  1, 
                  ifelse(REA_New < 0 , 
                         0 , 
                         REA_New))


# ******************************************************************************
#### Section 6: Set up the loop ####
# ******************************************************************************


## Initialise values here
ColLength <- Column_NE %>% length()
YearLength <- Variable_Years %>% length()

## Nrow = 15: 5 levels of NEs * 3 scenarios for aphids
## Ncol = 6: Mean +  SD for 3 variables: YL, LPH, LT
Estimates <- matrix(0, nrow = ColLength, ncol = 8) %>% data.frame()


## Filling in per the annual loops
## Yield loss raw data and mean, sd
Annual_YL <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Aphid densities
Annual_AD_Raw_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_AD_Raw_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_AD_Formatted <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss per hectare
Annual_LPH <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeMeans <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeSDs <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss per hectare moments
Annual_LPH_Raw_Change_Quantile_Y0 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y25 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y50 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y75 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y100 <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss total
Annual_LT <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Raw <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_Mean <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LT_SD <- matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()



YieldLosss_Total_RawData <- matrix(0, 
                                   nrow = DistSize, 
                                   ncol = ColLength) %>% data.frame()
LossPerHectare_Total_RawData <- matrix(0, 
                                       nrow = DistSize, 
                                       ncol = ColLength) %>% data.frame()
LossTotal_Total_RawData <- matrix(0, 
                                  nrow = DistSize, 
                                  ncol = ColLength) %>% data.frame()


# datalist_LPH_Means = vector("list", length = length(Variable_Years))
# datalist_LPH_SDs = vector("list", length = length(Variable_Years))





## Initialise values here
Threshold <- 5
Insecticide_PH <- 282

Field_NS <- 0.009
Field_ET <- 0.37
Field_AS <- 1 - Field_NS - Field_ET


# ******************************************************************************
#### Section 7: Loop ####
# ******************************************************************************

## Loop through each density of aphids and report a distribution of estimates ##
## then report mean (SD) from that distribution ##


for (Year in 1:length(Variable_Years)) {
  for (i in 1:length(Column_NE)) {
    
    
    ## Defining pests  *****************************************************************
    ## This is the central calculation
    ## For each value of aphids and NEs, we simulate the response
    Dist <- c(Column_Density[i] * (1 - (REA_New * Column_NE[i])))
    Dist %<>% ifelse(. == 0, 
                     0.001, 
                     .) ## TESTING: whether true zero causes issues
    
    
    # **************************************************************************
    ## YIELD LOSS UNDER NEVER-SPRAY CONDITIONS
    ## They always spray so don't lose anything but add insecticide 
    YieldLoss_NS <- Dist %>% 
      log() %>% 
      multiply_by(4.5) %>% 
      subtract(5.5) %>% 
      divide_by(100) %>% 
      ifelse(is.infinite(.), 0, .) %>% 
      ifelse(. < 0, 0, .)
    
    
    LossPerHectare_NS <-
      (Variable_Yield[Year] * Variable_Price[Year]) *  YieldLoss_NS
    
    
    ## Now calculate loss per hectare and total
    LossTotal_NS <- (Variable_Area[Year] * LossPerHectare_NS)
    
    
    
    # **************************************************************************
    ## YIELD LOSS UNDER ECONOMIC THRESHOLD CONDITIONS
    
    
    (100 / length(Dist) * sum(Dist > Threshold)) %>% divide_by(100) %>% multiply_by(LossPerHectare_NS) %>% summary()
    
    
    
    
    YieldLoss_ET <- ifelse(Dist >= Threshold, 
                           0, 
                           Dist) %>%  ## so only loss if density > threshold 
      log() %>% ## take logs
      multiply_by(4.5) %>% ## 4.5ln(AphidDensity)
      subtract(5.5) %>%  ## - 5.5
      divide_by(100) %>% ## get as percentage change
      ifelse(is.infinite(.), 0, .) %>% ## change infinites to zeroes
      ifelse(. < 0, 0.0000001, .) ## change negatives to 0.0000001
    
    
    LossPerHectare_ET <- ifelse(
      YieldLoss_ET == 0,
      Insecticide_PH, ## zero where dist>threshold for spraying
      ifelse(
        YieldLoss_ET == 0.0000001,##placeholder value for low dist
        0,
        YieldLoss_ET %>% 
          multiply_by(Variable_Yield[Year] * Variable_Price[Year])))
    
    
    LossTotal_ET <- (Variable_Area[Year] * LossPerHectare_ET)
    
    
    ## Now cover the placeholder
    YieldLoss_ET <- YieldLoss_ET %>% ifelse(. == 0.0000001, 0, .)
    
    
    # **************************************************************************
    ## YIELD LOSS UNDER ALWAYS-SPRAY CONDITIONS
    ## They always spray so don't lose anything but add insecticide 
    YieldLoss_AS <- 0 
    
    LossPerHectare_AS <- (Variable_Yield[Year] * Variable_Price[Year])  %>% ## per hectare value
      multiply_by(YieldLoss_AS) %>% ## lost yield percent times value
      add(Insecticide_PH) ## plus cost of sprays
    
    LossTotal_AS <- (Variable_Area[Year] * LossPerHectare_AS)
    
    
    ## WEIGHTED FIGURES  *****************************************************************
    YieldLoss_Total <-
      (YieldLoss_NS * Field_NS) +
      (YieldLoss_ET * Field_ET) +
      (YieldLoss_AS * Field_AS)
    YieldLosss_Total_RawData[, i] <- YieldLoss_Total
    
    
    LossPerHectare <-
      (LossPerHectare_NS * Field_NS) +
      (LossPerHectare_ET * Field_ET) +
      (LossPerHectare_AS * Field_AS)
    LossPerHectare_Total_RawData[, i] <- LossPerHectare
    
    
    LossTotal_Total <-
      (LossTotal_NS * Field_NS) +
      (LossTotal_ET * Field_ET) +
      (LossTotal_AS * Field_AS)
    LossTotal_Total_RawData[, i] <- LossTotal_Total
    
    
    ## FILL COLUMNS  *****************************************************************
    ## Furnish each column with mean and sd of each distribution
    
    Estimates[i, 1] <- YieldLoss_Total %>% mean(na.rm = TRUE)
    Estimates[i, 2] <- YieldLoss_Total %>% sd(na.rm = TRUE)
    
    Estimates[i, 3] <- LossPerHectare %>% mean(na.rm = TRUE)
    Estimates[i, 4] <- LossPerHectare %>% sd(na.rm = TRUE)
    
    Estimates[i, 5] <- LossTotal_Total %>% mean(na.rm = TRUE)
    Estimates[i, 6] <- LossTotal_Total %>% sd(na.rm = TRUE)
    
    
    ## This should really be cols 1 and 2 but adding later so for ease they're going 7, 8 
    Estimates[i, 7] <- Dist %>% mean(na.rm = TRUE)
    Estimates[i, 8] <- Dist %>% sd(na.rm = TRUE)
    
    Output_AD_Raw_Mean <- Estimates[, 7]    
    Output_AD_Raw_SD <- Estimates[, 8]    
    Output_AD_Formatted <- paste0(
      Estimates[, 7] %>% round(2) %>% sprintf("%.2f",. ),
      " (\u00B1 ",
      Estimates[, 8] %>% round(2) %>% sprintf("%.2f",. ),
      ")")
    
    
    
    ## OUTPUT NICELY: YIELD  *****************************************************************
    Output_Yield <- paste0(
      Estimates[, 1] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"),
      " (",
      Estimates[, 2] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ),
      ")"
    )
    
    Output_Yield_Raw <- Estimates[, 1] %>% multiply_by(100)
    
    
    
    ## OUTPUT NICELY: LPH  *****************************************************************
    Output_LPH <- paste0(
      Estimates[, 3] %>% Constrainer() %>% round(2) %>% sprintf("%.2f",. ),
      " (",
      Estimates[, 4] %>% Constrainer() %>% round(2) %>% sprintf("%.2f",. ),
      ")"
    )
    
    Output_LPH_Raw_Mean <- Estimates[, 3]
    Output_LPH_Raw_SD <- Estimates[, 4]
    
    
    ## OUTPUT NICELY: LT  *****************************************************************
    Output_LT <- paste0(
      Estimates[, 5] %>% 
        Constrainer() %>% 
        divide_by(1000000) %>% 
        Transformer() %>% 
        paste0(., "m"),
      " (",
      Estimates[, 6] %>% 
        Constrainer() %>% 
        divide_by(1000000) %>% 
        Transformer() %>% 
        paste0(., "m"),
      ")"
    )
    
    Output_LT_Raw <- Estimates[, 5]
    # ## Changes in LT compared to baseline
    # ChangeInTotaLoss <- rbind(
    #   0,
    #   Estimates[1 , 3] - Estimates[2 , 3],
    #   Estimates[1 , 3] - Estimates[3 , 3],
    #   Estimates[1 , 3] - Estimates[4 , 3],
    #   Estimates[1 , 3] - Estimates[5 , 3],
    #   0,
    #   Estimates[6 , 3] - Estimates[7 , 3],
    #   Estimates[6 , 3] - Estimates[8 , 3],
    #   Estimates[6 , 3] - Estimates[9 , 3],
    #   Estimates[6 , 3] - Estimates[10 , 3],
    #   0,
    #   Estimates[11 , 3] - Estimates[12 , 3],
    #   Estimates[11 , 3] - Estimates[13 , 3],
    #   Estimates[11 , 3] - Estimates[14 , 3],
    #   Estimates[11 , 3] - Estimates[15 , 3]
    # )
    
    
  }
  
  
  # datalist_LPH_Means[[Year]] <- LossPerHectare_Total_RawData[, i]
  
  
  ## Stitch together across years
  Annual_AD_Raw_Mean[, Year] <- Output_AD_Raw_Mean
  Annual_AD_Raw_SD[, Year] <- Output_AD_Raw_SD
  Annual_AD_Formatted[, Year] <- Output_AD_Formatted
  
  Annual_YL[, Year] <- Output_Yield
  Annual_YL_Raw[, Year] <- Output_Yield_Raw
  
  
  Annual_LPH_Raw_Mean[, Year] <- Output_LPH_Raw_Mean
  Annual_LPH_Raw_SD[, Year] <- Output_LPH_Raw_SD
  
  
  Annual_LT[, Year] <- Output_LT
  Annual_LT_Raw[, Year] <- Output_LT_Raw
  
  
  # for (i in 1:length(Column_NE)) {
  # 
  #   Summarise_LPH_Mean(LossPerHectare_Total_RawData, 
  #                      (length(Column_NE) / 3 * 1), 
  #                      (length(Column_NE) / 3 * 1) - i +1)
  #   }
  # 
  
  # Annual_YL_Mean[, Year] <- rbind(
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 1),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 2),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 3),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 4),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 5),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 1), 6),
  #   
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 7),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 8),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 9),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 10),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 11),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, (length(Column_NE) / 3 * 2), 12),
  #   
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 13),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 14),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 15),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 16),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 17),
  #   Summarise_LPH_PCT(YieldLosss_Total_RawData, Column_NE %>% length(), 18)
  # )
  # 
  # 
  # Annual_LPH_Mean[, Year] <- rbind(
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 1),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 2),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 3),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 4),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 5),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 6, 6),
  #   
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 7),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 8),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 9),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 10),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 11),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 12, 12),
  #   
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 13),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 14),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 15),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 16),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 17),
  #   Summarise_LPH_PCT(LossPerHectare_Total_RawData, 18, 18)
  # )
  # 
  # 
  
  Looper <- data.frame(
    "Left" = rep(c(11, 22, 33), each = 11) , 
    "Right" = seq.int(from = 1, to = 33, by = 1))
  
  for (Row in 1:nrow(Looper)){
    
    
    Output <- Summarise_LPH_All(LossPerHectare_Total_RawData, 
                                Looper[Row, 1], 
                                Looper[Row, 2])
    
    ## Store output
    Annual_LPH_Raw_ChangeMeans[Row, Year] <- Output["._Mean"] %>% as.numeric()
    Annual_LPH_Raw_ChangeSDs[Row, Year] <- Output["._SD"] %>% as.numeric()
    
    Annual_LPH_Raw_Change_Quantile_Y0[Row, Year] <- Output["._Y0"] %>% as.numeric()
    Annual_LPH_Raw_Change_Quantile_Y25[Row, Year] <- Output["._Y25"] %>% as.numeric()
    Annual_LPH_Raw_Change_Quantile_Y50[Row, Year] <- Output["._Median"] %>% as.numeric()
    Annual_LPH_Raw_Change_Quantile_Y75[Row, Year] <- Output["._Y75"] %>% as.numeric()
    Annual_LPH_Raw_Change_Quantile_Y100[Row, Year] <- Output["._Y100"] %>% as.numeric()
  }
  
  
}









# ******************************************************************************
#### Section 8: LABEL OUTPUTS ####
# ******************************************************************************


## Compile formatted outputs *************************************************
colnames(Annual_YL) <- Variable_Years
colnames(Annual_LPH) <- Variable_Years
colnames(Annual_LT) <- Variable_Years


## Label raw data  *************************************************
colnames(Annual_YL_Raw) <- Variable_Years
colnames(Annual_LPH_Raw) <- Variable_Years
colnames(Annual_LT_Raw) <- Variable_Years


## Label raw data  *************************************************
colnames(Annual_YL_Mean) <- Variable_Years



colnames(Annual_AD_Raw_Mean) <- Variable_Years
colnames(Annual_AD_Raw_SD) <- Variable_Years
colnames(Annual_AD_Formatted) <- Variable_Years


## Report annual average pest density
Column_AD <- paste0(
  Annual_AD_Raw_Mean %>% as.matrix() %>% rowmeans() %>% round(2) %>% sprintf("%.2f",. ),
  " (",
  Annual_AD_Raw_SD %>% as.matrix() %>% rowmeans()  %>% round(2) %>% sprintf("%.2f",. ),
  ")"
)



colnames(Annual_LPH_Mean) <- Variable_Years
# colnames(Annual_LPH_SD) <- Variable_Years
colnames(Annual_LT_Mean) <- Variable_Years
# colnames(Annual_LT_SD) <- Variable_Years


colnames(Annual_LPH_Raw_ChangeMeans) <- Variable_Years
colnames(Annual_LPH_Raw_ChangeSDs) <- Variable_Years

# ******************************************************************************
#### Section X1: Plot everything ####
# ******************************************************************************


## Plot Change vs NE facet by density and grouped by year 
Test_LPH_Means <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  Annual_LPH_Raw_ChangeMeans
) 


Test_LPH_SD <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  Annual_LPH_Raw_ChangeSDs
) 


# Test_Pivot <- bind_cols(
#   "Crop" = "Wheat",
#   Test_LPH_Means %>% pivot_longer(
#     cols      = 3:13,
#     names_to  = c("Year"),
#     values_to = c("Mean")
#   ),
#   Test_LPH_SD %>% pivot_longer(
#     cols      = 3:13,
#     names_to  = c("Year"),
#     values_to = c("SD")
#   ) %>% select("SD")
# )
# 
# 
# 
# Test_Pivot %>%
#   ggplot(aes(y = NE %>% as.factor(), 
#              xdist = dist_normal(Mean[, 1], SD[, 1]),
#              group = Year %>% as.factor(), 
#              fill = Year %>% as.factor())) +
#   stat_halfeye(normalize = "groups", 
#                position = position_dodge(width = 1.25)) + 
#   theme_bw() +
#   facet_wrap( ~ Density %>% as.factor()) +
#   scale_fill_discrete() + 
#   geom_vline(xintercept = 0) +
#   scale_x_continuous(limits = c(-1, 1), #
#                      breaks = seq.int(from = -1, to = 1, by = 0.25), 
#                      name = "Change in LPH")



# ******************************************************************************
#### Section X2: Plot all years average ####
# ******************************************************************************



## Plot Change vs NE facet by density and grouped by year 
Test_LPH_AllYears <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "Means" = Annual_LPH_Raw_ChangeMeans %>% as.matrix() %>% rowmeans(),
  "SD" = Annual_LPH_Raw_ChangeSDs %>% as.matrix() %>% rowmeans())



Test_LPH_AllYears_Quantiles <- cbind(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "Y0" = Annual_LPH_Raw_Change_Quantile_Y0 %>% as.matrix() %>% rowmeans(),
  "Y25" = Annual_LPH_Raw_Change_Quantile_Y25 %>% as.matrix() %>% rowmeans(),
  "Y50" = Annual_LPH_Raw_Change_Quantile_Y50 %>% as.matrix() %>% rowmeans(),
  "Y75" = Annual_LPH_Raw_Change_Quantile_Y75 %>% as.matrix() %>% rowmeans(),
  "Y100" = Annual_LPH_Raw_Change_Quantile_Y100 %>% as.matrix() %>% rowmeans()) %>% 
  data.frame()


PlotData <- Test_LPH_AllYears_Quantiles %>% data.frame() %>% pivot_longer(cols = Y0:Y100, names_to = "variable")

## Reshape here so that I can add the errorbars
# NewerData_Pivoted <- NewerData %>% pivot_longer(cols = y0:y100)


# PlotData %>% ggplot(
#   aes(
#     x = NE,
#     y = value,
#     fill = Density %>% as.factor(),
#     group = Density %>% as.factor())) +
#   stat_boxplot(geom = "errorbar",
#                width = 0.25,
#                position = position_dodge(width = 1.5)) +
# 
#   geom_boxplot(outlier.shape = NA) +
# 
#   theme_bw()


# ggplot(Test_LPH_AllYears_Quantiles, aes(x = NE, 
#                      fill = as.factor(Density),
#                      group = as.factor(Density)))+
#   geom_errorbar(aes(
#     ymin = Y0,
#     ymax = Y100,
#   ),width = 0.2)+ ## errorbar means you can have the nicer whiskers
#   geom_boxplot(
#     varwidth = 0.5,
#     outlier.shape = NA,
#     aes(
#       ymin = Y0,
#       lower = Y25,
#       middle = Y50,
#       upper = Y75,
#       ymax = Y100,
#     ),
#     stat = "identity" ## means you can specify moments as in AES()
#   )
# 
# 





## Output this
Example0 <- Test_LPH_AllYears %>% data.frame() %>%
  ggplot(
    aes(
      y = NE %>% as.factor(),
      xdist = dist_normal(Means, SD) %>% dist_truncated(upper = 0),
      group = Density %>% as.factor(),
      fill = Density %>% as.factor()
    )
  ) +
  stat_histinterval(normalize = "groups",
                    position = position_dodge(width = 1.1)) +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_fill_manual(name = "Initial pest density in aphids/t",
                    values = RColorBrewer::brewer.pal(n = 9, 
                                                      name = "Blues")[c(5, 7, 9)], 
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -250, to = 100, by = 25)) +
  scale_y_discrete(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  ) +
  ggtitle("Wheat")


ggsave(
  Example0,
  device = "jpeg",
  filename = paste0(here(), "/Output/Figures/FigureZ_Wheat_Plotting_Example0.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 250
)



PlotData <- Test_LPH_AllYears %>% data.frame() 

## Long line plot
PlotData %>% 
  ggplot(
    aes(
      y = NE %>% as.numeric(),
      x = Means %>% as.numeric(),
      group = Density %>% as.factor(),
      colour = Density %>% as.factor()
    )
  ) +
  geom_line() + geom_point() +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_fill_brewer(palette = "Blues", name = "Pest density") +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  scale_y_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  coord_flip()



## Stat_smooth method
### using linetype = 0 to hide loess line
Example1 <- ggplot(PlotData,
                   aes(Means, NE, group = Density, colour = Density %>% as.factor()))+
  geom_smooth(orientation = "y", se = TRUE, linetype = 0) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = 0) +
  scale_colour_manual(name = "Pest density", values = RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(5, 7, 9)]) +
  scale_x_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  scale_y_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  coord_flip() +
  ggtitle("Wheat")



ggsave(
  Example1,
  device = "jpeg",
  filename = paste0(here(), "/Output/Figures/FigureZ_Wheat_Plotting_Example1.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 250
)


PlotData %>% 
  data.frame() %>% 
  fwrite(sep = ",",
         here("Output/Tables",
              "TableZ_Wheat_PlotData.csv"))


## Modify upper bound to not exceed zero
PlotData$Ymax <- (PlotData$Means + PlotData$SD) %>% ifelse(. > 0, 0, .)
PlotData$Ymin <- (PlotData$Means - PlotData$SD) %>% ifelse(. > 0, 0, .)


Example2 <- PlotData %>% 
  ggplot(
    aes(
      x = NE %>% as.numeric(),
      group = Density %>% as.factor())) + 
  
  geom_line(aes(y = Means, color = Density %>% as.factor()), 
            linewidth = 1) +
  
  geom_point(aes(y = Means, color = Density %>% as.factor())) + 
  
  geom_ribbon(aes(y = Means, 
                  ymin = Ymin, 
                  ymax = Ymax, 
                  fill = Density %>% as.factor()), 
              outline.type = "both", alpha= 0.2) +
  
  theme_bw() +
  
  geom_vline(xintercept = 0) +
  
  scale_color_manual(name = "Pest density", 
                     values = c("#C6DBEF", "#4292C6", "black")) +
  
  scale_fill_manual(name = "Pest density", 
                    values = c("#C6DBEF", "#4292C6", "black")) +
  
  scale_y_continuous(name = "Change in lost yield against 100% presence",
                     breaks = seq.int(from = -120, to = 0, by = 10)) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.1),
    labels  = seq.int(from = 0, to = 1, by = 0.1) %>%
      sprintf("%.2f", .)
  )  +
  
  ggtitle("Wheat")


ggsave(
  Example2,
  device = "jpeg",
  filename = paste0(here(), "/Output/Figures/FigureZ_Wheat_T5_Plotting_Example2.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 250
)



# ******************************************************************************
#### Section 9: Calculate aggregates ####
# ******************************************************************************


## Mean (SD) Yield loss 
Column_YL_Mean <- paste0(
  Annual_YL_Raw %>% 
    as.matrix() %>%  
    Rfast::rowmeans() %>% 
    Constrainer() %>% 
    multiply_by(100) %>% 
    round(2) %>% 
    sprintf("%.2f",. ), 
  "% (\u00B1 ", 
  Annual_YL_Raw %>% 
    as.matrix() %>% 
    Rfast::rowmeans() %>% 
    Constrainer() %>% 
    multiply_by(100) %>% 
    round(2) %>% 
    sprintf("%.2f",. ),
  "%)")

## Aggregated yield loss 
Column_YL_Total <- Annual_YL_Raw %>% as.matrix() %>% rowsums()



## Mean (SD) lph
Column_LPH_Mean <- paste0(
  Annual_LPH_Raw_Mean %>% 
    as.matrix() %>%  
    Rfast::rowmeans() %>% 
    round(2) %>% 
    sprintf("£%.2f",. ), 
  " (\u00B1 ", 
  Annual_LPH_Raw_SD %>% 
    as.matrix() %>% 
    Rfast::rowVars(std = TRUE) %>% 
    round(2) %>% 
    sprintf("£%.2f",. ),
  ")")


## Mean (SD) lph
Column_LPH_Change <- paste0(
  Annual_LPH_Raw_ChangeMeans %>% 
    as.matrix() %>%  
    Rfast::rowmeans() %>% 
    round(2) %>% 
    sprintf("£%.2f",. ), 
  " (\u00B1 ", 
  Annual_LPH_Raw_ChangeSDs %>% 
    as.matrix() %>% 
    Rfast::rowVars(std = TRUE) %>% 
    round(2) %>% 
    sprintf("£%.2f",. ),
  ")")

## Aggregated losses per hectare
Column_LPH_Total <- Annual_LPH_Raw_Mean %>% as.matrix() %>% rowsums() %>% 
  round(2) %>% 
  format(nsmall = 2,
         big.mark = ",",
         zero.print = TRUE) %>%
  paste0("£", .)




## Mean (SD) total losses
Column_LT_Mean <- paste0(
  Annual_LT_Raw %>% 
    as.matrix() %>%  
    Rfast::rowmeans() %>% 
    divide_by(1000000) %>% 
    round(2) %>% 
    format(nsmall = 2,
           big.mark = ",",
           zero.print = TRUE) %>%
    paste0("£", ., "m"), 
  " (\u00B1 ", 
  Annual_LT_Raw %>% 
    as.matrix() %>% 
    Rfast::rowVars(std = TRUE) %>% 
    divide_by(1000000) %>% 
    round(2) %>% 
    format(nsmall = 2,
           big.mark = ",",
           zero.print = TRUE) %>%
    paste0("£", ., "m"),
  ")")


## Aggregated total loss 
Column_LT_Total <- Annual_LT_Raw %>% 
  as.matrix() %>% 
  rowsums() %>% 
  divide_by(1000000) %>% 
  round(2) %>% 
  format(nsmall = 2,
         big.mark = ",",
         zero.print = TRUE) %>%
  paste0("£", ., "m")


# ******************************************************************************
#### Section 10: Put the table together ####
## All columns go here
# ******************************************************************************


## Setup new design here

TableZ <- bind_cols(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "AD" = Column_AD,
  "YL" = Column_YL_Mean,
  "Average LPH" = Column_LPH_Mean,
  Annual_LPH[, c("2017", "2018", "2019", "2020", "2021")],
  "Total LT" = Column_LT_Total
)


TableZ_Limited <- bind_cols(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "AD" = Column_AD,
  "YL" = Column_YL_Mean,
  "Average LPH" = Column_LPH_Mean,
  "Change in LPH" = Column_LPH_Change
)



TableZ %>% data.frame()

# ******************************************************************************
#### Section 11: Exporting the table ####
# ******************************************************************************



TableZ %>% data.frame() %>% fwrite(sep = "#",
                                   here("Output/Tables", 
                                        "TableZ_OutputSummaryForWheat.txt"))


