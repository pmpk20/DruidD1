#### DRUID D1: Intro plotting of OSR data  ###############
# Function: To do sensitivity analysis with OSR data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 06/06/2024
# Change:
## - This version does sensitivity analysis


# *****************************************************************************
#### Section 0: Setting up ####
## NOTES: This reports (a) system information and (b) imports packages.
# *****************************************************************************



## sessionInfo()---------------------------------------------------------------
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8
# [2] LC_CTYPE=English_United Kingdom.utf8
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C
# [5] LC_TIME=English_United Kingdom.utf8
# time zone: Europe/London
# tzcode source: internal
# other attached packages:
# [1] patchwork_1.1.3.9000 distributional_0.3.2 ggdist_3.3.1
# [4] Rfast_2.1.0          RcppParallel_5.1.7   RcppZiggurat_0.1.6
# [7] Rcpp_1.0.11          data.table_1.15.0    RColorBrewer_1.1-3
# [10] lubridate_1.9.2      forcats_1.0.0        stringr_1.5.0
# [13] dplyr_1.1.3          purrr_1.0.2          readr_2.1.4
# [16] tidyr_1.3.0          tibble_3.2.1         tidyverse_2.0.0
# [19] ggplot2_3.4.4        readxl_1.4.3         magrittr_2.0.3
# [22] here_1.0.1
# [1] gtable_0.3.4      compiler_4.3.1    tidyselect_1.2.0  parallel_4.3.1
# [5] scales_1.3.0      R6_2.5.1          generics_0.1.3    munsell_0.5.0
# [9] rprojroot_2.0.4   pillar_1.9.0      tzdb_0.4.0        rlang_1.1.1
# [13] utf8_1.2.3        stringi_1.7.12    timechange_0.2.0  cli_3.6.1
# [17] withr_2.5.2       grid_4.3.1        rstudioapi_0.15.0 hms_1.1.3
# [21] lifecycle_1.0.4   vctrs_0.6.3       glue_1.6.2        farver_2.1.1
# [25] cellranger_1.1.0  fansi_1.0.4       colorspace_2.1-0  tools_4.3.1
# [29] pkgconfig_2.0.3


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


## Only run these if not already installed
# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
library(patchwork)



## Define here then call this wherever
TextSize <- 10

# ******************************************************************************
#### Section 1: Data Importing ####
## Here we import all the crop data and isolate the OSR estimates
# ******************************************************************************


## Import all crops then drop all not OSR
Data <-
  here("MarketData", 
       "AllCropData_V2.xlsx") %>% 
  readxl::read_xlsx(sheet = "Sheet1") %>% 
  data.frame()


OSR <- Data[Data$Crop == "OSR", ]


# ******************************************************************************
#### Section 2: Cleaning and trimming data ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************



## Trim years column names
Years <- OSR[3:51] %>% 
  colnames() %>% 
  gsub(pattern = "X", 
       replacement = "") %>% 
  as.numeric()
colnames(OSR) <- c("Crop", "Measure", Years)


## Labels for ease of reference
colnames(OSR) <- c("Crop", "Measure", Years)


## Drops years that aren't 2011:2021
OSR_Trimmed <- OSR[, c(1:2, 41:51)]


## transpose with t() then convert strings to factors
OSR_Trimmed_DF <-
  as.data.frame(x = t(OSR_Trimmed[, 3:13]), 
                stringsAsFactors = FALSE)
colnames(OSR_Trimmed_DF) <- OSR_Trimmed$Measure


## Use mutate() to convert selected columns to numeric
## Also add a new total area column going from 1000s HA to HA
OSR_Trimmed_DF <- OSR_Trimmed_DF %>%
  mutate(across(
    c(
      Area,
      Yield,
      Volume,
      ValueReal,
      PriceReal,
      WinterSpraysCostsReal,
      InsecticidesOnlyReal
    ),
    as.numeric
  ),
  Area_Total = Area * 1000)


## These are used later so we make them their own variables
Area <- OSR_Trimmed_DF$Area_Total
Price <- OSR_Trimmed_DF$PriceReal
Yield <- OSR_Trimmed_DF$Yield
Value_PH <- Price * Yield
Sprays <- OSR_Trimmed_DF$WinterSpraysCostsReal
Sprays_Insecticides <- OSR_Trimmed_DF$InsecticidesOnlyReal


## As above but named differently
Variable_Area <- Area
Variable_Yield <- Yield
Variable_Price <- Price
Variable_Sprays <- Sprays
Variable_Insecticides <- Sprays_Insecticides
Variable_Years <- OSR_Trimmed_DF %>% rownames() %>% as.numeric()


# ******************************************************************************
#### Section 3: Define initial variable values ####
## So we use the low/med/high scenarios from Zhang et al. 2018
## We need the following variables:
# area, price, yield [all from the market data above]
## aphids, density, NEs, yield loss [calculated below]
# ******************************************************************************




## Aphids  *****************************************************************
## Initial pest density for low/medium/high levels
Aphids_Low <- 5
Aphids_Medium <- 7.5
Aphids_High  <- 10


## NEs  *****************************************************************
## Range of NE presence
## NOTE: Use 0.01 as 0.00 causes issues
NE <- c(0.01, seq.int(from = 0.1, to = 1, by = 0.1))
Column_NE <- rep(NE,
                 times = 3)


## Corresponds to: c("Low", "Medium", "High"),
## Repeat the vector for each NE density
Column_Density <- rep(c(5, 7.5, 10),
                      each = Column_NE %>% unique() %>% length())


# ******************************************************************************
#### Section 4: Useful functions ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************



## Makes the input report zero if negative valued
## Anonymous AI said use this instead: pmax(Input, 0)
Constrainer <- function(Input) {
  pmax(Input, 0)
}



## Gives summary stats for losses per hectare
## using rfast because I can
Summarise_LPH_All <- function(Data, New, Old) {
  difference <- (Data[, New] - Data[, Old]) %>% as.matrix()
  
  summary_stats <- c(
    Rfast::colmeans(difference),
    Rfast::colVars(std = TRUE, difference)
  )
  
  names(summary_stats) <- c("Mean", "SD")
  
  return(summary_stats)
}




Pivoter <- function(PlotData) {
  PlotData %>% pivot_longer(
    cols = !c(Density, NE, Crop),
    names_to = c("Type", "Measure"),
    names_sep = "_"
  ) %>%
    pivot_wider(names_from = Measure, values_from = value)
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
SampleSize <- 36 ## number of plots they report
SEtoSD <- sqrt(SampleSize)
DistSize <- 1000

## Control *****************************************************************

## Write down the parameters from the REA ##
## then generate a distribution ##
Control_Mean <- 137.9444444
Control_SD <- 0.394268819
Control_Distribution <-
  rnorm(n = DistSize, mean = Control_Mean, sd = Control_SD)


## Treatment *****************************************************************

Treatment_Mean <- 77.16666667
Treatment_SD <- 0.227835165
Treatment_Distribution <-
  rnorm(n = DistSize, mean = Treatment_Mean, sd = Treatment_SD)

## PCT Change  ****************************************************************

## Calculate mean change from the entire distribution ##
REA_Old <- 0.440596053 ## (83-366)/366
REA_New <-
  (((Treatment_Distribution - Control_Distribution) / Control_Distribution
  )) %>% abs()


## Now constraint to [0, 1]
REA_New <- ifelse(REA_New > 1,
                  1,
                  ifelse(REA_New < 0 ,
                         0 ,
                         REA_New))


# ******************************************************************************
#### Section 6: Set up the loop ####
## INITIALISING INPUTS HERE SO VERY REPETITIVE
# ******************************************************************************


## Initialise values here
ColLength <- Column_NE %>% length()
YearLength <- Variable_Years %>% length()


## Store first loop outputs here
## Rows: scenarios of pest and NE density
## cols: variables we're storing data of
## note we store mean and sd not the distributions
Estimates <- matrix(0,
                    nrow = ColLength,
                    ncol = 26) %>% data.frame()


## Filling in per the annual loops
## Yield loss raw data and mean, sd
Annual_YL_NS_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_ET_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_AS_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()

Annual_YL_NS_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_ET_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_AS_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()

Annual_YL_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_YL_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Aphid densities
Annual_AD_Raw_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_AD_Raw_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_AD_Formatted <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss per hectare
## means, SD, untransformed raw values, changes, quantiles
## NS: Never Spray, AS: Always Spray, ET: Economic Threshold
## NOTE: Some of these are no longer used but can't work out which
Annual_LPH <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
# Annual_LPH_Mean <-
#   matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_NS_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ET_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_AS_Mean <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()

Annual_LPH_Raw_NS_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ET_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_AS_SD <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()

Annual_LPH_Raw_ChangeMeans <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeSDs <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()



Annual_LPH_Raw_ChangeMeans_NS <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeSDs_NS <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeMeans_AS <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeSDs_AS <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeMeans_ET <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_ChangeSDs_ET <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss per hectare moments
Annual_LPH_Raw_Change_Quantile_Y0 <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y25 <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y50 <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y75 <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
Annual_LPH_Raw_Change_Quantile_Y100 <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()


## Loss total
Annual_LT_Raw <-
  matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
# Annual_LT_Mean <-
#   matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()
# Annual_LT_SD <-
#   matrix(0, nrow = ColLength, ncol = YearLength) %>% data.frame()



YieldLoss_Total_RawData <- matrix(0,
                                  nrow = DistSize,
                                  ncol = ColLength) %>% data.frame()
YieldLoss_NS_RawData <- matrix(0,
                               nrow = DistSize,
                               ncol = ColLength) %>% data.frame()
YieldLoss_ET_RawData <- matrix(0,
                               nrow = DistSize,
                               ncol = ColLength) %>% data.frame()
YieldLoss_AS_RawData <- matrix(0,
                               nrow = DistSize,
                               ncol = ColLength) %>% data.frame()



LossPerHectare_Total_RawData <- matrix(0,
                                       nrow = DistSize,
                                       ncol = ColLength) %>% data.frame()

LossPerHectare_NS_RawData <- matrix(0,
                                    nrow = DistSize,
                                    ncol = ColLength) %>% data.frame()
LossPerHectare_ET_RawData <- matrix(0,
                                    nrow = DistSize,
                                    ncol = ColLength) %>% data.frame()
LossPerHectare_AS_RawData <- matrix(0,
                                    nrow = DistSize,
                                    ncol = ColLength) %>% data.frame()


LossTotal_Total_RawData <- matrix(0,
                                  nrow = DistSize,
                                  ncol = ColLength) %>% data.frame()

## Once used but scared to delete
# datalist_LPH_Means = vector("list", length = length(Variable_Years))
# datalist_LPH_SDs = vector("list", length = length(Variable_Years))


# ******************************************************************************
#### Section 6B: Initialise insecticides for loop ####
## Experimental
# ******************************************************************************


## Field proportions here
Field_NS <- 0.007
Field_ET <- 0.37
Field_AS <- 0.623



## Initialise economic threshold here
## FUTURE WORK: Update this to be flexible
Threshold_Default <- 5


## OLD VERSION USED A STATIC FIGURE
# Insecticide_PH <- 282


## We are now updating to use one figure per year
## Figure source: John Nix Farm Management Pocketbooks (Thanks Tree!)
Insecticide_PH <- Variable_Sprays
Insecticide_Only_PH <- (Variable_Sprays - Variable_Insecticides)


## Initialising
Dist <- matrix(nrow = 1000, ncol = 33, data = 0) %>% as.data.frame()



# ******************************************************************************
#### Section 7: Loop ####
# ******************************************************************************

## Loop through each density of aphids and report a distribution of estimates ##
## then report mean (SD) from that distribution ##


Loopy <-
  function(Threshold_Input,
           Insecticide_Scalar) {
    
    
    ## Necessary for the loop to make sense
    ## TODO change for if input is also a vector
    if (missing(Threshold_Input)) {
      Threshold <- Threshold_Default
    } else {
      Threshold <- Threshold_Input
    }
    
    
    
    Dist <- matrix(nrow = 1000, ncol = 33, data = 0) %>% as.data.frame()
    
    for (Year in seq_along(Variable_Years)) {
      for (i in seq_along(Column_NE)) {
        ## *****************************************************************
        ## S1: Define initial distribution of pests
        ## *****************************************************************
        
        
        ## Necessary for the loop to make sense
        ## TODO change for if input is also a vector
        if (missing(Insecticide_Scalar)) {
          IOCosts <- Insecticide_Only_PH[Year]
        } else {
          IOCosts <- Insecticide_Scalar
        }
        
        
        
        ## This is the central calculation
        ## For each value of aphids and NEs, we simulate the response
        Dist[, i] <- c(Column_Density[i] * (1 - (REA_New * Column_NE[i])))
        
        
        ## So turns out this is quite important!
        ## Removes zeroes for close to zeros
        ## important for functions using a log()
        Dist[, i] <- pmax(Dist[, i], 0.001)
        
        
        ## So if we have >1 Column_NE values this will truncate the distribution to
        ## be positive in all cases. i.e. an over-abundance of NEs would lead to
        ## zero pests not negative
        Dist[, i] <- pmax(Dist[, i], 0) 
        
        
        
        
        ## *****************************************************************
        ## S2_A: NEVER SPRAY
        ## *****************************************************************
        
        
        ## Effects is the truncated distribution with the mean (sd)
        ## from: https://doi.org/10.1371/journal.pone.0169475
        Effects <- rnorm(n = 100, 
                         mean = 0.132, 
                         sd = 0.099) %>%
          ifelse(. < 0, 0, .) %>%
          ifelse(. > 1, 1, .)
        
        
        ## YIELD LOSS UNDER NEVER-SPRAY CONDITIONS
        ## They always spray so don't lose anything but add insecticide
        YieldLoss_NS <- Dist[, i]  %>%
          magrittr::multiply_by(1 - Effects) %>%
          divide_by(100)
        
        
        YieldLoss_NS[is.infinite(YieldLoss_NS)] <- 0
        YieldLoss_NS[YieldLoss_NS < 0] <- 0
        
        
        ## LOSS PER HECTARE UNDER NEVER-SPRAY CONDITIONS
        LossPerHectare_NS <-
          (Variable_Yield[Year] * Variable_Price[Year]) *  YieldLoss_NS
        
        
        ## Now calculate loss per hectare and total
        LossTotal_NS <- (Variable_Area[Year] * LossPerHectare_NS)
        
        
        ## *****************************************************************
        ## S2_B: ECONOMIC THRESHOLD
        ## *****************************************************************
        
        
        ## Fine I'll vectorise for efficiency, but it won't be pretty 
        Dist[,i][Dist[,i] >= Threshold] <- 0 ## so only loss if density > threshold
        
        
        YieldLoss_ET <- Dist[,i] %>%  
          magrittr::multiply_by(1 - Effects) %>%
          divide_by(100)
        
        YieldLoss_ET[is.infinite(YieldLoss_ET)] <- 0 ## change infinites to zeroes
        YieldLoss_ET[YieldLoss_ET < 0] <- 0.0000001 ## change negatives to 0.0000001
        
        
        
        
        ## Rewriting so that dist < threshold means spray + YL even if small
        ## Dist > Threshold means no YL BUT full costs
        LossPerHectare_ET <- ifelse(
          YieldLoss_ET == 0,
          Variable_Sprays[Year],
          ## zero where dist>threshold for spraying
          YieldLoss_ET %>%
            multiply_by(Variable_Yield[Year] * Variable_Price[Year]) 
        )
        
        
        ## Multiply by total area
        LossTotal_ET <- (Variable_Area[Year] * LossPerHectare_ET)
        
        
        ## Now cover the placeholder
        YieldLoss_ET[YieldLoss_ET == 0.0000001] <- 0
        
        ## *****************************************************************
        ## S2_C: ALWAYS SPRAY
        ## *****************************************************************
        
        ## YIELD LOSS UNDER ALWAYS-SPRAY CONDITIONS
        ## They always spray so don't lose anything but add insecticide
        YieldLoss_AS <- 0
        
        
        ## Changed from "Insecticide_PH" to "Variable_Sprays[Year]"
        LossPerHectare_AS <-
          (Variable_Yield[Year] * Variable_Price[Year])  %>% ## per hectare value
          multiply_by(YieldLoss_AS) %>% ## lost yield percent times value
          add(Variable_Sprays[Year]) ## plus cost of sprays
        
        
        ## Again scale by area
        LossTotal_AS <- (Variable_Area[Year] * LossPerHectare_AS)
        
        
        ## *****************************************************************
        ## S3: WEIGHTED VALUES
        ## *****************************************************************
        
        
        ## YIELD LOSS  *****************************************************************
        YieldLoss_Total <-
          (YieldLoss_NS * Field_NS) +
          (YieldLoss_ET * Field_ET) +
          (YieldLoss_AS * Field_AS)
        YieldLoss_Total_RawData[, i] <- YieldLoss_Total
        YieldLoss_NS_RawData[, i] <- YieldLoss_NS
        YieldLoss_ET_RawData[, i] <- YieldLoss_ET
        YieldLoss_AS_RawData[, i] <- YieldLoss_AS
        
        
        ## LPH
        LossPerHectare <-
          (LossPerHectare_NS * Field_NS) +
          (LossPerHectare_ET * Field_ET) +
          (LossPerHectare_AS * Field_AS)
        LossPerHectare_Total_RawData[, i] <- LossPerHectare
        LossPerHectare_NS_RawData[, i] <- LossPerHectare_NS
        LossPerHectare_ET_RawData[, i] <- LossPerHectare_ET
        LossPerHectare_AS_RawData[, i] <- LossPerHectare_AS
        
        
        ## TOTAL LOSSES 
        LossTotal_Total <-
          (LossTotal_NS * Field_NS) +
          (LossTotal_ET * Field_ET) +
          (LossTotal_AS * Field_AS)
        LossTotal_Total_RawData[, i] <- LossTotal_Total
        
        ## *****************************************************************
        ## S4: OUTPUT WHAT WE NEED FROM THIS LOOP
        ## *****************************************************************
        
        
        ## Store yield loss means and SD
        ## Store yield loss means and SD
        Estimates[i, 1] <- YieldLoss_NS %>% mean(na.rm = TRUE)
        Estimates[i, 2] <- YieldLoss_ET %>% mean(na.rm = TRUE)
        Estimates[i, 3] <- YieldLoss_AS %>% mean(na.rm = TRUE)
        Estimates[i, 4] <- YieldLoss_Total %>% mean(na.rm = TRUE)
        Estimates[i, 5] <- YieldLoss_NS %>% sd(na.rm = TRUE)
        Estimates[i, 6] <- YieldLoss_ET %>% sd(na.rm = TRUE)
        Estimates[i, 7] <- YieldLoss_AS %>% sd(na.rm = TRUE)
        Estimates[i, 8] <- YieldLoss_Total %>% sd(na.rm = TRUE)
        
        ## Output nice format
        Output_Yield_Raw <- Estimates[, 4] %>% multiply_by(100)
        
        
        Estimates[i, 9] <-  LossPerHectare_NS %>% mean(na.rm = TRUE)
        Estimates[i, 10] <- LossPerHectare_ET %>% mean(na.rm = TRUE)
        Estimates[i, 11] <- LossPerHectare_AS %>% mean(na.rm = TRUE)
        Estimates[i, 12] <- LossPerHectare %>% mean(na.rm = TRUE)
        Estimates[i, 13] <- LossPerHectare_NS %>% sd(na.rm = TRUE)
        Estimates[i, 14] <- LossPerHectare_ET %>% sd(na.rm = TRUE)
        Estimates[i, 15] <- LossPerHectare_AS %>% sd(na.rm = TRUE)
        Estimates[i, 16] <- LossPerHectare %>% sd(na.rm = TRUE)
        
        
        Estimates[i, 17] <- LossTotal_NS %>% mean(na.rm = TRUE)
        Estimates[i, 18] <- LossTotal_ET %>% mean(na.rm = TRUE)
        Estimates[i, 19] <- LossTotal_AS %>% mean(na.rm = TRUE)
        Estimates[i, 20] <- LossTotal_Total %>% mean(na.rm = TRUE)
        Estimates[i, 21] <- LossTotal_NS %>% sd(na.rm = TRUE)
        Estimates[i, 22] <- LossTotal_ET %>% sd(na.rm = TRUE)
        Estimates[i, 23] <- LossTotal_AS %>% sd(na.rm = TRUE)
        Estimates[i, 24] <- LossTotal_Total %>% sd(na.rm = TRUE)
        
        Estimates[i, 25] <- Dist[,i] %>% mean(na.rm = TRUE)
        Estimates[i, 26] <- Dist[,i] %>% sd(na.rm = TRUE)  
        
        Output_AD_Raw_Mean <- Estimates[, 25]
        Output_AD_Raw_SD <- Estimates[, 26]
        Output_AD_Formatted <- paste0(
          Estimates[, 25] %>% round(2) %>% sprintf("%.2f", .),
          " (\u00B1 ",
          Estimates[, 26] %>% round(2) %>% sprintf("%.2f", .),
          ")"
        )
        
        
        
        
        
        
        ## OUTPUT NICELY: LPH  *****************************************************************
        Output_LPH_Raw_Mean <- Estimates[, 12]
        Output_LPH_Raw_SD <- Estimates[, 16]
        
        
        Output_LPH_Raw_NS_Mean <- Estimates[, 9]
        Output_LPH_Raw_ET_Mean <- Estimates[, 10]
        Output_LPH_Raw_AS_Mean <- Estimates[, 11]
        
        
        Output_LPH_Raw_NS_SD <- Estimates[, 13]
        Output_LPH_Raw_ET_SD <- Estimates[, 14]
        Output_LPH_Raw_AS_SD <- Estimates[, 15]
        
        
        
        ## OUTPUT NICELY: LT  *****************************************************************
        
        Output_LT_Raw <- Estimates[, 20]
        
        
      }
      
      ## Stitch together across years
      Annual_AD_Raw_Mean[, Year] <- Output_AD_Raw_Mean
      Annual_AD_Raw_SD[, Year] <- Output_AD_Raw_SD
      Annual_AD_Formatted[, Year] <- Output_AD_Formatted
      
      Annual_YL_NS_Mean[, Year] <- Estimates[, 1]
      Annual_YL_ET_Mean[, Year] <- Estimates[, 2]
      Annual_YL_AS_Mean[, Year] <- Estimates[, 3]
      
      Annual_YL_NS_SD[, Year] <- Estimates[, 5]
      Annual_YL_ET_SD[, Year] <- Estimates[, 6]
      Annual_YL_AS_SD[, Year] <- Estimates[, 7]
      
      
      Annual_LPH_Raw_Mean[, Year] <- Output_LPH_Raw_Mean
      Annual_LPH_Raw_NS_Mean[, Year] <- Output_LPH_Raw_NS_Mean
      Annual_LPH_Raw_ET_Mean[, Year] <- Output_LPH_Raw_ET_Mean
      Annual_LPH_Raw_AS_Mean[, Year] <- Output_LPH_Raw_AS_Mean
      
      Annual_LPH_Raw_NS_SD[, Year] <- Output_LPH_Raw_NS_SD
      Annual_LPH_Raw_ET_SD[, Year] <- Output_LPH_Raw_ET_SD
      Annual_LPH_Raw_AS_SD[, Year] <- Output_LPH_Raw_AS_SD
      
      Annual_LPH_Raw_SD[, Year] <- Output_LPH_Raw_SD
      Annual_LT_Raw[, Year] <- Output_LT_Raw
      
      
      ## Create a vector to loop through
      ## The values in this DF are those that the function uses
      ## Create a vector to loop through
      ## The values in this DF are those that the function uses
      Looper <- data.frame("Left" = rep(c(11, 22, 33), each = 11) ,
                           "Right" = seq.int(from = 1, to = 33, by = 1))
      
      
      LPH_Results <- lapply(seq_len(nrow(Looper)), function(Row) {
        Output_NS <- Summarise_LPH_All(LossPerHectare_NS_RawData,
                                       Looper[Row, 1],
                                       Looper[Row, 2])
        Output_ET <- Summarise_LPH_All(LossPerHectare_ET_RawData,
                                       Looper[Row, 1],
                                       Looper[Row, 2])
        Output_AS <- Summarise_LPH_All(LossPerHectare_AS_RawData,
                                       Looper[Row, 1],
                                       Looper[Row, 2])
        
        ## Return a named list
        NS <- c(
          Mean = as.numeric(Output_NS["Mean"]),
          SD = as.numeric(Output_NS["SD"])
        )
        ET <- c(
          Mean = as.numeric(Output_ET["Mean"]),
          SD = as.numeric(Output_ET["SD"])
        )
        AS <- c(
          Mean = as.numeric(Output_AS["Mean"]),
          SD = as.numeric(Output_AS["SD"])
        )
        c(NS, 
          ET,
          AS)
        
        
      })
      
      ## Now convert the list to a data frame
      Results_DF <- do.call(rbind, LPH_Results)
      
      
      Annual_LPH_Raw_ChangeMeans_NS[, Year] <-
        Results_DF[, 1] %>% as.numeric()
      Annual_LPH_Raw_ChangeSDs_NS[, Year] <-
        Results_DF[, 2] %>% as.numeric()
      
      ## We need this for plot CLPH
      Annual_LPH_Raw_ChangeMeans_ET[, Year] <-
        Results_DF[, 3] %>% as.numeric()
      Annual_LPH_Raw_ChangeSDs_ET[, Year] <-
        Results_DF[, 4] %>% as.numeric()
      
      
      ## Always spray
      Annual_LPH_Raw_ChangeMeans_AS[, Year] <-
        Results_DF[, 5]  %>% as.numeric()
      Annual_LPH_Raw_ChangeSDs_AS[, Year] <-
        Results_DF[, 6] %>% as.numeric()
      
      
      
    }
    
    
    PlotData_CLPH <- data.frame(
      "Density" = Column_Density,
      "NE" = Column_NE,
      "Crop" = "OSR",
      
      "NS_Means" = Annual_LPH_Raw_ChangeMeans_NS %>% as.matrix() %>% rowmeans(),
      "NS_SD" = Annual_LPH_Raw_ChangeSDs_NS %>% as.matrix() %>% rowmeans(),
      
      "ET_Means" = Annual_LPH_Raw_ChangeMeans_ET %>% as.matrix() %>% rowmeans(),
      "ET_SD" = Annual_LPH_Raw_ChangeSDs_ET %>% as.matrix() %>% rowmeans(),
      
      "AS_Means" = Annual_LPH_Raw_ChangeMeans_AS %>% as.matrix() %>% rowmeans(),
      "AS_SD" = Annual_LPH_Raw_ChangeSDs_AS %>% as.matrix() %>% rowmeans()) %>%
      Pivoter() %>%  
      mutate(
        Ymin = pmax(Means - SD, 0),
        Ymax = pmax(Means + SD, 0),
        Threshold = Threshold,
        Insecticide = IOCosts)
    
    
    
    PlotData_LPH <- data.frame(
      "Density" = Column_Density,
      "NE" = Column_NE,
      "Crop" = "OSR",
      
      "NS_Means" = Annual_LPH_Raw_NS_Mean %>% as.matrix() %>% rowmeans(),
      "NS_SD" = Annual_LPH_Raw_NS_SD %>% as.matrix() %>% rowmeans(),
      
      "ET_Means" = Annual_LPH_Raw_ET_Mean %>% as.matrix() %>% rowmeans(),
      "ET_SD" = Annual_LPH_Raw_ET_SD %>% as.matrix() %>% rowmeans(),
      
      "AS_Means" = Annual_LPH_Raw_AS_Mean %>% as.matrix() %>% rowmeans(),
      "AS_SD" = Annual_LPH_Raw_AS_Mean %>% as.matrix() %>% rowmeans()
    ) %>%
      Pivoter() %>% 
      mutate(
        Ymin = pmax(Means - SD, 0), ## Check if pmin is better
        Ymax = pmax(Means + SD, 0)) %>% 
      mutate(
        Ymin = ifelse(Means == SD, Means, Ymin),
        Ymax = ifelse(Means == SD, Means, Ymax),
        Threshold = Threshold,
        Insecticide = IOCosts)
    
    
    
    PlotData_YL <- data.frame(
      "Density" = Column_Density,
      "NE" = Column_NE,
      "Crop" = "OSR",
      
      "NS_Means" = Annual_YL_NS_Mean %>% as.matrix() %>% rowmeans() %>% multiply_by(100),
      "NS_SD" = Annual_YL_NS_SD %>% multiply_by(100) %>% as.matrix() %>% rowmeans(),
      
      "ET_Means" = Annual_YL_ET_Mean %>% as.matrix() %>% rowmeans() %>% multiply_by(100),
      "ET_SD" = Annual_YL_ET_SD %>% multiply_by(100) %>% as.matrix() %>% rowmeans(),
      
      "AS_Means" = Annual_YL_AS_Mean %>% as.matrix() %>% rowmeans() %>% multiply_by(100),
      "AS_SD" = Annual_YL_AS_SD %>% multiply_by(100) %>% as.matrix() %>% rowmeans()
    ) %>%
      Pivoter() %>% 
      mutate(
        Ymin = pmax(Means - SD, 0), ## Check if pmin is better
        Ymax = pmax(Means + SD, 0)) %>% 
      mutate(
        Ymin = ifelse(Ymin %>% is.na(), 0, Ymin),
        Ymax = ifelse(Ymax %>% is.na(), 0, Ymax),
        Threshold = Threshold,
        Insecticide = IOCosts)
    
    
    return(
      list(PlotData_CLPH, PlotData_LPH, PlotData_YL) %>%
        setNames(c(
          sprintf("PlotData_CLPH_Threshold%2s", 
                  paste(Threshold, IOCosts %>% round(0), sep = "_Insecticide")),
          sprintf("PlotData_LPH_Threshold%2s", 
                  paste(Threshold, IOCosts %>% round(0), sep = "_Insecticide")),
          sprintf("PlotData_YL_Threshold%2s", 
                  paste(Threshold, IOCosts %>% round(0), sep = "_Insecticide"))
        )))
    
    
    
  }


# ******************************************************************************
#### Section Outputs together ####
# ******************************************************************************


# ******************************************************************************
#### Section Outputs together ####
# ******************************************************************************


## Thresholds to iterate through
# thresholds <- seq.int(from = 0, to = 10, by = 2)
# thresholds <- c(2.5, 5, 10)
thresholds <- c(1, 2.5, 5, 7.5, 10)

# Initialize empty lists to store the outputs
plot_data_clph <- list()
plot_data_lph <- list()
plot_data_yl <- list()


# Iterate over the Threshold values
for (threshold in thresholds) {
  
  # Call the Loopy() function with the current Threshold value
  output <- Loopy(Threshold_Input = threshold)
  
  # Append the outputs to the respective lists
  plot_data_clph <- c(plot_data_clph, list(output$PlotData_CLPH_Threshold))
  plot_data_lph <- c(plot_data_lph, list(output$PlotData_LPH_Threshold))
  plot_data_yl <- c(plot_data_yl, list(output$PlotData_YL_Threshold))
}

# Combine the lists into a single data frame for each output
PlotData_CLPH <- do.call(rbind, plot_data_clph) %>% data.frame()
PlotData_LPH <- do.call(rbind, plot_data_lph) %>% data.frame()
PlotData_YL <- do.call(rbind, plot_data_yl) %>% data.frame()


## Export, fwrite much better
PlotData_CLPH %>%
  fwrite(sep = ",",
         here("Output/LoopData",
              "OSR_ThresholdSA_V2_CLPH.csv"))

PlotData_LPH %>%
  fwrite(sep = ",",
         here("Output/LoopData",
              "OSR_ThresholdSA_V2_LPH.csv"))

PlotData_YL %>%
  fwrite(sep = ",",
         here("Output/LoopData",
              "OSR_ThresholdSA_V2_YL.csv"))


# ******************************************************************************
#### Plot setup ####
# ******************************************************************************


## Specify once here for consistency
TextSize <- 14
TextFamily <- "sans"
TextType <- element_text(size = TextSize,
                         colour = "black",
                         family = TextFamily)


## Plot Yield without AS
LPH_Plot <- PlotData_LPH %>% 
  dplyr::filter(Type == "ET") %>% 
  ggplot(aes(
    x = NE %>% as.numeric(),
    group = Density %>% as.factor()
  )) +
  
  geom_line(aes(y = Means, color = Density %>% as.factor()),
            linewidth = 1) +
  
  geom_point(aes(y = Means, color = Density %>% as.factor())) +
  
  geom_ribbon(
    aes(
      y = Means,
      ymin = Ymin,
      ymax = Ymax,
      fill = Density %>% as.factor()
    ),
    outline.type = "both",
    alpha = 0.2
  ) +
  
  theme_bw() +
  
  facet_grid(
    ~ Threshold,
    scales = "free_y") +
  
  geom_hline(yintercept = 0) +
  
  scale_color_manual(name = "Pest density",
                     values = c("#C6DBEF", "#4292C6", "black")) +
  
  scale_fill_manual(name = "Pest density",
                    values = c("#C6DBEF", "#4292C6", "black")) +
  
  scale_x_continuous(
    name = "Natural enemy presence as percentage.",
    breaks = seq.int(from = 0, to = 1, by = 0.25),
    labels  = seq.int(from = 0, to = 1, by = 0.25) %>%
      sprintf("%.2f", .)
  )  +
  
  scale_y_continuous(name = "Mean lost yield\nper hectare ",
                     breaks = seq.int(from = 0, to = 300, by = 50)) +
  
  ggtitle("OSR: Losses per hectare") +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "white"),
    legend.text = TextType,
    strip.text = TextType,
    text = TextType,
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType
  )


LPH_Plot %>% 
  ggsave(
    device = "png",
    filename = paste0(here(), 
                      "/Output/Figures/",
                      "OSR_ThresholdSA_V2_LPH_Plot.png"), 
    width = 20,
    height = 15,
    units = "cm",
    dpi = 500
  )
