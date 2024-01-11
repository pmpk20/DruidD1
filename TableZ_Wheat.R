#### DRUID D1: Intro plotting of wheat data  ###############
# Function: To plot wheat data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 12/10/23
# Change:
## - Updated economic threshold calculation


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


# ******************************************************************************
#### Section 1: Data Importing ####
## Here we import all the crop data and isolate the wheat estimates
# ******************************************************************************


Data <-
  here("MarketData","AllCropData_V1.xlsx") %>% readxl::read_xlsx(sheet = "Sheet1") %>% data.frame()


Wheat <- Data[Data$Crop == "Wheat", ] 


# ******************************************************************************
#### Section 2: Cleaning and trimming data ####
## We clean the data, drop missing years and name the variables we need
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
NE <- c(0.00, 0.25, 0.50, (1 - 0.34), 0.75, 1)
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


## Nrow = 15: 5 levels of NEs * 3 scenarios for aphids
## Ncol = 4: Mean, SD, lower CI, upper CI for four variables
Estimates <- matrix(0, nrow = ColLength, ncol = 4) %>% data.frame()

Estimates_LT <- matrix(0, nrow = DistSize, ncol = ColLength) %>% data.frame()

Annual_YL <- matrix(0, nrow = ColLength, ncol = 11) %>% data.frame()

Annual_YL_Raw <- matrix(0, nrow = ColLength, ncol = 11) %>% data.frame()

Annual_LT <- matrix(0, nrow = ColLength, ncol = 11) %>% data.frame()


## Initialise values here
Threshold <- 5
Insecticide_PH <- 282

Field_NS <- 0.009
Field_ET <- 0.37
Field_AS <- 1 - Field_NS - Field_ET


# ******************************************************************************
#### Section 4A: Loop ####
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
  ## YIELD LOSS UNDER ALWAYS-SPRAY CONDITIONS
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
  

  LossPerHectare <- 
    (LossPerHectare_NS * Field_NS) + 
    (LossPerHectare_ET * Field_ET) + 
    (LossPerHectare_AS * Field_AS)
  
  LossTotal_Total <- 
    (LossTotal_NS * Field_NS) + 
    (LossTotal_ET * Field_ET) + 
    (LossTotal_AS * Field_AS)
  
  
  
  ## FILL COLUMNS  *****************************************************************
  ## Furnish each column with mean and sd of each distribution
  Estimates[i, 1] <- YieldLoss_Total %>% mean(na.rm = TRUE)
  Estimates[i, 2] <- YieldLoss_Total %>% sd(na.rm = TRUE)
  
  Estimates[i, 3] <- LossTotal_Total %>% mean(na.rm = TRUE)
  Estimates[i, 4] <- LossTotal_Total %>% sd(na.rm = TRUE)
  Estimates_LT[, i] <- LossTotal_Total
    
  
  Output_Yield <- paste0(
    Estimates[, 1] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"),
    " (",
    Estimates[, 2] %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ),
    ")"
  )
  
  Output_Yield_Raw <- Estimates[, 1]
  
    
  Output_LT <- paste0(
    Estimates[, 3] %>% Constrainer() %>% divide_by(1000000) %>% Transformer(),
    " (",
    Estimates[, 4] %>% Constrainer() %>% divide_by(1000000) %>% Transformer(),
    ")"
  )
  
  
  ## Changes in LT compared to baseline
  ChangeInTotaLoss <- rbind(
    0,
    Estimates[1 , 3] - Estimates[2 , 3],
    Estimates[1 , 3] - Estimates[3 , 3],
    Estimates[1 , 3] - Estimates[4 , 3],
    Estimates[1 , 3] - Estimates[5 , 3],
    0,
    Estimates[6 , 3] - Estimates[7 , 3],
    Estimates[6 , 3] - Estimates[8 , 3],
    Estimates[6 , 3] - Estimates[9 , 3],
    Estimates[6 , 3] - Estimates[10 , 3],
    0,
    Estimates[11 , 3] - Estimates[12 , 3],
    Estimates[11 , 3] - Estimates[13 , 3],
    Estimates[11 , 3] - Estimates[14 , 3],
    Estimates[11 , 3] - Estimates[15 , 3]
  )
  
  
}
  Annual_YL[, Year] <- Output_Yield
  Annual_YL_Raw[, Year] <- Output_Yield_Raw
  Annual_LT[, Year] <- Output_LT
}



## Compile and format outputs *************************************************

colnames(Annual_YL) <- Variable_Years
colnames(Annual_LT) <- Variable_Years


# ******************************************************************************
#### Section 5: Put the table together ####
## All columns go here
# ******************************************************************************


## Setup new design here

TableZ <- bind_cols(
  "Density" = Column_Density,
  "NE" = Column_NE,
  Annual_LT[, c("2017", "2018", "2019", "2020", "2021")]
)



TableZ %>% data.frame()

# ******************************************************************************
#### Section 6B: Exporting the table ####
# ******************************************************************************



TableZ %>% data.frame() %>% fwrite(sep = "#",
                                   here("Output/Tables", 
                                        "TableZ_OutputSummaryForWheat.txt"))


