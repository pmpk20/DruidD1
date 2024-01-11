#### DRUID D1: Intro plotting of wheat data  ###############
# Function: To plot wheat data
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Last Edited: 02/10/2023
# Changes:
## - Changed CI to plus/minus one value
## - Misc typos and formatting


# *****************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# *****************************************************************************



## sessionInfo() *************************************************************
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
## Libraries here: ************************************************************
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

## Presence/absence of NEs
## Easier to use one value but this sequence makes a column for the table
# NE <- seq.int(from = 0, to = 1, by = 0.25)

## Adding a slot for 34% reduction figure
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
    as.numeric() %>%
    round(2) %>%
    format(nsmall = 2,
           big.mark = ",",
           zero.print = TRUE) %>%
    paste0("£", .) %>%
    gsub(pattern = "£  ", replacement = "£") %>%
    gsub(pattern = "£   ", replacement = "£") %>%
    gsub(pattern = "£-", replacement = "- £") %>%
    gsub(pattern = "£ -", replacement = "- £") %>%
    gsub(pattern = "£  -", replacement = "- £") %>%
    gsub(pattern = "£   -", replacement = "- £") %>%
    sub(pattern = "£    -", replacement = "- £") %>%
    gsub(pattern = "£ ", replacement = "£")
}


## Exact same as above BUT changed for millions indicator
TransformerMillions <- function(Input) {
  Input %>%
    as.numeric() %>%
    round(2) %>%
    format(nsmall = 2,
           big.mark = ",",
           zero.print = TRUE) %>%
    paste0("£", ., "m") %>%
    gsub(pattern = "£  ", replacement = "£") %>%
    gsub(pattern = "£   ", replacement = "£") %>%
    gsub(pattern = "£-", replacement = "- £") %>%
    gsub(pattern = "£ -", replacement = "- £") %>%
    gsub(pattern = "£  -", replacement = "- £") %>%
    gsub(pattern = "£   -", replacement = "- £") %>%
    sub(pattern = "£    -", replacement = "- £") %>%
    gsub(pattern = "£ ", replacement = "£")
}


## Makes the input report zero if negative valued
Constrainer <- function(Input) {
  ifelse(Input < 0,
         0,
         Input) 
}


## Given a vector of losses per hectare, 
### return mean (SD) for change between levels
SummariseLPHDiffs <- function(Data, Col1, Col2) {
  
  Input <- (Data[Col1] - Data[Col2]) %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% round(2) %>% sprintf("£%.2f",. ), 
      " (\u00B1 ", 
      sd(.) %>% round(2) %>% sprintf("£%.2f",. ), 
      ")"))) 
  
  return(Input %>% as.character())

}


## Calculate and format the percentage change in LPH
Summarise_LPH_PCT <- function(Data, Col1, Col2) {
  
  Input <- ((Data[Col2] - Data[Col1])/Data[Col1]) %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% round(2) %>% sprintf("%.2f",. ),
      "% (", 
      sd(.) %>% round(2) %>% sprintf("%.2f",. ), 
      "%)"))) 
  
  return(Input %>% as.character())
  
}



## Calculate and format the change in total losses
Summarise_LT_Diffs <- function(Data, Col1, Col2) {
  
  Input <- (Data[Col1] - Data[Col2]) %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% divide_by(1000000) %>% TransformerMillions(),
      " (", 
      sd(.) %>% divide_by(1000000) %>% TransformerMillions(), 
      ")"))) 
  
  return(Input %>% as.character())
  
}


## Calculate and format the change in total losses
Summarise_LT_PCT <- function(Data, Col1, Col2) {
  
  Input <- ((Data[Col2] - Data[Col1])/Data[Col1]) %>% 
    summarise_all(list(~ str_c(
      mean(.) %>% round(2) %>% sprintf("%.2f",. ),
      "% (", 
      sd(.) %>% round(2) %>% sprintf("%.2f",. ), 
      "%)"))) 
  
  return(Input %>% as.character())
  
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
#### Section 6: Setup loop ####
## So here we estimate yield loss and more parameters for each permutation
# ******************************************************************************


## Initialise values here
Threshold <- 5
Insecticide_PH <- 282
Field_NS <- 0.009
Field_ET <- 0.37
Field_AS <- 1 - Field_NS - Field_ET


## Nrow = 15: 5 levels of NEs * 3 scenarios for aphids
## Ncol = 4: Mean, SD, lower CI, upper CI for four variables
Estimates <- matrix(0, 
                    nrow = Column_NE %>% length(), 
                    ncol = 26) %>% data.frame()


LossPerHectare_RawData <- matrix(0, 
                     nrow = DistSize, 
                     ncol = Column_NE %>% length()) %>% data.frame()


## Store LT raw data here
LossTotal_RawData <- matrix(0, 
                                 nrow = DistSize, 
                                 ncol = Column_NE %>% length()) %>% data.frame()


## Define colnames here for ease of reference
colnames(Estimates) <- c(
  "Dist: Mean", 
  "Dist: SD", 
  
  "Yield loss (NS): Mean", 
  "Yield loss (NS): SD", 
  
  "LPH (NS): Mean", 
  "LPH (NS): SD", 
  
  "LT (NS): Mean", 
  "LT (NS): SD", 
  
  "Yield loss (ET): Mean", 
  "Yield loss (ET): SD", 
  
  "LPH (ET): Mean", 
  "LPH (ET): SD", 
  
  "LT (ET): Mean", 
  "LT (ET): SD", 
  
  "Yield loss (AS): Mean", 
  "Yield loss (AS): SD", 
  
  "LPH (AS): Mean", 
  "LPH (AS): SD", 
  
  "LT (AS): Mean", 
  "LT (AS): SD", 
  
  "YL (All): Mean", 
  "YL (All): SD", 
  
  "LPH (All): Mean", 
  "LPH (All): SD", 
  
  "LT (All): Mean", 
  "LT (All): SD"
  
  
)



# ******************************************************************************
#### Section 7: Run loop ####
## We clean the data, drop missing years and name the variables we need
# ******************************************************************************



## Loop through each density of aphids and report a distribution of estimates ##
## then report mean (SD) from that distribution ##
for (i in 1:length(Column_NE)) {
  
  
  # **************************************************************************
  ## This is the central calculation
  ## For each value of aphids and NEs, we simulate the response
  ## Dist = pest density
  Dist <- c(Column_Density[i] * (1 - (REA_New * Column_NE[i])))
  Dist %<>% ifelse(. == 0, 
                   0.001, 
                   .) ## TESTING: whether true zero causes issues
  
  # **************************************************************************
  ## NO-SPRAY CONDITIONS
  ## We now calculate the yield loss for each value
  YieldLoss_NS <- (4.5 * log(Dist) - 5.5) %>% divide_by(100)
  
  ## If there's zero loss bc many NEs then code zero
  YieldLoss_Adjusted_NS <-
    ifelse(is.infinite(YieldLoss_NS), 0, YieldLoss_NS)
  
  YieldLoss_Truncated_NS <-
    ifelse(YieldLoss_Adjusted_NS < 0, 0, YieldLoss_Adjusted_NS)
  
  
  ## Now calculate loss per hectare and total
  ## for every possibility
  LossPerHectare_NS <- (Column_Yield * Column_Price) *  YieldLoss_Truncated_NS
  LossTotal_NS <- (Column_Area * LossPerHectare_NS)
  # LossPerHectare_RawData[, i] <- LossPerHectare_NS
  
  
  # **************************************************************************
  ## YIELD LOSS UNDER ALWAYS-SPRAY CONDITIONS
  ## They always spray so don't lose anything but add insecticide 
  YieldLoss_Truncated_AS <- 0 
  
  ## Now calculate loss per hectare and total
  ## for every possibility
  LossPerHectare_AS <- (Column_Yield * Column_Price)  %>% ## per hectare value
    multiply_by(YieldLoss_Truncated_AS) %>% ## lost yield percent times value
    add(Insecticide_PH) ## plus cost of sprays
  
  
  LossTotal_AS <- (Column_Area * LossPerHectare_AS)
  
  
  
  # **************************************************************************
  ## YIELD LOSS UNDER ECONOMIC THRESHOLD CONDITIONS
  YieldLoss_ET <- ifelse(Dist >= Threshold, 0, Dist) %>%  ## so only loss if density > threshold 
    log() %>% ## take logs
    multiply_by(4.5) %>% ## 4.5ln(AphidDensity)
    subtract(5.5) %>%  ## - 5.5
    divide_by(100) %>% ## get as percentage change
    ifelse(is.infinite(.), 0, .) %>% ## change infinites to zeroes
    ifelse(. < 0, 0.0000001, .) ## change negatives to 0.1 for identification later
  
  
  ## Now calculate loss per hectare and total
  ## for every possibility
  LossPerHectare_ET <- ifelse(
      YieldLoss_ET == 0,
      Insecticide_PH, ## zero where dist>threshold for spraying
      ifelse(
        YieldLoss_ET == 0.0000001,##placeholder value for low dist
        0,
        YieldLoss_ET %>% multiply_by(Column_Yield * Column_Price)
      )
    )
    
  
  LossTotal_ET <- (Column_Area * LossPerHectare_ET)
  ## Now cover the placeholder
  YieldLoss_ET <- YieldLoss_ET %>% ifelse(. == 0.0000001, 0, .)
  
  # **************************************************************************
  ## WEIGHTED VALUES
  YieldLoss_Total <- (YieldLoss_Truncated_NS * Field_NS) + 
    (YieldLoss_ET * Field_ET) + 
    (YieldLoss_Truncated_AS * Field_AS)
  
  ## weighted loss per hectare
  LPH_Total <- (LossPerHectare_NS * Field_NS) + 
    (LossPerHectare_ET * Field_ET) + 
    (LossPerHectare_AS * Field_AS)
  
  LossTotal_Total <- (LossTotal_NS * Field_NS) + 
    (LossTotal_ET * Field_ET) + 
    (LossTotal_AS * Field_AS)
  
  LossPerHectare_RawData[, i] <- LPH_Total
  LossTotal_RawData[, i] <- LossTotal_Total
  
  
  
  # **************************************************************************
  
  
  ## Furnish each column with mean and sd of each distribution
  Estimates[i, 1] <- Dist %>% mean(na.rm = TRUE)
  Estimates[i, 2] <- (qnorm(p = 0.975) * (Dist %>% sd()) / sqrt(SampleSize))
  
  
  Estimates[i, 3] <- YieldLoss_Truncated_NS %>% mean(na.rm = TRUE)
  Estimates[i, 4] <- (qnorm(p = 0.975) * (YieldLoss_Truncated_NS %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 5] <- LossPerHectare_NS %>% mean(na.rm = TRUE)
  Estimates[i, 6] <- (qnorm(p = 0.975) * (LossPerHectare_NS %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 7] <- LossTotal_NS %>% mean(na.rm = TRUE)
  Estimates[i, 8] <- (qnorm(p = 0.975) * (LossTotal_NS %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 9] <- YieldLoss_ET %>% mean(na.rm = TRUE)
  Estimates[i, 10] <- (qnorm(p = 0.975) * (YieldLoss_ET %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 11] <- LossPerHectare_ET %>% mean(na.rm = TRUE)
  Estimates[i, 12] <- (qnorm(p = 0.975) * (LossPerHectare_ET %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 13] <- LossTotal_ET %>% mean(na.rm = TRUE)
  Estimates[i, 14] <- (qnorm(p = 0.975) * (LossTotal_ET %>% sd()) / sqrt(SampleSize))
  
  ## Manually changed the SD rows to be zero since there's one value and no variation
  Estimates[i, 15] <- YieldLoss_Truncated_AS %>% mean(na.rm = TRUE)
  Estimates[i, 16] <- 0
  
  Estimates[i, 17] <- LossPerHectare_AS %>% mean(na.rm = TRUE)
  Estimates[i, 18] <- (qnorm(p = 0.975) * (LossPerHectare_AS %>% sd()) / sqrt(SampleSize)) %>% is.na %>% ifelse(0, 1)
  
  Estimates[i, 19] <- LossTotal_AS %>% mean(na.rm = TRUE)
  Estimates[i, 20] <- (qnorm(p = 0.975) * (LossPerHectare_AS %>% sd()) / sqrt(SampleSize)) %>% is.na %>% ifelse(0, 1)
  
  Estimates[i, 21] <- YieldLoss_Total %>% mean(na.rm = TRUE)
  Estimates[i, 22] <- (qnorm(p = 0.975) * (YieldLoss_Total %>% sd()) / sqrt(SampleSize)) 
  
  Estimates[i, 23] <- LPH_Total %>% mean(na.rm = TRUE)
  Estimates[i, 24] <- (qnorm(p = 0.975) * (LPH_Total %>% sd()) / sqrt(SampleSize))
  
  Estimates[i, 25] <- LossTotal_Total %>% mean(na.rm = TRUE)
  Estimates[i, 26] <- (qnorm(p = 0.975) * (LossTotal_Total %>% sd()) / sqrt(SampleSize))
  
  
}

# ******************************************************************************
#### Section 8A: Change in LPH ####
# ******************************************************************************


# Compared to no NEs present
ChangeInLPH_0 <- rbind(
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 1),
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 2),
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 3),
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 4),
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 5),
  SummariseLPHDiffs(LossPerHectare_RawData, 1, 6),

  SummariseLPHDiffs(LossPerHectare_RawData, 7, 7),
  SummariseLPHDiffs(LossPerHectare_RawData, 7, 8),
  SummariseLPHDiffs(LossPerHectare_RawData, 7, 9),
  SummariseLPHDiffs(LossPerHectare_RawData, 7, 10),
  SummariseLPHDiffs(LossPerHectare_RawData, 7, 11),
  SummariseLPHDiffs(LossPerHectare_RawData, 7, 12),

  SummariseLPHDiffs(LossPerHectare_RawData, 13, 13),
  SummariseLPHDiffs(LossPerHectare_RawData, 13, 14),
  SummariseLPHDiffs(LossPerHectare_RawData, 13, 15),
  SummariseLPHDiffs(LossPerHectare_RawData, 13, 16),
  SummariseLPHDiffs(LossPerHectare_RawData, 13, 17),
  SummariseLPHDiffs(LossPerHectare_RawData, 13, 18)
)



## Compared to all NEs present
ChangeInLPH_1 <- rbind(
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 1),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 2),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 3),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 4),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 5),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 6),
  
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 7),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 8),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 9),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 10),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 11),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 12),
  
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 13),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 14),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 15),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 16),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 17),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 18)
)



## As above but in percentage change terms
ChangeInLPH_PCT_1 <- rbind(
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 1),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 2),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 3),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 4),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 5),
  Summarise_LPH_PCT(LossPerHectare_RawData, 6, 6),
  
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 7),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 8),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 9),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 10),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 11),
  Summarise_LPH_PCT(LossPerHectare_RawData, 12, 12),
  
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 13),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 14),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 15),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 16),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 17),
  Summarise_LPH_PCT(LossPerHectare_RawData, 18, 18)
)

# ******************************************************************************
#### Section 8B: Change in Loss Total ####
# ******************************************************************************


## Compared to no NEs present
ChangeInLossTotal_0 <- rbind(
  Summarise_LT_Diffs(LossTotal_RawData, 1, 1),
  Summarise_LT_Diffs(LossTotal_RawData, 1, 2),
  Summarise_LT_Diffs(LossTotal_RawData, 1, 3),
  Summarise_LT_Diffs(LossTotal_RawData, 1, 4),
  Summarise_LT_Diffs(LossTotal_RawData, 1, 5),
  Summarise_LT_Diffs(LossTotal_RawData, 1, 6),
  
  Summarise_LT_Diffs(LossTotal_RawData, 7, 7),
  Summarise_LT_Diffs(LossTotal_RawData, 7, 8),
  Summarise_LT_Diffs(LossTotal_RawData, 7, 9),
  Summarise_LT_Diffs(LossTotal_RawData, 7, 10),
  Summarise_LT_Diffs(LossTotal_RawData, 7, 11),
  Summarise_LT_Diffs(LossTotal_RawData, 7, 12),
  
  Summarise_LT_Diffs(LossTotal_RawData, 13, 13),
  Summarise_LT_Diffs(LossTotal_RawData, 13, 14),
  Summarise_LT_Diffs(LossTotal_RawData, 13, 15),
  Summarise_LT_Diffs(LossTotal_RawData, 13, 16),
  Summarise_LT_Diffs(LossTotal_RawData, 13, 17),
  Summarise_LT_Diffs(LossTotal_RawData, 13, 18)
)



## Compared to all NEs present
ChangeInLossTotal_1 <- rbind(
  Summarise_LT_Diffs(LossTotal_RawData, 6, 1),
  Summarise_LT_Diffs(LossTotal_RawData, 6, 2),
  Summarise_LT_Diffs(LossTotal_RawData, 6, 3),
  Summarise_LT_Diffs(LossTotal_RawData, 6, 4),
  Summarise_LT_Diffs(LossTotal_RawData, 6, 5),
  Summarise_LT_Diffs(LossTotal_RawData, 6, 6),
  
  Summarise_LT_Diffs(LossTotal_RawData, 12, 7),
  Summarise_LT_Diffs(LossTotal_RawData, 12, 8),
  Summarise_LT_Diffs(LossTotal_RawData, 12, 9),
  Summarise_LT_Diffs(LossTotal_RawData, 12, 10),
  Summarise_LT_Diffs(LossTotal_RawData, 12, 11),
  Summarise_LT_Diffs(LossTotal_RawData, 12, 12),
  
  Summarise_LT_Diffs(LossTotal_RawData, 18, 13),
  Summarise_LT_Diffs(LossTotal_RawData, 18, 14),
  Summarise_LT_Diffs(LossTotal_RawData, 18, 15),
  Summarise_LT_Diffs(LossTotal_RawData, 18, 16),
  Summarise_LT_Diffs(LossTotal_RawData, 18, 17),
  Summarise_LT_Diffs(LossTotal_RawData, 18, 18)
)



## As above but in percentage change terms
ChangeInLossTotal_PCT_1 <- rbind(
  Summarise_LT_PCT(LossTotal_RawData, 6, 1),
  Summarise_LT_PCT(LossTotal_RawData, 6, 2),
  Summarise_LT_PCT(LossTotal_RawData, 6, 3),
  Summarise_LT_PCT(LossTotal_RawData, 6, 4),
  Summarise_LT_PCT(LossTotal_RawData, 6, 5),
  Summarise_LT_PCT(LossTotal_RawData, 6, 6),
  
  Summarise_LT_PCT(LossTotal_RawData, 12, 7),
  Summarise_LT_PCT(LossTotal_RawData, 12, 8),
  Summarise_LT_PCT(LossTotal_RawData, 12, 9),
  Summarise_LT_PCT(LossTotal_RawData, 12, 10),
  Summarise_LT_PCT(LossTotal_RawData, 12, 11),
  Summarise_LT_PCT(LossTotal_RawData, 12, 12),
  
  Summarise_LT_PCT(LossTotal_RawData, 18, 13),
  Summarise_LT_PCT(LossTotal_RawData, 18, 14),
  Summarise_LT_PCT(LossTotal_RawData, 18, 15),
  Summarise_LT_PCT(LossTotal_RawData, 18, 16),
  Summarise_LT_PCT(LossTotal_RawData, 18, 17),
  Summarise_LT_PCT(LossTotal_RawData, 18, 18)
)

# ******************************************************************************
#### Section 9: Creating columns ####
# ******************************************************************************


## Compile and format outputs *************************************************


## Output nicely here
Column_AD <-
  paste0(Estimates$`Dist: Mean` %>% round(2), 
         " (\u00B1 ", 
         Estimates$`Dist: SD` %>% round(2),
         ")")


## Yield loss column
Column_YL_NS <-
  paste0(
    Estimates$`Yield loss (NS): Mean` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    " (\u00B1 ", 
    Estimates$`Yield loss (NS): SD` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    ")")


Column_YL_ET <-
  paste0(
    Estimates$`Yield loss (ET): Mean` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    " (\u00B1 ", 
    Estimates$`Yield loss (ET): SD` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    ")")



Column_YL_AS <-
  paste0(
    Estimates$`Yield loss (AS): Mean` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    " (\u00B1 ", 
    Estimates$`Yield loss (AS): SD` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    ")")


## Weighted expected yield loss
Column_YL_Total <-
  paste0(
    Estimates$`YL (All): Mean` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    " (\u00B1 ", 
    Estimates$`YL (All): SD` %>% Constrainer() %>% multiply_by(100) %>% round(2) %>% sprintf("%.2f",. ) %>% paste0(., "%"), 
    ")")



# *****************************************************************************
## Loss per hectare
Column_LPH_NS <- paste0(
  Estimates$`LPH (NS): Mean`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LPH (NS): SD`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  ")")


Column_LPH_ET <- paste0(
  Estimates$`LPH (ET): Mean`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LPH (ET): SD`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  ")")


Column_LPH_AS <- paste0(
  Estimates$`LPH (AS): Mean`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LPH (AS): SD`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  ")")


## All together
Column_LPH_Total <- paste0(
  Estimates$`LPH (All): Mean`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LPH (All): SD`  %>% Constrainer() %>% sprintf("%.2f",. ) %>% Transformer(), 
  ")")


# *****************************************************************************
## Loss per hectare

## Total loss
Column_LT_NS <- paste0(
  Estimates$`LT (NS): Mean` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LT (NS): SD` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  ")")


## economic threshold
Column_LT_ET <- paste0(
  Estimates$`LT (ET): Mean` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LT (ET): SD` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  ")")


## Always spray
Column_LT_AS <- paste0(
  Estimates$`LT (AS): Mean` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LT (AS): SD` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  ")")

## All together:
Column_LT_Total <- paste0(
  Estimates$`LT (All): Mean` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  " (\u00B1 ", 
  Estimates$`LT (All): SD` %>% Constrainer() %>% divide_by(1000000) %>% Transformer(), 
  ")")



# ******************************************************************************
#### Section 10: Put the table together ####
## All columns go here
# ******************************************************************************


## Setup new design here
TableX <- bind_cols(
  "Density" = Column_Density,
  "NE" = Column_NE,
  "Aphids" = Column_AD,
  
  "YL (NS)" = Column_YL_NS,
  "YL (ET)" = Column_YL_ET,
  "YL (AS)" = Column_YL_AS,
  "YL (Weighted)" = Column_YL_Total,
  
  "LPH (NS)" = Column_LPH_NS,
  "LPH (ET)" = Column_LPH_ET,
  "LPH (AS)" = Column_LPH_AS,
  "LPH (Weighted)" = Column_LPH_Total,
  "Change in LPH vs 1" = ChangeInLPH_1,
  "PCT Change in LPH vs 1" = ChangeInLPH_PCT_1,
  
  
  "LT (NS)" = Column_LT_NS,
  "LT (ET)" = Column_LT_ET,
  "LT (AS)" = Column_LT_AS,
  "LT (Weighted)" = Column_LT_Total,
  "Change in LT vs 1" = ChangeInLossTotal_1,
  "PCT Change in LT vs 1" = ChangeInLossTotal_PCT_1
)



TableX %>% data.frame() %>% View()

# ******************************************************************************
#### Section 11: Exporting the table ####
# ******************************************************************************


## Output all data by AS/NS/ET
TableX %>% data.frame() %>% fwrite(sep = "#",
                                   here("Output/Tables", 
                                        "TableX_OutputSummaryForWheat.txt"))


